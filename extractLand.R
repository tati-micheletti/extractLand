defineModule(sim, list(
  name = "extractLand",
  description = "Point extraction of landscape values",
  keywords = "",
  authors = c(person("Julie", "Tuner", email = "", role = c("aut", "cre")),
              person("Rory", "McInnes", email = "", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(extractLand = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "extractLand.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.8.9001)", "ggplot2", "terra", "data.table"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("caribouYears", "integer", NULL, NA, NA,
                    paste0("This is the year range of data we want to run models for.")),
    defineParameter("histLandYears", "integer", 2010:2023, NA, NA,
                    paste0("This is the year range we use past (not simulated) landscape layers.")),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "landscapeYearly", objectClass = 'list',
                 desc = 'list of spatRaster stacks of the yearly landscape layers'),
    expectsInput(objectName = "landscape5Yearly", objectClass = 'list',
                 desc = 'list of spatRaster stacks of the 5 yearly landscape layers'),
    expectsInput(objectName = "tracks", objectClass = 'data.table',
                 desc = 'tracks of used and random steps to extract environmental covariates for')
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = 'extractLand', objectClass = "data.table", desc = "Landscape values matched by year to points")
  )
))


doEvent.extractLand <- function(sim, eventTime, eventType, priority) {
  switch(
    eventType,
    init = {
      # run data harmonization
      sim <- Init(sim)

    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  message("Starting extraction...")

  if(!is.null(Par$caribouYears)){
    tracks <- sim$tracks[year >= min(Par$caribouYears) & year <= max(Par$caribouYears)]
  }
  tracks <- sim$tracks
  years <- sort(unique(sim$tracks$year))
  landYears <- c("2019","2020","2021") #update this to a param (histLandYears)

  # Get available years from all dynamic layers
  validYears <- intersect(as.character(years), landYears)

  # Main extraction
  extracted_list <- lapply(validYears, function(yr){
    message("Extracting for year ", yr)

    pts_yr <- tracks[tracks$year == as.integer(yr), ]

    # Annual data
    annual_rasts <- sim$landscapeYearly[[paste0("year", yr)]]

    # 5 Year data
    fiveYear <- paste0("intYear", yr)
    if (!is.null(sim$landscape5Yearly[[fiveYear]])) {
      land5Obj <- sim$landscape5Yearly[[fiveYear]]
    } else {
      message("No 5-year data found for ", fiveYear)
      land5Obj <- NULL
    }

    # combine all the rasters
    landscapeYr <- c(annual_rasts, fiveYear) #might be an issue

    # extract the values
    coords <- pts_yr[, .(x1_, y1_)]
    vals <- terra::extract(landscapeYr, coords)
    vals <- vals[, -1, drop = FALSE]  # drop cell ID column

    # merge
    dt <- data.table(cbind(as.data.frame(pts_yr), vals))
    dt$year <- as.integer(yr)

    return(dt)
  })

  # Combine and store
  sim$extractLand <- rbindlist(extracted_list, fill = TRUE)
  #message("Extraction complete: ", nrow(sim$extractedPoints), " records.")

  return(invisible(sim))

}
### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")


  return(invisible(sim))
}
