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
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
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
  #message("Starting extraction...")
  
  tracks <- sim$caribouLoc
  years <- sort(unique(tracks$year))
  
  # Get available years from all dynamic layers
  fireYears      <- names(sim$landscapeYearly$histFire)
  harvestYears   <- names(sim$landscapeYearly$timeSinceHarvest)
  landcoverYears <- names(sim$landscapeYearly$histLand)
  availableYears <- Reduce(intersect, list(fireYears, harvestYears, landcoverYears))
  validYears <- intersect(as.character(years), availableYears)
  
  # Main extraction
  extracted_list <- Map(function(yr) {
    message("Extracting for year: ", yr)
    
    pts_yr <- tracks[tracks$year == as.integer(yr), ]
    fire_rast <- sim$landscape$fire[[yr]]
    harvest_rast <- sim$landscape$harvest[[yr]]
    landcover_rast <- sim$landscape$landcover[[yr]]
    
    landscapeYr <- c(fire_rast, harvest_rast, landcover_rast)
    names(landscapeYr) <- c("timeSinceFire", "timeSinceHarvest", "landcover")
    
    vals <- terra::extract(landscapeYr, pts_yr)
    vals <- vals[, -1, drop = FALSE]
    
    data.table(cbind(as.data.frame(pts_yr), vals))
  }, yr = validYears)
  
  # Combine and store
  sim$extractLand <- rbindlist(extracted_list, fill = TRUE)
  #message("Extraction complete: ", nrow(sim$extractedPoints), " records.")
  
  # Schedule save event
  sim <- scheduleEvent(sim, time(sim), "myModuleName", "save")
  
  return(invisible(sim))
  
}
### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  
  return(invisible(sim))
}