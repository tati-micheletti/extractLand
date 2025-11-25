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
  reqdPkgs = list("SpaDES.core (>= 2.1.8.9001)", "ggplot2", "terra", "data.table", "distanceto"),
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
    expectsInput(objectName = "landscapeYearly", objectClass = 'spatRaster',
                 desc = 'list of spatRaster stacks of the yearly landscape layers'),
    expectsInput(objectName = "landscape5Yearly", objectClass = 'spatRaster',
                 desc = 'list of spatRaster stacks of the 5 yearly landscape layers'),
    expectsInput(objectName = "tracks", objectClass = 'data.table',
                 desc = 'tracks of used and random steps to extract environmental covariates for')
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = 'extractLand', objectClass = "data.table",
                  desc = "Landscape values and distance calculations matched by year to points")
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

  # valid years only
  tracks <- sim$tracks
  years <- sort(unique(tracks$year))
  validYears <- intersect(as.character(years), Par$histLandYears)

  extracted_list <- lapply(sort(intersect(as.character(years), validYears)), function(yr){
    browser()
    message("Extracting for year ", yr)

    pts_yr <- tracks[year == as.integer(yr), ]

    # Annual raster layers
    annual_rasts <- sim$landscapeYearly[[paste0("year", yr)]]

    # 5-Year vector layers
    fiveYearInt <- paste0("intYear", pts_yr$int.year[1])
    fiveYearObj <- sim$landscape5Yearly[[fiveYearInt]]

    crs_year <- terra::crs(annual_rasts[[1]])

    # Annual raster value extraction
    # Extract Start raster values
    vals_start <- terra::extract(
      annual_rasts,
      terra::vect(pts_yr[, .(x1_, y1_)], geom = c("x1_", "y1_"),
                  crs = terra::crs(annual_rasts[[1]]))
    )[, -1, drop = FALSE]

    setnames(vals_start, paste0(names(vals_start), "_start"))

    # Extract End raster values
    vals_end <- terra::extract(
      annual_rasts,
      terra::vect(pts_yr[, .(x2_, y2_)], geom = c("x2_", "y2_"),
                  crs = terra::crs(annual_rasts[[1]]))
    )[, -1, drop = FALSE]

    setnames(vals_end, paste0(names(vals_end), "_end"))

    # Convert 5-year SpatVectors to sf

    # PAVED (SpatVectorCollection)
    # May need to add a second paved conversion for the other layer
    paved_sf <- if (!is.null(fiveYearObj$paved))
      st_as_sf(fiveYearObj$paved[[2]])
    else NULL
    paved_sf <- st_cast(paved_sf, "MULTILINESTRING")

    # UNPAVED (SpatVector)
    unpaved_sf <- if (!is.null(fiveYearObj$unpaved))
      st_as_sf(fiveYearObj$unpaved)
    else NULL
    unpaved_sf <- st_cast(unpaved_sf, "MULTILINESTRING")

    # POLYS (SpatVector)
    polys_sf <- if (!is.null(fiveYearObj$polys))
      st_as_sf(fiveYearObj$polys)
    else NULL
    polys_sf <- st_cast(polys_sf, "MULTIPOLYGON")


    # Calculate the distance to the vector layers

    # Unpaved distance calculation
    if (!is.null(unpaved_sf)) {
      pts_yr <- extract_distto(
        DT = pts_yr,
        feature = unpaved_sf,
        name = "unpaved",
        where = "both",
        crs = crs_year,
        int.yr = pts_yr$int.year[1]
      )
    } else {
      pts_yr[, dist_unpaved_end := NA_real_]
    }

    # Paved distance calculation
    if (!is.null(paved_sf)) {
      pts_yr <- extract_distto(
        DT = pts_yr,
        feature = paved_sf,
        name = "paved",
        where = "both",
        crs = crs_year,
        int.yr = pts_yr$int.year[1]
      )
    } else {
      pts_yr[, dist_paved_end := NA_real_]
    }

    # Polygon distance calculation
    if (!is.null(polys_sf)) {
      pts_yr <- extract_distto(
        DT = pts_yr,
        feature = polys_sf,
        name = "polys",
        where = "both",
        crs = crs_year,
        int.yr = pts_yr$int.year[1]
      )
    } else {
      pts_yr[, dist_polys_end := NA_real_]
    }

    # Combine the annual extractions and 5 year distance calculations
    dt <- data.table(
      cbind(
        as.data.frame(pts_yr),
        vals_start,
        vals_end
      )
    )
    dt$year <- as.integer(yr)

    return(dt)
  })

  # Combine all years
  sim$extractLand <- rbindlist(extracted_list, fill = TRUE)
  message("Extraction complete: ", nrow(sim$extractLand), " records.")

  return(invisible(sim))

}
### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")


  return(invisible(sim))
}
