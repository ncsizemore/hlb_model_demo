## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "TruckDistribution",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.1", TruckDistribution = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "day",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "TruckDistribution.Rmd")),
  reqdPkgs = list("lubridate", "trapezoid", "purrr", "sf", "terra"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("trucks_per_10_acres", "numeric", 10, NA, NA,
                    "The number of trucks required to harvest 10 acres of citrus in one season."),
    defineParameter("truck_interval", "numeric", 1, NA, NA,
                    "Time interval in days for truck operations during harvesting seasons"),
    defineParameter("road_dispersal_density", "numeric", 1/10000, NA, NA,
                    "Density of sampling for psyllid dispersal on truck routes"),
    defineParameter("psyllids_per_truck_percentage", "numeric", 0.05, NA, NA,
                    "Percentage of psyllids in harvesting location removed by a truck"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = dplyr::bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "citr_polys_cells", objectClass = "list", desc = "list of smaller grid squares contained in each of the aggregated harvesting locations", sourceURL = NA),
    expectsInput(objectName = "corridor_paths", objectClass = "list", desc = "list of smaller grid squares contained in each of the aggregated harvesting locations", sourceURL = NA),
    expectsInput(objectName = "harvest_season", objectClass = "list", desc = "list of smaller grid squares contained in each of the aggregated harvesting locations", sourceURL = NA)
  ),
  outputObjects = dplyr::bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "harvest_season", objectClass = NA, desc = NA),
    createsOutput(objectName = "truck_schedule", objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.TruckDistribution = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      #as.numeric(sim$harvest_season[1] - mdy(G(sim)$.start_date)) - 1
      # schedule future event(s)
      sim <- scheduleEvent(sim, as.numeric(sim$harvest_season[1] - mdy(G(sim)$start_date)) - 1 , "TruckDistribution", "generate_truck_schedule")
    },
    generate_truck_schedule = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      sim <- GenerateTruckSchedule(sim)

      sim <- scheduleEvent(sim, time(sim) + as.numeric(sim$harvest_season[1] - (sim$harvest_season[1] - years(1))), "TruckDistribution", "generate_truck_schedule")
      sim <- scheduleEvent(sim, time(sim) + 1, "TruckDistribution", "disperse_from_trucks")
      # ! ----- STOP EDITING ----- ! #
    },
    disperse_from_trucks = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      sim <- DisperseFromTrucks(sim)

      # e.g.,
      if (sim$date + days(P(sim)$truck_interval) >= sim$harvest_season[1] & sim$date + days(P(sim)$truck_interval) <= sim$harvest_season[4]){
        sim <- scheduleEvent(sim, time(sim) + P(sim)$truck_interval, "TruckDistribution", "disperse_from_trucks")
      }
      # ! ----- STOP EDITING ----- ! #
    }, update_harvest_season = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

            # e.g.,
      sim$harvest_season <- sim$harvest_season + years(1)
      
      sim <- scheduleEvent(sim, time(sim) + as.numeric(sim$date + years(1) - sim$date), "TruckDistribution", "update_harvest_season")
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  
  # if the sim is started in the middle of harvesting season, we don't do any harvesting until the next year
  sim$harvest_season <- mdy(paste(G(sim)$harvest_dates, year(mdy(G(sim)$start_date))))
  sim$harvest_season[3:4] <- sim$harvest_season[3:4] + years(1)
  if (sim$date >= sim$harvest_season[1]){
    sim$harvest_season <- sim$harvest_season + years(1)
  } 
  
  sim <- scheduleEvent(sim, time(sim) + as.numeric(sim$harvest_season[4] - sim$date + 1), "TruckDistribution", "update_harvest_season")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


### template for your event1
GenerateTruckSchedule <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  
  sim$truck_schedule <- .generate_truck_schedule(sim$removed_citrus, sim$harvest_season, sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
DisperseFromTrucks <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

 tmp <- .disperse_from_trucks(sim$adults, sim$truck_schedule, sim$corridor_paths, sim$harvest_season, sim$citr_polys_cells, sim$date, sim)
 sim$adults$assign(tmp[[1]])

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  
  # ! ----- EDIT BELOW ----- ! #
  
  # sim$citr_polys_cells <- readRDS(paste0(simPaths$inputPath, "citrus_polys_cells.rds"))
  # sim$land <- readRDS(paste0(simPaths$inputPath, "land.rds"))
  # sim$corridor_paths <- readRDS(paste0(simPaths$inputPath, "cpath1-560.rds"))
  
  sim$maps$routes$citrus_harvesting_locations_to_processing <- readRDS(paste0(simPaths$inputPath, "all_routes_citrus_origins_to_processing.rds"))
  
    
  sim$maps$citrus_harvesting_locations <- terra::aggregate(sim$maps$citrus, fact=8, fun="sum")

  
  # assuming a threshold of 10 small squares:
  citrus_origins <- xyFromCell(raster(sim$maps$citrus_harvesting_locations), which(sim$maps$citrus_harvesting_locations[] > 10), spatial = TRUE)
  sim$maps$citrus_origins <- st_transform(st_as_sf(citrus_origins), crs(sim$maps$citrus))
  
  # cells on lower-res, aggregated grid
  sim$maps$citrus_harvesting_locations_cells <- tabularaster::cellnumbers(sim$maps$citrus_harvesting_locations, sim$maps$citrus_origins)$cell_
  
  rows_cols <- rowColFromCell(raster(sim$maps$citrus_harvesting_locations), sim$maps$citrus_harvesting_locations_cells)
  # Use tf.pad to build indices. note that expandgrid etc was way off the mark
  # we just needed the coordinate pairs with each combination of 0/1
  #tf$constant(matrix(c(c(0L, 1L), c(0L, 1L)), ncol = 2))
  # first pad with 0 before and after each row
  pad1 <- tf$pad(tf$constant(as.matrix(rows_cols), dtype=tf$float32), tf$constant(matrix(c(c(0L, 1L), c(0L, 1L)), ncol = 2)))
  # then pad each row before with 1, then after with 0
  pad2 <- tf$pad(tf$pad(tf$constant(as.matrix(rows_cols), dtype=tf$float32), tf$constant(matrix(c(c(0L, 1L), c(0L, 0L)), ncol = 2)), constant_values = 1L), tf$constant(matrix(c(c(0L, 0L), c(0L, 1L)), ncol = 2)))
  # then concat to get list of indices
  # NB: sim$maps$citrus_harvesting_locations_tensor_indices contains all 4-indices [0/1,x,y,0] where (x,y)
  # is a harvesting location in the pooled grid
  sim$maps$citrus_harvesting_locations_tensor_indices <- tf$cast(tf$concat(c(pad1, pad2), axis=0L), tf$int32)
  # test to verify we got it right:
  # tst_up <- tf$tensor_scatter_nd_update(tst, indpad, tf$constant(100000L, dtype=tst$dtype, shape=shape(indpad$shape[[1]])))

  
  sim$maps$citrus_processing <- readRDS(paste0(simPaths$inputPath, "citrus_processing.rds"))
  # this was generated by:
  # citrus_processing <- tibble::tribble( ~name,~addr,
  #                                       "Citrosuco North America, Inc.", "5937 Highway 60 East, Lake Wales, FL 33898", 
  #                                       "Florida's Natural", "20205 US-27, Lake Wales, FL 33853", 
  #                                       "Cutrale Citrus Juices USA", "602 McKean St, Auburndale, FL 33823", 
  #                                       "Cutrale Citrus Juices USA", "1451 W Derby Ave, Auburndale, FL 33823", 
  #                                       "Cutrale Citrus Juices USA", "11 Cloud St, Leesburg, FL 34748", 
  #                                       "Peace River Citrus Prods Inc", "4104 NW Highway 72, Arcadia, FL 34266", 
  #                                       "Peace River Citrus Prods Inc", "2020 US-17, Bartow, FL 33830", 
  #                                       "Tropicana", "9th St E, Bradenton, FL 34208", 
  #                                       "Tropicana", "6500 Glades Cut Off Rd, Fort Pierce, FL 34981", 
  #                                       "Southern Gardens Citrus", "1820 Co Rd 833, Clewiston, FL 33440", 
  #                                       "Louis Dreyfus Citrus Inc", "19100 SW Warfield Blvd, Indiantown, FL 34956")
  # citrus_processing <- citrus_processing[c(1:3, 5:11), ]
  # citrus_processing <- citrus_processing %>% tidygeocoder::geocode(addr, method = 'osm')
  # citrus_processing$lat[c(1,5,9)] <- c(27.895890, 27.215060, 26.741240)
  # citrus_processing$long[c(1,5,9)] <- c(-81.498470, -81.906690, -81.128060)
  # citrus_processing <- st_as_sf(x = citrus_processing, coords = c("long", "lat"), crs = "+proj=longlat +datum=WGS84")
  # sim$maps$citrus_processing <- st_transform(citrus_processing, crs(sim$maps$citrus))
  
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
