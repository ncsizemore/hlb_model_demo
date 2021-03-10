## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "Wind",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.3", Wind = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "day",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "Wind.Rmd")),
  reqdPkgs = list("pracma", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".generate_wind_initial_time", "numeric", 1, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".advect_psyllids_initial_time", "numeric", 1, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".generate_wind_time_step", "numeric", 1, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".advect_psyllids_time_step", "numeric", 1, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = dplyr::bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("grid", objectClass = "data.frame", desc = "xy coordinates of raster", sourceURL = NA)
  ),
  outputObjects = dplyr::bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.Wind = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.generate_wind_initial_time, "Wind", "generate_wind")
      sim <- scheduleEvent(sim, P(sim)$.advect_psyllids_initial_time, "Wind", "advect_psyllids")
    },
    generate_wind = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)
      
      sim <- GenerateWind(sim)

      # e.g.,
      sim <- scheduleEvent(sim, time(sim) +  P(sim)$.generate_wind_time_step, "Wind", "generate_wind")

      # ! ----- STOP EDITING ----- ! #
    },
    advect_psyllids = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function
      
      sim <- AdvectPsyllids(sim)

      # schedule future event(s)

      # e.g.,
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.advect_psyllids_time_step, "Wind", "advect_psyllids")

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

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}


### template for your event1
GenerateWind <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  
  sim$wind <- .generate_wind(sim$uninfected, c(sim$map_params,G(sim),P(sim)))

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
AdvectPsyllids <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  sim$uninfected <- .advect_psyllids(sim$uninfected, sim$wind, sim$wind[[3]], sim$grid, c(sim$map_params,G(sim),P(sim)))
  sim$infected <- .advect_psyllids(sim$infected, sim$wind, sim$wind[[3]], sim$grid, c(sim$map_params,G(sim),P(sim)))
    
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
  
  #browser()
  
  # ! ----- EDIT BELOW ----- ! #
  
  x_centers <- xFromCol(sim$uninfected$as.RasterLayer())
  y_centers <- yFromRow(sim$uninfected$as.RasterLayer())
  sim$grid <- expand.grid(x_centers, y_centers)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
