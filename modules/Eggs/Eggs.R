## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "Eggs",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.0", Eggs = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "day",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "Eggs.Rmd")),
  reqdPkgs = list("tensorflow"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("egg_time_step", "numeric", 1, NA, NA, 
                    "This describes the simulation time step interval"),
    defineParameter("egg_initial_time", "numeric", 1, NA, NA, 
                    "This describes the initial time for eggs"),
    defineParameter("egg_transmission_threshold", "numeric", 25, NA, NA, 
                    "number of infected psyllids necessary for 
                    uninfected adults to begin producing infected eggs
                    at pr_egg_infection rate"),
    defineParameter("pr_egg_infection", "numeric", 0.17, NA, NA, 
                    "proportion of uninfected adult eggs that become infected"),
    defineParameter("perc_female", "numeric", c("uninf" = 0.5, "inf" = 0.625), NA, NA, 
                    "Percentage of female psyllids")
  ),
  inputObjects = dplyr::bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("maps", "list", "list of various map params. ones beginning with . are updated; others are fixed."),
    expectsInput("developing", "tensorflow.tensor", "infected raster"),
    expectsInput("adults", "tensorflow.tensor", "infected raster")
  ),
  outputObjects = dplyr::bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    #createsOutput("uninfected", "VeloxRaster", "uninfected raster"),
    #createsOutput("infected", "VeloxRaster", "infected raster"),
    #createsOutput("adults", "tensorflow.tensor", "uninfected raster"),
    #createsOutput("developing", "tensorflow.tensor", "infected raster")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.Eggs = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$egg_initial_time, "Eggs", "progress")
    },
    progress = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- ProgressEggs(sim)

      # schedule future event(s)

      # e.g.,
      sim <- scheduleEvent(sim, time(sim) + P(sim)$egg_time_step, "Eggs", "progress")

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

ProgressEggs <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  #browser()
  # if (as.logical(tf$reduce_any(tf$math$is_nan(sim$adults)))){
  #   browser()
  # }
  
  progression <- .progress_eggs(sim$adults, sim$developing, sim$maps$tensors$adj_egg_rate, G(sim)$developmental_days, P(sim)$egg_transmission_threshold, P(sim)$pr_egg_infection)

  
  sim$adults$assign(progression[[1]])

  sim$developing$assign(progression[[2]])
  
  # if (as.logical(tf$reduce_any(tf$math$is_nan(sim$adults)))){
  #   browser()
  # }
  
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
  #dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  #message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #



  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
