## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "ControlMeasures",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.3", ControlMeasures = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "day",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "ControlMeasures.Rmd")),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("pesticides_initial_time", "numeric", 1, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter("schedule_time_step", "numeric", 1, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = dplyr::bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = dplyr::bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.ControlMeasures = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$pesticides_initial_time, "ControlMeasures", "schedule_pesticides")
    },
    schedule_pesticides = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      sim <- SchedulePesticides(sim)
      
      if (as.logical(tf$reduce_any(tf$greater(sim$maps$tensors$scheduled_pesticides, 0)))){
        sim <- scheduleEvent(sim, time(sim), "ControlMeasures", "apply_pesticides")
      }
      
      # e.g.,
      sim <- scheduleEvent(sim, time(sim) + P(sim)$schedule_time_step, "ControlMeasures", "schedule_pesticides")
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$apply_pesticides_time_step, "ControlMeasures", "apply_pesticides")
      
      # ! ----- STOP EDITING ----- ! #
    },
    apply_pesticides = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      sim <- ApplyPesticides(sim)

      # e.g.,
      sim <- scheduleEvent(sim, time(sim), "ControlMeasures", "pesticides_kill")
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$apply_pesticides_time_step, "ControlMeasures", "apply_pesticides")

      # ! ----- STOP EDITING ----- ! #
    },
    pesticides_kill = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      sim <- PesticidesKill(sim)

      # e.g.,
      #browser()
 
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
SchedulePesticides <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
#browser()
  # increment schedule tensor on symptomatic squares
  sim$maps$tensors$scheduled_pesticides <- sim$maps$tensors$citrus * ( (sim$maps$tensors$scheduled_pesticides + tf$where(
    tf$greater(sim$maps$tensors$symptomatic_citrus, sim$maps$tensors$citrus_symptom_times),
    1, 0)) ) 
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
ApplyPesticides <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
#browser()
  
  # where scheduled are > 0 and == application time, return 1 (new)
  # all other places, decrease effectiveness
  daily_decrease_factor <- 0.25
  sim$maps$tensors$active_pesticides <- tf$where(
    tf$logical_and(tf$greater(sim$maps$tensors$scheduled_pesticides,0), 
                   tf$equal(sim$maps$tensors$scheduled_pesticides, sim$maps$tensors$application_time)),
           1, daily_decrease_factor*sim$maps$tensors$active_pesticides)
  
  # if any activepesticides  > 0 , then kill
  if (as.logical(tf$greater(tf$reduce_max(sim$maps$tensors$active_pesticides), 0))){
    sim <- scheduleEvent(sim, time(sim), "ControlMeasures", "pesticides_kill")
  }
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
PesticidesKill <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  # daily_kill_rate is how percentage that are killed in a day given fresh/full strength
  # application. as effect wears off, want this number to decrease
  daily_kill_rate <- c("uninf" = 0.25, "inf" = 0.25)
  
  # we assign to adults:
  # where active pesticides > 0, the killed version
  # elsewhere, the current amoutn
  sim$adults$assign( tf$where( tf$greater(
    tf$expand_dims(tf$stack(list(sim$maps$tensors$active_pesticides,
                                 sim$maps$tensors$active_pesticides),
                            axis=0L), -1L), 0),
    tf$expand_dims(tf$stack(list(sim$maps$tensors$active_pesticides * daily_kill_rate["uninf"],
                                 sim$maps$tensors$active_pesticides * daily_kill_rate["inf"]),
                            axis=0L), -1L), 
    sim$adults)
  ) 

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
  
  sim$maps$tensors$scheduled_pesticides <- tf$zeros_like(sim$maps$tensors$citrus)
  sim$maps$tensors$active_pesticides <- tf$zeros_like(sim$maps$tensors$citrus)
  # how long after scheduling until applying?
  sim$maps$tensors$application_time <- 14*sim$maps$tensors$citrus

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
