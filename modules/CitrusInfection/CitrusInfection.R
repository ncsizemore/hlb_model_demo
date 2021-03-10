## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "CitrusInfection",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.0", CitrusInfection = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "day",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "CitrusInfection.Rmd")),
  reqdPkgs = list("raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("assess_infection_initial_time", "numeric", 1, NA, NA,
                    "Describes the simulation time at which the first citrus infection advancement events should occur."),
    defineParameter("assess_infection_time_step", "numeric", 1, NA, NA,
                    "Describes the simulation time at which the first citrus infection advancement events should occur."),
    defineParameter("advance_symptoms_time_step", "numeric", 1, NA, NA,
                    "Describes the time step for citrus infection advancement events should occur."),
    defineParameter("infection_threshold", "numeric", 1, NA, NA,
                    "Number of infected psyllids required in a citrus cell for infection to be considered present"),
    defineParameter("citrus_removal_mean", "numeric", 365, NA, NA,
                    "mean of exponential distribution for waiting time after
                    symptoms develop until removal of the given block"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = dplyr::bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("maps", "list", desc = "list of various map params.", sourceURL = NA),
    expectsInput("tensors", "list", "list of various map params"),
    expectsInput("functions", "tensorflow.tensor", "adult uninfected/infected tensors"),
    expectsInput("adults", "tensorflow.tensor", "adult uninfected/infected tensors"),
    expectsInput("infected_citrus", "RasterLayer", desc = "indicator of infected squares - value is the number days a square has been infected", sourceURL = NA),
    expectsInput("symptomatic_citrus", "RasterLayer", desc = "0/1 indicator of symptomatic squares", sourceURL = NA),
    expectsInput("removed_citrus", "RasterLayer", desc = "0/1 indicator of symptomatic squares removed from production", sourceURL = NA),
    expectsInput("citrus_symptom_times", "RasterLayer", desc = "raster of randomly generated times for citrus to become symptomatic after becoming infected", sourceURL = NA),
    expectsInput("citrus_removal_times", "RasterLayer", desc = "raster of randomly generated times for citrus to be removed by growers after becoming symptomatic", sourceURL = NA)
  ),
  outputObjects = dplyr::bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput("infected_citrus", "RasterLayer", desc = "indicator of infected squares - value is the number days a square has been infected"),
    createsOutput("symptomatic_citrus", "RasterLayer", desc = "0/1 indicator of symptomatic squares"),
    createsOutput("removed_citrus", "RasterLayer", desc = "0/1 indicator of removed squares")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.CitrusInfection = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$assess_infection_initial_time, "CitrusInfection", "assess_infection")
    },
    assess_infection = {
      # ! ----- EDIT BELOW ----- ! #
      sim <- AssessInfection(sim)
      
      sim <- scheduleEvent(sim,  time(sim) + P(sim)$assess_infection_time_step, "CitrusInfection", "assess_infection")
      # ! ----- STOP EDITING ----- ! #
    },
    advance_symptoms = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      sim <- AdvanceSymptoms(sim)

      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$advance_symptoms_time_step, "CitrusInfection", "advance_symptoms")

      # ! ----- STOP EDITING ----- ! #
    },
    remove_citrus = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      sim <- RemoveCitrus(sim)

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}


Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

AssessInfection <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  
  #browser()
  
   sim$maps$tensors$infected_citrus <- sim$maps$tensors$citrus * (sim$maps$tensors$infected_citrus + tf$where(tf$greater(sim$adults[2,,,1], P(sim)$infection_threshold), 1, 0))
   
   if (as.logical(tf$greater(tf$reduce_sum(sim$maps$tensors$infected_citrus), 0))){
     sim <- scheduleEvent(sim, time(sim), "CitrusInfection", "advance_symptoms")
   }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


AdvanceSymptoms <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  
  sim$maps$tensors$symptomatic_citrus <- sim$maps$tensors$citrus * (sim$maps$tensors$symptomatic_citrus + tf$where(tf$greater(sim$maps$tensors$infected_citrus, sim$maps$tensors$citrus_symptom_times), 1, 0))
  
  
  if (as.logical(tf$greater(tf$reduce_sum(sim$maps$tensors$symptomatic_citrus), 0))){
    sim <- scheduleEvent(sim, time(sim), "CitrusInfection", "remove_citrus")
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


RemoveCitrus <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  sim$maps$tensors$removed_citrus <- sim$maps$tensors$citrus * (sim$maps$tensors$symptomatic_citrus + tf$where(tf$greater(sim$maps$tensors$symptomatic_citrus, sim$maps$tensors$citrus_removal_times), 1, 0))

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
  #sim$maps$citrus <- readRDS(paste0(simPaths$inputPath, "crops_04.rds"))
  
  #browser()
  infected_citrus <- 0 * sim$maps$citrus
  symptomatic_citrus <- infected_citrus
  removed_citrus <- infected_citrus
  
  sim$maps$tensors$infected_citrus <- sim$.mods$UpdateParameters$ras_to_tens(infected_citrus)
  sim$maps$tensors$symptomatic_citrus <- sim$.mods$UpdateParameters$ras_to_tens(symptomatic_citrus)
  sim$maps$tensors$removed_citrus <- sim$.mods$UpdateParameters$ras_to_tens(removed_citrus)
  #citrus_bool <- sim$maps$citrus[] == 1
  
  citrus_symptom_times <- sim$maps$citrus
  citrus_removal_times <- sim$maps$citrus
  
  n_citrus_cells <- length(sim$maps$citrus_cells)

  modes <- seq(from = 1, to = 2.5, length = nrow(citrus_symptom_times))
  #gamma_mode <- 2
  gamma_tail_point <- 6
  gamma_tail_prob <- 0.99
  
  a_vec <- lapply(modes, function(mode){
    uniroot(function(a) { s <- mode/(a-1); qgamma(gamma_tail_prob, shape = a, scale = s) - gamma_tail_point}, lower = 1, upper = 20 )$root
  })
  a_vec <- unlist(a_vec)
  s_vec <- modes/(a_vec - 1)
  
  times <- rgamma(n_citrus_cells, shape = a_vec, scale = s_vec )
  
  citrus_symptom_times[sim$maps$citrus_cells] <- 365*times
  sim$maps$tensors$citrus_symptom_times <- sim$.mods$UpdateParameters$ras_to_tens(citrus_symptom_times)
  
  citrus_removal_times[sim$maps$citrus_cells] <- rexp(n_citrus_cells, 1/P(sim)$citrus_removal_mean)
  sim$maps$tensors$citrus_removal_times <- sim$.mods$UpdateParameters$ras_to_tens(citrus_removal_times)

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
