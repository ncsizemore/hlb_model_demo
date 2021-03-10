## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "PsyllidSpread",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.0", PsyllidSpread = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "day",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "PsyllidSpread.Rmd")),
  reqdPkgs = list("tensorflow", "terra", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", default value, min, max, "parameter description"),
    defineParameter("disperse_urban_time_step", "numeric", 1L, NA, NA, 
                    "This describes the simulation time step interval"),
    defineParameter("disperse_groves_time_step", "numeric", 1L, NA, NA, 
                    "This describes the simulation time step interval"),
    defineParameter("poisson_days_between_jumps", "numeric", 8L, NA, NA, 
                    "Expected days between jumps of wind invasion into new plots in groves"),
    defineParameter("psyllid_cycle_length", "numeric", c("uninf" = 51L, "inf" = 35L), NA, NA, 
                    "Frequency of NN dispersal in groves: 
                    psyllids at their current plot disperse to NNs in this schedule"),
    defineParameter("weight_matrix_urban", "character", c(402.336, "Gauss"), NA, NA, 
                    "Dispersal/Focal weight matrix (in form of focalWeight function)"),
    defineParameter("weight_matrix_groves", "character", c(402.336, "Gauss"), NA, NA, 
                    "Dispersal/Focal weight matrix (in form of focalWeight function)"),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = dplyr::bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("maps", "list", "list of various map params"),
    expectsInput("wind", "list", "wind vector components, wind dispersal probability"),
    expectsInput("weight_matrix_urban", "matrix", "uninfected raster"),
    expectsInput("weight_matrix_groves", "matrix", "wind vector components, wind dispersal probability"),
    expectsInput("groves_dispersal_season", "character", "time in which grove dispersal is active."),
    expectsInput("urban_dispersal_season", "character", "list of rasters that describe wind.")
),
  outputObjects = dplyr::bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
  )
))

## event types
#   - type `init` is required for initialization

doEvent.PsyllidSpread = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)
    },
    
    disperse_urban = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      sim <- DisperseUrban(sim)

      # schedule future event(s)

      # e.g.,
      if (sim$date + days(P(sim)$disperse_urban_time_step) >= sim$urban_dispersal_season[1] & sim$date + days(P(sim)$disperse_urban_time_step) <= sim$urban_dispersal_season[2]){
        sim <- scheduleEvent(sim, time(sim) + P(sim)$disperse_urban_time_step, "PsyllidSpread", "disperse_urban")
      } 
      # ! ----- STOP EDITING ----- ! #
    },
    disperse_groves = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      sim <- DisperseGroves(sim)
      
      # schedule future event(s)
      
      # e.g.,
      if (sim$date + days(P(sim)$disperse_groves_time_step) >= sim$groves_dispersal_season[1] & sim$date + days(P(sim)$disperse_groves_time_step) <= sim$groves_dispersal_season[2]){
        sim <- scheduleEvent(sim, time(sim) + P(sim)$disperse_groves_time_step, "PsyllidSpread", "disperse_groves")
      }
      # ! ----- STOP EDITING ----- ! #
    },
    sample_groves = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      sim <- SampleGroves(sim)
      
      # ! ----- STOP EDITING ----- ! #
    },
    update_dispersal_seasons = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- UpdateDispersalSeasons(sim)

      sim <- scheduleEvent(sim, time(sim) + 365, "PsyllidSpread", "update_dispersal_seasons")
      #browser()
      sim <- scheduleEvent(sim, time(sim) + as.numeric(sim$urban_dispersal_season[1] - sim$date), "PsyllidSpread", "disperse_urban")
      sim <- scheduleEvent(sim, time(sim) + as.numeric(sim$groves_dispersal_season[1] - sim$date), "PsyllidSpread", "disperse_groves")
      
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
  dec_31 <- as.numeric(mdy(paste("December 31,", year(mdy(G(sim)$start_date)))) - mdy(G(sim)$start_date))
  sim <- scheduleEvent(sim, dec_31, "PsyllidSpread", "update_dispersal_seasons", eventPriority = 6)

  if (sim$date < sim$groves_dispersal_season[1]){
    disperse_groves_initial_time <- sim$groves_dispersal_season[1] - days(1)
  }else if (sim$date >= sim$groves_dispersal_season[1] & sim$date <= sim$groves_dispersal_season[2]){
    disperse_groves_initial_time <- sim$date + 1
  }
  
  if (sim$date < sim$urban_dispersal_season[1]){
    disperse_urban_initial_time <- sim$urban_dispersal_season[1] - days(1)
  }else if (sim$date >= sim$urban_dispersal_season[1] & sim$date <= sim$urban_dispersal_season[2]){
    disperse_urban_initial_time <- sim$date + 1
  }
  
  date_to_sim_time <- function(date){
    as.numeric(date - mdy(G(sim)$start_date))
  }
  
  disperse_urban_initial_time <- date_to_sim_time(disperse_urban_initial_time)
  disperse_groves_initial_time <- date_to_sim_time(disperse_groves_initial_time)
  
  # schedule future event(s)
  sim <- scheduleEvent(sim, disperse_urban_initial_time, "PsyllidSpread", "disperse_urban")
  sim <- scheduleEvent(sim, disperse_groves_initial_time, "PsyllidSpread", "disperse_groves")

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}


DisperseUrban <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  #browser() 
    # citrus <- sim$maps$citrus
    # crs(citrus) <- crs(sim$maps$land)
    # region <- sim$maps$land + 1 - citrus
    # 
  if (as.logical(tf$reduce_any(tf$math$is_nan(sim$adults)))){
    browser()
  }
    
  #browser()  
  # tf$reduce_any(tf$less(sim$.mods$UpdateParameters$ras_to_tens(sim$maps$landls2),0))
  #urban dispersers: dispersers have to not leave by wind, but leave, and come from urban cells
  urban_landls2 <- sim$maps$landls2
  crs(urban_landls2) <- crs(sim$maps$citrus)
  urban_landls2 <- urban_landls2 - sim$maps$citrus
    sim$dispersers$assign(tf$expand_dims(tf$expand_dims(1 - sim$wind$wind_dispersal_prob_T,0L),-1L) * (1 - sim$maps$tensors$pr_psyllid_stay) * tf$expand_dims(tf$expand_dims(sim$.mods$UpdateParameters$ras_to_tens(urban_landls2), 0L),-1L) * sim$adults)
    
    

    #browser()
    
    sim$adults$assign(.disperse_urban(sim$adults, tf$constant(sim$weight_matrix_urban, dtype = sim$adults$dtype), sim$dispersers, sim$maps$tensors$pr_psyllid_stay))
    
    if (as.logical(tf$reduce_any(tf$math$is_nan(sim$adults)))){
      browser()
    }
    
    if (as.logical(tf$reduce_any(tf$less(sim$adults,0)))){
      browser()
    }
    
    if (as.numeric(tf$random$poisson(tf$constant(1L, shape = shape(1)), 1/P(sim)$poisson_days_between_jumps)) > 0){
        sim <- scheduleEvent(sim, time(sim), "PsyllidSpread", "sample_groves")
    }
 # browser()
 
    # bench::mark(
    #   .disperse(sim$adults, tf$constant(sim$weight_matrix_urban), dispersers, sim$maps$pr_psyllid_stay_T),
    #   .disperse_tf(sim$adults, tf$constant(sim$weight_matrix_urban), dispersers, sim$maps$pr_psyllid_stay_T),
    #   min_time = 10
    # )
    
   #browser()  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

DisperseGroves <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  #browser()
  if (as.logical(tf$reduce_any(tf$math$is_nan(sim$adults)))){
    browser()
  }
  
  
  adults_cropped_current_hres <- .update_adults_cropped_hres(sim)
  if (as.logical(tf$reduce_any(tf$math$is_nan(adults_cropped_current_hres)))){
    browser()
  }
  
   out <- .disperse_groves(sim$adults,  adults_cropped_current_hres, sim$adults_cropped_duration_hres, tf$constant(sim$weight_matrix_groves, dtype = sim$adults$dtype), sim$maps$citrus_rows, sim$maps$citrus_cols, sim$maps$tensors$pr_psyllid_stay_cropped_hres, P(sim)$psyllid_cycle_length[1], sim)

   sim$adults$assign(out[[1]])
   sim$adults_cropped_prev$assign(out[[2]])
   sim$adults_cropped_prev_hres$assign(out[[3]])

   if (as.logical(tf$reduce_any(tf$math$is_nan(sim$adults)))){
     browser()
   }
   
   gc()
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
   
   
}

SampleGroves <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  
  # if (as.logical(tf$reduce_any(tf$math$is_nan(sim$adults)))){
  #   browser()
  # }
  #browser()
      
      
      if (as.logical(tf$greater(tf$reduce_max(sim$adults), G(sim)$cell_establishment_goal))){
        
        if (as.logical(tf$greater(tf$reduce_max(sim$adults[2,,,1]), 
                                  .01*G(sim)$cell_establishment_goal))){
          psyllids_to_add <- c("uninf" = 10, "inf" = 0)
        } else{
          psyllids_to_add <- c("uninf" = 5, "inf" = 5)
        }

      
        adults_cropped_current_hres <- .update_adults_cropped_hres(sim)
        out <- .sample_citrus_cropped_hres(sim$adults, adults_cropped_current_hres, sim$maps$citrus_rows, sim$maps$citrus_cols, sim$maps$tensors$citrus_cropped_hres, sim$maps$tensors$citrus_cropped_hres_pr, psyllids_to_add, sim)
      
      
        sim$adults$assign(out[[1]])
        sim$adults_cropped_prev$assign(out[[2]])
        sim$adults_cropped_prev_hres$assign(out[[3]])
        sim$maps$tensors$citrus_cropped_hres_pr$assign(out[[4]])
  
      }
  
  # if (as.logical(tf$reduce_any(tf$math$is_nan(sim$adults)))){
  #   browser()
  # }
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


UpdateDispersalSeasons <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  #browser()
  date <- sim$date
  yrs <- ceiling(time_length(interval(mdy(G(sim)$start_date), date),"years"))
  
  sim$urban_dispersal_season <- mdy(paste(P(sim)$disperse_urban_season, year(mdy(G(sim)$start_date)))) + years(yrs)
  
  sim$groves_dispersal_season <- mdy(paste(P(sim)$disperse_groves_season, year(mdy(G(sim)$start_date)))) + years(yrs)
  
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
  #sim$maps$homestead <- readRDS(paste0(simPaths$inputPath, "homestead.rds"))
  
  ######################
  #browser()
  
  

  # ! ----- EDIT BELOW ----- ! #
  #sim$inf_ind <- sim$maps$homestead
  #sim$inf_ind_sales <- sim$maps$homestead
  #browser()
  #sim$inf_ind <- P(sim, "Eggs", "pr_egg_infection") * sim$inf_ind
  #sim$uninf_ind <- sim$inf_ind
  #sim$uninf_ind <- 1 - sim$inf_ind
  
  #browser()
  if (class(P(sim)$weight_matrix_urban) == "character"){
    sim$weight_matrix_urban <- raster::focalWeight(raster(sim$maps$landls2), 
                                                   as.numeric(P(sim)$weight_matrix_urban[[1]]),
                                                   P(sim)$weight_matrix_urban[[2]])
    
    sim$weight_matrix_urban[ceiling(nrow(sim$weight_matrix_urban)/2), 
                            ceiling(nrow(sim$weight_matrix_urban)/2)] <- 0
    sim$weight_matrix_urban <- sim$weight_matrix_urban/sum(sim$weight_matrix_urban)
  } else if ("matrix" %in% class(P(sim)$weight_matrix_urban)){
    sim$weight_matrix_urban <- P(sim)$weight_matrix_urban
    
    sim$weight_matrix_urban[ceiling(nrow(sim$weight_matrix_urban)/2), 
                            ceiling(nrow(sim$weight_matrix_urban)/2)] <- 0
    sim$weight_matrix_urban <- sim$weight_matrix_urban/sum(sim$weight_matrix_urban)
  }
  
  if (class(P(sim)$weight_matrix_groves) == "character"){
    sim$weight_matrix_groves <- raster::focalWeight(raster(sim$maps$landls2), 
                                                   as.numeric(P(sim)$weight_matrix_groves[[1]]),
                                                   P(sim)$weight_matrix_groves[[2]])
    
    sim$weight_matrix_groves[ceiling(nrow(sim$weight_matrix_groves)/2), 
                            ceiling(nrow(sim$weight_matrix_groves)/2)] <- 0
    sim$weight_matrix_groves <- sim$weight_matrix_groves/sum(sim$weight_matrix_groves)
  } else if ("matrix" %in% class(P(sim)$weight_matrix_groves)){
    sim$weight_matrix_groves <- P(sim)$weight_matrix_groves
    
    sim$weight_matrix_groves[ceiling(nrow(sim$weight_matrix_groves)/2), 
                            ceiling(nrow(sim$weight_matrix_groves)/2)] <- 0
    sim$weight_matrix_groves <- sim$weight_matrix_groves/sum(sim$weight_matrix_groves)
  }
  

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above


