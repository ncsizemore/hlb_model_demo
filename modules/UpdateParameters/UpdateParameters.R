## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "UpdateParameters",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.3", UpdateParameters = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "day",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "UpdateParameters.Rmd")),
  reqdPkgs = list( "tensorflow", "lubridate", "terra"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("update_survival_rates_init_time", "numeric", 1, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter("update_movement_rates_init_time", "numeric", 1, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter("update_survival_rates_time_step", "numeric", 1, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter("update_movement_rates_time_step", "numeric", 1, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter("pr_psyllid_survival_init", "list", list("uninf" = c("invalid" = 0, "urban" = 0.88, "citrus" = 0.88), "inf" = c("invalid" = 0, "urban" = 0.88, "citrus" = 0.88)), NA, NA,
                    "initial proportions of psyllids that survive on c(invalid, urban, citrus) cells"),
    defineParameter("pr_psyllid_stay_init", "list", list("uninf" = c("invalid" = 0, "urban" = 0.65, "citrus" = 0.95), "inf" = c("invalid" = 0, "urban" = 0.65, "citrus" = 0.95)), NA, NA,
                    "initial proportions of psyllids that stay at their location on c(invalid, urban, citrus) cells"),
    defineParameter("pr_egg_survival_init", "list", list("uninf" = c("invalid" = 0, "urban" = 0.95, "citrus" = 0.95), "inf" = c("invalid" = 0, "urban" = 0.95, "citrus" = 0.95)), NA, NA,
                    "initial proportions of eggs that survive on c(invalid, urban, citrus) cells"),
    defineParameter("egg_rate_init", "list", list("uninf" = c("invalid" = 0, "urban" = 10, "citrus" = 25), "inf" = c("invalid" = 0, "urban" = 10, "citrus" = 25)), NA, NA,
                    "initial rate of egg laying per female on c(invalid, urban, citrus) cells"),
    defineParameter("disperse_urban_season", "character", c('January 1', 'December 31'), NA, NA, 
                    "Dates for urban dispersal"),
    defineParameter("disperse_groves_season", "character", c('January 1', 'December 31'), NA, NA, 
                    "Dates for grove dispersal"),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = dplyr::bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("date", "character", "current date of the simulation (incremented from starting date)."),
    expectsInput("maps", "list", "list of various map params"),
        expectsInput("citrus", "list", "list of various map params"),
    expectsInput("tensors", "list", "list of various map params"),
    expectsInput("functions", "list", "adult uninfected/infected tensors"),
        expectsInput("tens_to_ras", "function", "adult uninfected/infected tensors"),
        expectsInput("ras_to_tens", "function", "adult uninfected/infected tensors"),
        expectsInput("mat_to_ras", "function", "adult uninfected/infected tensors"),
    expectsInput("adults", "tensorflow.tensor", "adult uninfected/infected tensors"),
    expectsInput("developing", "tensorflow.tensor", "adult uninfected/infected tensors"),
    expectsInput("dispersers", "tensorflow.tensor", "adult uninfected/infected tensors"),
    expectsInput("pr_psyllid_survival", "tensorflow.tensor", "adult uninfected/infected tensors"),
    expectsInput("pr_egg_survival", "tensorflow.tensor", "adult uninfected/infected tensors"),
    expectsInput("pr_psyllid_stay", "tensorflow.tensor", "adult uninfected/infected tensors"),
    expectsInput("dispersers", "tensorflow.tensor", "adult uninfected/infected tensors"),
    expectsInput("dispersers", "tensorflow.tensor", "adult uninfected/infected tensors")
  ),
  outputObjects = dplyr::bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput("date", "character", "current date of the simulation (incremented from starting date)."),
    createsOutput("groves_dispersal_season", "character", "time in which grove dispersal is active."),
    createsOutput("urban_dispersal_season", "list", "list of rasters that describe wind.")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.UpdateParameters = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)
    },
    update_rates = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      sim <- UpdateRates(sim)

      # schedule future event(s)
      # e.g.,
      sim <- scheduleEvent(sim, time(sim) + P(sim)$update_survival_rates_time_step, "UpdateParameters", "update_rates", eventPriority = 1.5)

      # ! ----- STOP EDITING ----- ! #
    },
    advance_date = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- AdvanceDate(sim)

      sim <- scheduleEvent(sim, time(sim) + 1, "UpdateParameters", "advance_date", eventPriority = 1)
      
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
 
  # schedule future event(s)
  sim <- scheduleEvent(sim, 1, "UpdateParameters", "advance_date", eventPriority = 1.25)
  sim <- scheduleEvent(sim, P(sim)$update_survival_rates_init_time, "UpdateParameters", "update_rates", eventPriority = 1)
    

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


### template for your event1
UpdateRates <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # if (as.logical(tf$reduce_any(tf$math$is_nan(sim$adults)))){
  #   browser()
  # }
  
  new_maps <- .update_rates(sim$adults, sim$maps$tensors$pr_psyllid_survival, sim$maps$tensors$pr_egg_survival, sim$maps$tensors$egg_rate, sim$maps$tensors$adj_egg_rate, sim$maps$tensors$pr_psyllid_stay, sim$wind$wind_dispersal_prob_T, G(sim)$cell_capacity, G(sim)$perc_female, sim)
  
  # bench::mark(.update_rates(sim$adults, sim$developing, sim$maps$pr_psyllid_survival_T, sim$maps$pr_egg_survival_T, sim$maps$egg_rate_T, sim$maps$pr_psyllid_stay_T, G(sim)$cell_capacity),
  #             .update_rates_tf(sim$adults, sim$developing, sim$maps$pr_psyllid_survival_T, sim$maps$pr_egg_survival_T, sim$maps$egg_rate_T, sim$maps$pr_psyllid_stay_T, G(sim)$cell_capacity),
  #             min_time = 10
  #             )
  
  #browser()
  
  #tf$reduce_any(tf$less(new_maps[[6]],0))
  
  sim$maps$tensors[["pr_psyllid_survival"]]$assign(new_maps[[1]])
  sim$maps$tensors[["pr_egg_survival"]]$assign(new_maps[[2]])
  sim$maps$tensors[["egg_rate"]]$assign(new_maps[[3]])
  sim$maps$tensors[["pr_psyllid_stay"]]$assign(new_maps[[4]])
  sim$maps$tensors[["adj_egg_rate"]]$assign(new_maps[[6]])

  sim$dispersers$assign(new_maps[[5]])
  
  # if (as.logical(tf$reduce_any(tf$math$is_nan(sim$adults)))){
  #   browser()
  # }
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


### template for your event2
AdvanceDate <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  # if (as.logical(tf$reduce_any(tf$math$is_nan(sim$adults)))){
  #   browser()
  # }
  
  sim$date <- sim$date + days(1)
  
  daily_updates <- .advance_date_tf(sim$adults, sim$developing, sim$adults_cropped_prev_hres, sim$adults_cropped_duration_hres, sim$maps$tensors$pr_psyllid_survival, sim$maps$tensors$pr_egg_survival)
  sim$adults$assign(daily_updates[[1]])
  sim$developing$assign(daily_updates[[2]])
  sim$adults_cropped_duration_hres$assign(daily_updates[[3]])
  
  .daily_plot(sim)
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
  # dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  # message(currentModule(sim), ": using dataPath '", dPath, "'.")
  # 
  # #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  # dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  # message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  
  # maps <- sim$maps
  # maps.ordered <- maps[order(names(maps))]
  # which(map(maps.ordered, class) == "SpatRaster")
  # maps.packed <- map(maps.ordered, ~ if(class(.x) == "SpatRaster"){terra::pack(.x)} else {.x} )
  # saveRDS(maps.packed, paste0(simPaths$inputPath, "maps.packed.rds"), compress = FALSE)
  # qs::qsave(maps.packed, paste0(simPaths$inputPath, "maps_qs.qs"))
  # maps.qs <- qs::qread(paste0(simPaths$inputPath, "maps_qs.qs"))
  # If updating the above file, change it in functions.R file as well
    
    maps <- purrr::map(qs::qread(paste0(simPaths$inputPath, "maps_qs.qs")), 
                       ~ if(class(.x) == "PackedSpatRaster"){terra::rast(.x)} else {.x} ) 
    
    ### Adjust uninfected params
    maps$pr_psyllid_survival[[1]][maps$invalid_cells] <- P(sim)$pr_psyllid_survival_init$uninf["invalid"]
    maps$pr_psyllid_survival[[1]][maps$valid_urban_cells] <- P(sim)$pr_psyllid_survival_init$uninf["urban"]
    maps$pr_psyllid_survival[[1]][maps$citrus_cells] <- P(sim)$pr_psyllid_survival_init$uninf["citrus"]

    maps$pr_egg_survival[[1]][maps$invalid_cells] <- P(sim)$pr_egg_survival_init$uninf["invalid"]
    maps$pr_egg_survival[[1]][maps$valid_urban_cells] <- P(sim)$pr_egg_survival_init$uninf["urban"]
    maps$pr_egg_survival[[1]][maps$citrus_cells] <- P(sim)$pr_egg_survival_init$uninf["citrus"]
    
    maps$egg_rate[[1]][maps$invalid_cells] <- P(sim)$egg_rate_init$uninf["invalid"]
    maps$egg_rate[[1]][maps$valid_urban_cells] <- P(sim)$egg_rate_init$uninf["urban"]
    maps$egg_rate[[1]][maps$citrus_cells] <- P(sim)$egg_rate_init$uninf["citrus"]

    maps$pr_psyllid_stay[[1]][maps$invalid_cells] <- P(sim)$pr_psyllid_stay_init$uninf["invalid"]
    maps$pr_psyllid_stay[[1]][maps$valid_urban_cells] <- P(sim)$pr_psyllid_stay_init$uninf["urban"]
    maps$pr_psyllid_stay[[1]][maps$citrus_cells] <- P(sim)$pr_psyllid_stay_init$uninf["citrus"]
    
    ### Adjust infected params
    maps$pr_psyllid_survival[[2]][maps$invalid_cells] <- P(sim)$pr_psyllid_survival_init$inf["invalid"]
    maps$pr_psyllid_survival[[2]][maps$valid_urban_cells] <- P(sim)$pr_psyllid_survival_init$inf["urban"]
    maps$pr_psyllid_survival[[2]][maps$citrus_cells] <- P(sim)$pr_psyllid_survival_init$inf["citrus"]
    
    maps$pr_egg_survival[[2]][maps$invalid_cells] <- P(sim)$pr_egg_survival_init$inf["invalid"]
    maps$pr_egg_survival[[2]][maps$valid_urban_cells] <- P(sim)$pr_egg_survival_init$inf["urban"]
    maps$pr_egg_survival[[2]][maps$citrus_cells] <- P(sim)$pr_egg_survival_init$inf["citrus"]
    
    maps$egg_rate[[2]][maps$invalid_cells] <- P(sim)$egg_rate_init$inf["invalid"]
    maps$egg_rate[[2]][maps$valid_urban_cells] <- P(sim)$egg_rate_init$inf["urban"]
    maps$egg_rate[[2]][maps$citrus_cells] <- P(sim)$egg_rate_init$inf["citrus"]
    
    maps$pr_psyllid_stay[[2]][maps$invalid_cells] <- P(sim)$pr_psyllid_stay_init$inf["invalid"]
    maps$pr_psyllid_stay[[2]][maps$valid_urban_cells] <- P(sim)$pr_psyllid_stay_init$inf["urban"]
    maps$pr_psyllid_stay[[2]][maps$citrus_cells] <- P(sim)$pr_psyllid_stay_init$inf["citrus"]
    
    sim$maps <- maps

    wind_dx <- sim$maps$land
    wind_dy <- sim$maps$land
    
    # rand_wind_dx <- runif(length(wind_dx[]), -2, -2)
    # rand_wind_dy <- runif(length(wind_dx[]), 0, 0)
    # 
    # #non_na_cells <- !is.na(wind_dx[])
    # values(wind_dx) <- rand_wind_dx
    # values(wind_dy) <- rand_wind_dy
    
    wind_dispersal_prob <- sim$maps$land
    #wind_dispersal_prob[] <- ifelse(!is.na(sim$maps$land[]), G(sim)$wind_dispersal_prob_under_capacity, sim$maps$land[])  # else some small chance of wind dispersing anyway
    
    sim$wind <- list("wind_dx" = wind_dx, "wind_dy" = wind_dy, "wind_dispersal_prob" = wind_dispersal_prob)

    sim$wind["wind_dispersal_prob_T"] <- 1
    sim$wind$wind_dispersal_prob_T <- tf$Variable(ras_to_tens(sim$wind[[3]]))
    
  

    sim$date <- mdy(G(sim)$start_date)
    sim$urban_dispersal_season <- mdy(paste(P(sim)$disperse_urban_season, year(mdy(G(sim)$start_date))))
    sim$groves_dispersal_season <- mdy(paste(P(sim)$disperse_groves_season, year(mdy(G(sim)$start_date))))
  
    if (mdy(G(sim)$start_date) >= sim$groves_dispersal_season[[2]]){
      sim$groves_dispersal_season <- sim$groves_dispersal_season + years(1)
    }else if (mdy(G(sim)$start_date) >= sim$groves_dispersal_season[[1]]){
      sim$groves_dispersal_season[[1]] <- mdy(G(sim)$start_date)
    }
    
    
    if (mdy(G(sim)$start_date) >= sim$urban_dispersal_season[[2]]){
      sim$urban_dispersal_season <- sim$urban_dispersal_season + years(1)
    }else if (mdy(G(sim)$start_date) >= sim$urban_dispersal_season[[1]]){
      sim$urban_dispersal_season[[1]] <- mdy(G(sim)$start_date)
    }
    
    sim$maps$extents$citrus_zoom_1 <- extent(539780.710071081, 557981.624356796, 2976996.27659564, 2992323.36230992)
    sim$maps$extents$citrus_zoom_1_tight <- extent(554407.183792247, 556489.904355187, 2981528.59054962, 2983254.27330177)
    sim$maps$extents$homestead <- extent(549454.5, 559946.1, 2813841, 2820400)

    
    sim$maps$citrus_zoom_1_cells <- cells(sim$maps$citrus_cropped_hres, ext(sim$maps$extents$citrus_zoom_1))
    sim$maps$citrus_zoom_1_tight_cells <- cells(sim$maps$citrus_cropped_hres, ext(sim$maps$extents$citrus_zoom_1_tight))
    
    #browser()
    initial_psyllids <- rast(readRDS(paste0(simPaths$inputPath, "uninfected_init.rds")))
    initial_psyllids[sim$maps$na_cells] <- 0
    #initial_psyllids_cropped <- crop(initial_psyllids, sim$maps$extents$citrus)
    # cells <- terra::cells(initial_psyllids, terra::ext(sim$maps$extents$citrus_zoom_1_tight))
    # sample(cells[which(sim$maps$citrus[cells] == 1)], 5)
  
    adults_tmp <- tf$constant(terra::as.matrix(initial_psyllids, wide=TRUE), dtype = tf$float32)
    developing_tmp <- tf$zeros_like(adults_tmp)
  
    sim$maps$homestead_T <- ras_to_tens(sim$maps$homestead)

    sim$adults <- tf$Variable(tf$expand_dims(tf$stack(list(adults_tmp, developing_tmp)), -1L))
    sim$adults_cropped_prev <- tf$Variable(sim$adults[, sim$maps$citrus_rows,  sim$maps$citrus_cols,])
    sim$adults_cropped_prev_hres <- tf$Variable(tf$keras$layers$UpSampling2D(size=c(8L,5L))(sim$adults_cropped_prev))
    #sim$new_adults_cropped_hres <- tf$Variable(tf$zeros_like(sim$adults_cropped_hres))
    
    #############
    ### start here 
    #browser()
    # I think this is all obsolete as it just samples citrus to get
    # some psyllids initially assigned to groves.
    # commenting out for now.  If it's not, we need to fix to work with new
    # tens_to_ras
    # acphr <- sim$functions$tens_to_ras(sim$adults_cropped_prev_hres)
    # cells <- sim$maps$citrus_zoom_1_tight_cells
    # valid_cells <- cells[which(sim$maps$citrus_cropped_hres[cells] == 1)]
    # sample <- sample(valid_cells, 35)
    # acphr[sample] <- 10
    # sim$acphr <- acphr; sim$sampled_valid <- sample
    # sim$adults_cropped_prev_hres[1,,,1] # why does this have NaNs?
    # sim$adults_cropped_prev_hres[1,,,1]$assign(terra::as.matrix(acphr, wide=TRUE))
    # sim$adults_cropped_prev$assign(tf$constant(8*5, dtype=sim$adults$dtype) * tf$nn$avg_pool2d(sim$adults_cropped_prev_hres, ksize = c(8L,5L), strides = c(8L,5L), padding = "SAME"))
    # sim$adults[,sim$maps$citrus_rows,sim$maps$citrus_cols,]$assign(sim$adults_cropped_prev)
    # 
    # raster::plot(raster(sim$functions$tens_to_ras(sim$adults_cropped_prev_hres)), ext=sim$maps$extents$citrus_zoom_1_tight)
    # raster::plot(raster(sim$functions$tens_to_ras(sim$adults_cropped_prev)), ext=sim$maps$extents$citrus_zoom_1_tight)
    
    #############
    
    #sim$adults_cropped_perc_hres <- tf$Variable(tf$zeros_like(sim$adults_cropped_hres))
    #sim$adults_cropped_indicator <- tf$Variable(tf$zeros_like(sim$adults_cropped))
    sim$adults_cropped_duration_hres <- tf$Variable(tf$zeros_like(sim$adults_cropped_prev_hres))
    
    sim$developing <- tf$Variable(tf$zeros(shape(2L, nrow(initial_psyllids), ncol(initial_psyllids), G(sim)$developmental_days), dtype=sim$adults$dtype))
    sim$dispersers <- tf$Variable(tf$zeros_like(sim$adults))
    

    
    sim$maps$tensors <- list(
      "citrus" = ras_to_tens(sim$maps$citrus),
      "citrus_cropped" = tf$constant(terra::as.matrix(sim$maps$citrus_cropped, wide=TRUE), dtype = tf$float32),
      "citrus_cropped_hres" = tf$expand_dims(tf$expand_dims(tf$constant(terra::as.matrix(sim$maps$citrus_cropped_hres, wide=TRUE), dtype = tf$float32), 0L), -1L),
      "citrus_cropped_hres_arrivals" = tf$Variable(tf$zeros(shape=sim$adults_cropped_prev_hres$shape, dtype=tf$float32)),
      "pr_psyllid_survival" = tf$Variable(ras_to_tens(sim$maps$pr_psyllid_survival)),
      "egg_rate" = tf$Variable(ras_to_tens(sim$maps$egg_rate)),
      "pr_egg_survival" = tf$Variable(ras_to_tens(sim$maps$pr_egg_survival)),
      "pr_psyllid_stay" = tf$Variable(ras_to_tens(sim$maps$pr_psyllid_stay)),
      "pr_psyllid_stay_cropped" = tf$Variable(ras_to_tens(crop(sim$maps$pr_psyllid_stay, sim$maps$extents$citrus))),
      "dispersers" = tf$Variable(tf$zeros_like(sim$adults))
    )

    #browser()

    sim$maps$tensors[["citrus_cropped_hres_flat"]] <- tf$expand_dims(tf$reshape(sim$maps$tensors$citrus_cropped_hres, shape = shape(-1L)),0L)
    sim$maps$tensors[["citrus_cropped_hres_pr"]] <- tf$Variable(sim$maps$tensors$citrus_cropped_hres_flat)
    sim$maps$tensors[["pr_psyllid_stay_cropped_hres"]] <- tf$Variable(tf$keras$layers$UpSampling2D(size=c(8L,5L), dtype=tf$float32)(sim$maps$tensors$pr_psyllid_stay_cropped))
    
    # adjust citrus sampling prob for random insertions
    # one_initial_location <- sample(which(!initial_psyllids[] == 0), 1)
    one_initial_location <- sample(which(sim$maps$homestead[] == 1), 1)
    a <- prob_ras(one_initial_location)
    # b <- sim$maps$citrus
    # a[sim$maps$citrus_rows,] <- exp(-abs(1352 - sim$maps$citrus_rows))
    # a <- matrix(rep(exp(-abs(1352 - sim$maps$citrus_rows)), length(sim$maps$citrus_cols)), nrow=length(sim$maps$citrus_rows), byrow = FALSE)
    # b[sim$maps$citrus_rows, sim$maps$citrus_cols] <- a
    # exp(-abs(1352 - sim$maps$citrus_rows))
    crs(a) <- crs(sim$maps$citrus)
    # sim$plot( raster((a * sim$maps$citrus)/sum((a * sim$maps$citrus)[], na.rm=T)) )
    # 
    # 
    # rast <- sim$maps$landls2
    # idx <- terra::rowColFromCell(rast, 2) 
    # i <- idx[1]; j <- idx[2]
    # mat <- terra::as.matrix(rast, wide=TRUE)
    # mat2 <- exp(-abs(1352 - row(mat) * terra::as.matrix(sim$maps$citrus, wide = TRUE)))
    # mat2 <- mat2/sum(mat2, na.rm=T)
    # mat2r <- sim$functions$mat_to_ras(mat2)
    
    sim$maps$tensors[["citrus_cropped_hres_pr"]]$assign(tf$expand_dims(tf$reshape(
      tf$keras$layers$UpSampling2D(size=c(8L,5L), dtype=tf$float32)(
        tf$expand_dims(tf$expand_dims(ras_to_tens(
          crop((a * sim$maps$citrus)/sum((a * sim$maps$citrus)[], na.rm=T), sim$maps$extents$citrus)
          ),0L),-1L)
      ), shape = shape(-1L)
    ),0L))
    
    # multiply the appropriate female percentage by the corresponding portion of parameter stacks
    # (there might be a more concise version of this)
    sim$maps$tensors[["adj_egg_rate"]] <- tf$Variable(tf$stack(
      list((tf$constant(G(sim)$perc_female["uninf"], dtype = sim$maps$tensors$pr_psyllid_stay$dtype) * sim$maps$tensors$pr_psyllid_stay * sim$maps$tensors$egg_rate)[1,,,], 
           (tf$constant(G(sim)$perc_female["inf"], dtype = sim$maps$tensors$pr_psyllid_stay$dtype) * sim$maps$tensors$pr_psyllid_stay * sim$maps$tensors$egg_rate)[2,,,]
      ), axis=0L))

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
