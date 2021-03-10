library(SpaDES); library(tensorflow); library(tfprobability)

source("paths_and_modules.R")

sim_start_date <- 'November 28, 1998'
sim_end_date <- 'December 31, 2009'


## Set simulation and module parameters
simTimes <- list(start = 0, 
                 end = as.numeric(lubridate::mdy(sim_end_date) - lubridate::mdy(sim_start_date)))
simParams <- list(
  .globals=list(start_date = sim_start_date,
                harvest_dates = c('December 1','December 20','March 31','June 20'),
                perc_female = c("uninf" = 0.5, "inf" = 0.625),
                developmental_days = 17L,
                cell_capacity = 5000000L,
                cell_establishment_goal = 100000L,
                wind_dispersal_prob_over_capacity = 0,
                wind_dispersal_prob_under_capacity = 0,
                plot_extent = "full", #"citrus_zoom_1_tight",#"full",# "initial",
                plot_object = "adults",#"infected",#"developing", #c("maps", "pr_psyllid_survival_T"),
                plot_range = NA, #c(0,100000) #c(0,1),
                simulation_begin_time = stringr::str_replace_all(Sys.time(), ":", "-")
                ),
  #.checkpoint = list(interval = 30, file = "chkpnt.RData"),
  PsyllidSpread = list(disperse_urban_time_step = 1, 
                       disperse_groves_time_step = 1, 
                       disperse_urban_season = c("start" = 'January 1', "end" = 'December 31'),
                       disperse_groves_season = c("start" = 'January 1', "end" = 'December 31'),
                       weight_matrix_urban = c(4 * 402.336, "Gauss"),
                       weight_matrix_groves = c(200, "Gauss"),
                       poisson_days_between_jumps = 100L
                       ),
  Eggs = list(egg_time_step = 1,
              egg_transmission_threshold = 25,
              pr_egg_infection = 0.17,
              egg_initial_time = 1
              ),
  PlantSales = list(activate_sales_initial_time = 1L, 
                    activate_sales_time_step = 1L,
                    activate_sales_threshold = 100L,
                    distr_sales_time_step = 14L,
                    distr_sales_lambda = 5, 
                    local_sales_intensity = 1/7,
                    local_sales_time_step = 5L,
                    psyllids_per_plant = 10,
                    distr_inf_threshold = 1,
                    .useCache = FALSE),
  UpdateParameters = list(pr_psyllid_survival_init = 
                            list("uninf" = c("invalid" = 0, "urban" = 0.98, "citrus" = 0.985), 
                                 "inf" = c("invalid" = 0, "urban" = 0.88, "citrus" = 0.978)), # (.985^51)^(1/35)
                          pr_psyllid_stay_init = 
                            list("uninf" = c("invalid" = 0, "urban" = 0.5, "citrus" = 0.95),
                                 "inf" = c("invalid" = 0, "urban" = 0.65, "citrus" = 0.95)),
                          pr_egg_survival_init = 
                            list("uninf" = c("invalid" = 0, "urban" = 0.95, "citrus" = 0.8614),
                                 "inf" = c("invalid" = 0, "urban" = 0.95, "citrus" = 0.8614)),
                          egg_rate_init = 
                            list("uninf" = c("invalid" = 0, "urban" = 10, "citrus" = 20), 
                                 "inf" = c("invalid" = 0, "urban" = 20, "citrus" = 30)),
                          .useCache = FALSE),
  CitrusInfection = list(citrus_removal_mean = 365L,
                         infection_threshold = 100,
                         .useCache = FALSE),
  ControlMeasures = list(pesticides_initial_time = 1L,
                         .useCache = FALSE),
  Wind = list(generate_wind_initial_time = 1L,
             generate_wind_time_step = 5000L,
             advect_psyllids_initial_time = 5000L,
             advect_psyllids_time_step = 5000L,
            .useCache = FALSE)
)


mySim <- simInit(times = simTimes, params = simParams, modules = simModules, loadOrder = unlist(simModules), paths = simPaths)

dir <- paste0("png/", G(mySim)$plot_extent, "/", G(mySim)$simulation_begin_time)
ifelse(!dir.exists(file.path(dir)), dir.create(file.path(dir), recursive = TRUE), FALSE)

mySim <- spades(mySim, debug = TRUE)





