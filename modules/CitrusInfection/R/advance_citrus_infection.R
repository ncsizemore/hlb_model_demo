.advance_citrus_infection <- function(inf_ind, inf_citrus, symp_citrus, citrus_symptom_times, params){
  inf_citrus <- (1/params$.pr_egg_infection)* inf_ind$as.RasterLayer() * params$citrus + inf_citrus

  symp_citrus[] <- ifelse(inf_citrus[] > citrus_symptom_times[], 1, 0) + symp_citrus[]
  return(list(inf_citrus, symp_citrus))
}

# get infected citrus
# inf_citrus tracks the number of days that a cell has been infected.
# symp_citrus tracks number of days symptomatic

# on initialization, we simulate runif vars to decide how long each citrus square
# will remain asymptomatic

# we can also simulate exponential

