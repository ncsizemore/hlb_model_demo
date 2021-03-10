.remove_citrus <- function(symp_citrus, removed_citrus, citrus_removal_times, params){
  #inf_citrus <- (1/G(mySim)$.pr_egg_infection)* inf_ind$as.RasterLayer() * citrus + inf_citrus

  removed_citrus[] <- ifelse(symp_citrus[] > citrus_removal_times[], 1, removed_citrus[])
  return(removed_citrus)
}