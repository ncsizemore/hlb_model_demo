.assess_infection <- function(inf_ind_sales, adults, params){
  browser()
  # Takes current infected adult psyllid population and checks to identify which squares are infected

  #inf_ind <- velox(infected$as.RasterLayer(band=20))
  
  inf_ind <- adults[,,1,2]
  
  # computes cells that are infected based on having more than one infected psyllid
  inf_ind$rasterbands[[1]] <- ifelse(floor(inf_ind$rasterbands[[1]] == 0), 0, floor(inf_ind$rasterbands[[1]])/floor(inf_ind$rasterbands[[1]]))
  
  # combines the above cells with cells from homestead and that have received an infected sale
  inf_ind$rasterbands[[1]] <- inf_ind$rasterbands[[1]] + params$homestead$rasterbands[[1]] + inf_ind_sales$rasterbands[[1]]
  
  # in case a cell is considered infected for multiple reasons, normalize it
  inf_ind$rasterbands[[1]] <- ifelse(inf_ind$rasterbands[[1]] > 0, 1, inf_ind$rasterbands[[1]])
  inf_ind$rasterbands[[1]] <- params$.pr_egg_infection *ifelse(!is.na(params$landls2$rasterbands[[1]]) & is.na(inf_ind$rasterbands[[1]]), 0, inf_ind$rasterbands[[1]]) 
  
  return(inf_ind)
}
