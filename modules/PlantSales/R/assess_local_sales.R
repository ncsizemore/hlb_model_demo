.assess_local_sales <- function(infected_sales, inf_ind, params){
  # update infection indicator based on squares that received infected sales
  inf_ind$rasterbands[[1]] <- ifelse(infected_sales$rasterbands[[1]] >= 1, params$.pr_egg_infection, inf_ind$rasterbands[[1]])
  
  landls2 <- params$landls2
  landls2$rasterbands[[1]] <- ifelse(infected_sales$rasterbands[[1]] >= 1, 1, landls2$rasterbands[[1]])
  
  return(list(inf_ind, landls2))
  
}

# 
# infected_sales <- mySim$infected_sales
# inf_ind <- mySim$inf_ind
# landls2 <- G(mySim)$.landls2
# params <- c(G(mySim),P(mySim))
# 
# 
# plot(infected_sales$as.RasterLayer())
# plot(inf_ind$as.RasterLayer())
# plot(landls2$as.RasterLayer())
