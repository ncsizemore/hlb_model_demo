.advect_psyllids <- function(uninfected, wind, wind_dispersal_prob, grid, params){
  
  wind_dx <- wind[[1]]; wind_dy <- wind[[2]]
  
  grid_tmp <- grid
  grid_tmp$Var1 <- grid_tmp$Var1 + wind_dx[] # add wind
  grid_tmp$Var2 <- grid_tmp$Var2 + wind_dy[] # add wind
  
  cells <- raster::extract(uninfected$as.RasterLayer(band=20), grid_tmp, cellnumbers = TRUE)
  cells <- cbind(cells,1:nrow(cells))
  cells_sub <- cells[which(!cells[,1] == cells[,3] & !is.na(cells[,2])) ,] # remove cells which didn't move or went to NA location
  
  uninf <- uninfected$as.RasterLayer(band=20)
  #browser()
                                
  wind_dispersers <- wind_dispersal_prob * (1 - params$.pr_psyllid_stay$as.RasterLayer()) * uninf # select wind dispersers
  
  uninf[cells_sub[,1]] <- uninf[cells_sub[,1]] + wind_dispersers[cells_sub[,3]] # column 3 is origin, column 1 destination
  
  uninf <- params$landls2$as.RasterLayer() * uninf
  
  uninfected$rasterbands[[20]] <- raster::as.matrix(uninf)

  return(uninfected)
}