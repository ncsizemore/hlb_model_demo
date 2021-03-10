.generate_wind <- function(uninfected, params){
  
  uninf <- uninfected$as.RasterLayer(band=20)
  
  wind_dx <- params$land$as.RasterLayer()
  wind_dy <- params$land$as.RasterLayer()
  
  #rand_wind_dx <- runif(length(wind_dx[]), -2, -2) # left wind only
  rand_wind_dx <- runif(length(wind_dx[]), 0, 0)
  rand_wind_dy <- runif(length(wind_dx[]), 0, 0)
  
  non_na_cells <- !is.na(wind_dx[])
  wind_dx[non_na_cells] <- rand_wind_dx[non_na_cells]
  wind_dy[non_na_cells] <- rand_wind_dy[non_na_cells]
  
  wind_dispersal_prob <- params$land$as.RasterLayer()
  wind_dispersal_prob[] <- ifelse(uninf[] > params$.cell_capacity, params$.wind_dispersal_prob_over_capacity, # if greater than cell capacity,  
                                  ifelse(!is.na(uninf[]), params$.wind_dispersal_prob_under_capacity, uninf[]) ) # else some small chance of wind dispersing anyway
  
  wind <- list(wind_dx, wind_dy, wind_dispersal_prob)
  
  return(wind)
}


