.plotTM <- function(uninfected, infected, citrus, water, infected_sales){
 tm <- tm_shape(uninfected$as.RasterLayer()) + tm_raster(legend.show = FALSE, col = "layer", palette = "ivory2") + 
    tm_shape(water) + tm_raster(legend.show = FALSE, col = "layer", alpha = 0.5, palette = "steelblue1") + 
    tm_shape(threshold_NA(citrus,0)) + tm_raster(legend.show = FALSE, col = "layer", palette = "orange") + 
    tm_shape(threshold_NA(infected_sales$as.RasterLayer(),0)) + tm_raster(col = "layer", palette = "tomato") + 
    tm_shape(threshold_NA(uninfected$as.RasterLayer(band=20), 1)) + tm_raster(style = "headtails", colorNA = NULL, palette = "YlGn")+
    #tm_shape(uninfected$as.RasterLayer(band=20)) + tm_raster(style = "headtails", colorNA = NULL, palette = "YlGn")+
    tm_shape(threshold_NA(infected$as.RasterLayer(band=20), 1)) + tm_raster(style = "headtails", colorNA = NULL, palette = "OrRd")
  
  return(tm)
}

threshold_NA <- function(ras, threshold){
  ras[!(ras > threshold)] <- NA
  return(ras)
}