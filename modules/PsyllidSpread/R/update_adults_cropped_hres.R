# this function should get called prior to any dispersing at grove scale
# it takes as input the adult populations at both resolutions after the last grove scale movement
# along with the current population at low resolution. it returns an updated version
# of the current population at high resolution
.update_adults_cropped_hres <- function(sim){
  #browser()
  # these get assigned after movement at grove scale
  prev_adults_cropped_hres <- sim$adults_cropped_prev_hres
  prev_adults_cropped <- sim$adults_cropped_prev
  
  # raster::plot(raster(sim$acphr), ext=sim$maps$extents$citrus_zoom_1_tight)
  # raster::plot(raster(sim$functions$tens_to_ras(sim$adults_cropped_prev_hres)), ext=sim$maps$extents$citrus_zoom_1_tight)
  # 
  # raster::plot(raster(sim$functions$tens_to_ras(adults_cropped_current_hres)), ext=sim$maps$extents$citrus_zoom_1_tight)
  
  prev_adults_cropped_indicator <- tf$where(tf$greater(prev_adults_cropped, 1, 0), 1, 0)
  
  
  ratio <- tf$math$divide_no_nan(sim$adults[,sim$maps$citrus_rows,sim$maps$citrus_cols,], prev_adults_cropped)
  #raster::plot(raster(sim$functions$tens_to_ras(ratio)), ext=sim$maps$extents$citrus_zoom_1_tight)
  ratio <- tf$keras$layers$UpSampling2D(size=c(8L,5L), dtype=sim$adults$dtype)(ratio)
  rescale_adults_cropped_hres <- prev_adults_cropped_hres * ratio
 # raster::plot(raster(sim$functions$tens_to_ras(rescale_adults_cropped_hres)), ext=sim$maps$extents$citrus_zoom_1_tight)
  
  current_adults_cropped_indicator <- tf$where(tf$greater(sim$adults[,sim$maps$citrus_rows,sim$maps$citrus_cols,],0), 1, 0)
  
  # check where adults
  check <- tf$math$logical_and(tf$equal(current_adults_cropped_indicator, 1), tf$equal(prev_adults_cropped_indicator, 0) )
  
  
  new_adults_to_add <- tf$cond(tf$math$reduce_any(check), 
          function(x){
     tf$keras$layers$UpSampling2D(size=c(8L,5L), dtype=sim$adults$dtype)(tf$cast(tf$where(check, 1, 0), dtype=sim$adults$dtype) * sim$adults[,sim$maps$citrus_rows,sim$maps$citrus_cols,])
    }, 
      function(x) {tf$zeros_like(sim$adults_cropped_prev_hres)}
    )
  
  current_adults_cropped_hres <- rescale_adults_cropped_hres + new_adults_to_add
  #raster::plot(raster(sim$functions$tens_to_ras(current_adults_cropped_hres)), ext=sim$maps$extents$citrus_zoom_1_tight)
  
  return(current_adults_cropped_hres)
 
}

.update_adults_cropped_hres_tf <- tf$`function`(.update_adults_cropped_hres)