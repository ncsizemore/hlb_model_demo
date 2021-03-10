.daily_plot <- function(sim){
  plot_obj <- G(sim)$plot_object
  
  if (length(plot_obj) == 1){
    if (plot_obj == "infected"){
      r <- raster(tens_to_ras(sim$adults[2,,,1]))
    } else {
      object <- sim[[plot_obj[1]]]
      if ("SpatRaster" %in% class(object)){
        r <- raster(object)
      } else if ("RasterLayer" %in% class(object)){
        r <- object
      } else if ("tensorflow.tensor" %in% class(object)){
        r <- tens_to_ras(object)
        if (nlyr(r) == 1){
          r <- raster(r)
        } else{
          names(r) <- c("Uninfected", "Infected")
        }
      }
    }
  } else if (length(plot_obj) == 2){
    object <- sim[[plot_obj[1]]][[plot_obj[2]]]
    if ("SpatRaster" %in% class(object)){
      r <- raster(object)
    } else if ("RasterLayer" %in% class(object)){
      r <- object
    } else if ("tensorflow.tensor" %in% class(object)){
      r <- raster(tens_to_ras(object))
    }
  } else if (length(plot_obj) == 3){
    object <- sim[[plot_obj[1]]][[plot_obj[2]]][[plot_obj[3]]]
    if ("SpatRaster" %in% class(object)){
      r <- raster(object)
    } else if ("RasterLayer" %in% class(object)){
      r <- object
    } else if ("tensorflow.tensor" %in% class(object)){
      r <- raster(tens_to_ras(object))
    }
  }
  
  #browser()
  png_path <- file.path(paste0("png/", G(sim)$plot_extent, "/", G(sim)$simulation_begin_time), paste0("day", stringr::str_pad(time(sim), width=4, side="left", pad=0), ".png"))
  png(png_path, width=2000, height=1000)
  par(ask = FALSE)
  extent <- sim$maps$extents[G(sim)$plot_extent]
  range <- G(sim)$plot_range
  if (is.na(range)){
    #raster::plot(r, main=paste0("Day ", time(sim)), ext=extent[[1]])
    plt(r, extent[[1]], sub=paste0("Day ", time(sim)))
  } else{
  raster::plot(r, zlim=range, main=paste0("Day ", time(sim)), ext=extent[[1]])
  }
  dev.off()
}