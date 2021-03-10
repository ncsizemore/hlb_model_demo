.disperse_from_trucks <- function(adults, truck_schedule, paths, harvest_season, citr_polys_cells, date, sim){
    
    #browser()
    day_of_season <- date - harvest_season[1]
    #day_of_season <- 10
    
    trucks_today <- unlist(map(seq_along(truck_schedule), ~ length(which(truck_schedule[[.x]] == day_of_season))))
    
    all_routes <- sim$maps$routes$citrus_harvesting_locations_to_processing
    sample_all_routes <- sample(1:10, length(all_routes), replace=TRUE)
    
    selected_routes <- dplyr::bind_rows(map(seq_along(all_routes), ~ all_routes[[.x]][[sample_all_routes[.x]]]))

    sample <- st_line_sample(st_transform(selected_routes, crs(sim$maps$citrus)), n = ceiling(P(sim)$road_dispersal_density * st_length(selected_routes)), type = "random")
    
    #sample <- st_line_sample(st_transform(selected_routes, crs(sim$maps$citrus)), n = ceiling(1/50000 * st_length(selected_routes)), type = "random")
    #saveRDS(sample, "../ncsizemore.github.io_hugo.files/R/sample.rds")

    #leaflet::leaflet(st_transform(sample, crs = 4326) %>% st_cast("POINT") %>% as("Spatial")) %>% leaflet.mapboxgl::addMapboxGL(style = "mapbox://styles/mapbox/outdoors-v11", accessToken = "pk.eyJ1IjoibmNzaXplbW9yZSIsImEiOiJjazVweGhua3UwYWg2M2Ryem1iazllMzQzIn0.UnEMbbyS2sg-ygD8saJqQQ", setView = FALSE) %>%  leaflet::addCircleMarkers(weight = 1, fillOpacity = 0, radius = 3) %>% leaflet::setView(-81, 27, zoom = 8)
    
    
    adults_pooled <- tf$constant(8*8, dtype=adults$dtype) * tf$nn$avg_pool2d(adults, ksize = c(8L,8L), strides = c(8L,8L), padding = "SAME")

    ras <- raster(sim$maps$citrus)
    cells <- map(seq_along(all_routes), ~ tabularaster::cellnumbers(ras, sample[.x,])$cell_)

    # slice adults_pooled at all harvesting location indices [0/1,x,y,0] where (x,y)
    # is a harvesting location in the pooled grid
    # we scale by percentage of psyllids each truck takes and number of trucks 
    # (note that adults pooled has uninfected and infected both)
    # so this contains the psyllids removed from the harvesting locations as a vector [uninf, inf]
    adults_removed_harvesting_locations <- P(sim)$psyllids_per_truck_percentage * rep(trucks_today, 2) * 
      as.numeric(tf$gather_nd(adults_pooled, sim$maps$citrus_harvesting_locations_tensor_indices))

    l <- map(seq_along(cells), ~ rep(adults_removed_harvesting_locations[.x], length(cells[[.x]])) )
    tb <- tibble::tibble(cell = unlist(cells), psyllids = unlist(l))
    tb_sum <- tb %>% dplyr::group_by(cell) %>% dplyr::summarise(tot = sum(psyllids, na.rm = TRUE))
    # tb_sum contains the number of new psyllids in each cell
    adults_flat <- sim$.mods$UpdateParameters$flatten_tensor(adults[1,,,1])
    ind <- tf$pad(tf$expand_dims(tf$constant(as.integer(tb_sum$cell), dtype = tf$int32), axis=1L),tf$constant(matrix(c(c(0L, 1L), c(0L, 0L)), ncol = 2)))
    update <- tf$gather_nd(adults_flat, ind)
    new <- tf$scatter_nd(ind, update, adults_flat$shape)
    adults[1,,,1]$assign(sim$.mods$UpdateParameters$unflatten_tensor(new, adults[1,,,1]) + adults[1,,,1])
    #adults_rast1[tb_sum$cell] <- adults_rast1[tb_sum$cell] + tb_sum$tot
    
    l <- map(seq_along(cells), ~ rep(adults_removed_harvesting_locations[.x + length(seq_along(cells))], length(cells[[.x]])) )
    tb <- tibble::tibble(cell = unlist(cells), psyllids = unlist(l))
    tb_sum <- tb %>% dplyr::group_by(cell) %>% dplyr::summarise(tot = sum(psyllids, na.rm = TRUE))
    # tb_sum contains the number of new psyllids in each cell
    adults_flat <- sim$.mods$UpdateParameters$flatten_tensor(adults[2,,,1])
    ind <- tf$pad(tf$expand_dims(tf$constant(as.integer(tb_sum$cell), dtype = tf$int32), axis=1L),tf$constant(matrix(c(c(0L, 1L), c(0L, 0L)), ncol = 2)))
    update <- tf$gather_nd(adults_flat, ind)
    new <- tf$scatter_nd(ind, update, adults_flat$shape)
    adults[2,,,1]$assign(sim$.mods$UpdateParameters$unflatten_tensor(new, adults[2,,,1]) + adults[2,,,1])
    
    # etc etc.
    
    #mapview::mapview(sample[640,])
    
    # uninf <- uninfected$as.RasterLayer(band=20)
    # inf <- infected$as.RasterLayer(band=20)
    # total <- uninf + inf
    # 
    # uninf_perc <- uninf/total
    # uninf_perc[is.na(uninf_perc) & !is.na(uninf)] <- uninf[is.na(uninf_perc) & !is.na(uninf)]
    # inf_perc <- inf/total
    # inf_perc[is.na(inf_perc) & !is.na(inf)] <- inf[is.na(inf_perc) & !is.na(inf)]
    
    # total <- tf$reduce_sum(adults, axis = 0L)
    # sim$.mods$UpdateParameters$ras_to_tens(sim$maps$harvesting_locations)
    # 
    # psyllids_per_truck <- min(0.01*total, (2500/64) * sim$maps$harvesting_locations)
    # 
    # uninf_per_truck <- psyllids_per_truck * uninf_perc
    # inf_per_truck <- psyllids_per_truck * inf_perc
    
  # remove psyllids from harvesting locations
    # for (i in 1:560){
    #   uninf[ citr_polys_cells[[i]] ] <- uninf[ citr_polys_cells[[i]] ] - uninf_per_truck[ citr_polys_cells[[i]] ]*trucks_today[i]
    #   inf[ citr_polys_cells[[i]] ] <- inf[ citr_polys_cells[[i]] ] - inf_per_truck[ citr_polys_cells[[i]] ]*trucks_today[i]
    # }
    
    # uninf_dt <- as.data.table.raster(uninf)
    # inf_dt <- as.data.table.raster(uninf)
    # uninf_list <- as.list(uninf[])
    # inf_list <- as.list(inf[])
    # lapply(1:560, function(i){
    #   cells <- citr_polys_cells[[i]]
    #   l1 <- uninf[ cells ] - uninf_per_truck[ cells ]*trucks_today[i]
    #   l2 <- inf[ cells ] - inf_per_truck[ cells ]*trucks_today[i]
    #   uninf_list[ cells ] <<- l1
    #   inf_list[cells] <<- l2
    #   #return(list(l1,l2))
    #   
    # })
    # 
    # uninf[] <- unlist(uninf_list)
    # inf[] <- unlist(inf_list)
    # 
    
    
    
    # for (i in 1:560) set(uninf_dt,citr_polys_cells[[i]],1L,lst[[i]][[1]])
    # for (i in 1:560) set(inf_dt,citr_polys_cells[[i]],1L,lst[[i]][[2]])
    # uninf_dt[citr_polys_cells[[i]], layer := l1 ] 
    # inf_dt[citr_polys_cells[[i]], layer := l2 ]
  
    
    # for (i in 1:560){
    # uninf[ citr_polys_cells[[i]] ] <- lst[[i]][[1]]
    # inf[ citr_polys_cells[[i]] ] <- lst[[i]][[2]]
    # }
    
            # place them uniformly along path

    # lapply(1:560, function(i){
    #   dest <- sample(1:10, 1) # random choice of destination
    #   cells <- citr_polys_cells[[i]] # origin cells
    #   path_cells <- paths[[i]][[dest]] # path cells
    #   
    #   not_na_path_cells <- !is.na(uninf[path_cells])
    #   
    #   # uninf[ path_cells][ not_na_path_cells]
    #   # inf[ path_cells][ not_na_path_cells]
    #   
    #   l1 <- sum(uninf_per_truck[ cells ]*trucks_today[i])/length(not_na_path_cells)
    #   l2 <- sum(inf_per_truck[ cells ]*trucks_today[i])/length(not_na_path_cells)
    #   
    #   uninf_list[ path_cells ][not_na_path_cells] <<- as.list(unlist(uninf_list[ path_cells ][not_na_path_cells]) + l1)
    #   inf_list[ path_cells ][not_na_path_cells] <<- as.list(unlist(inf_list[ path_cells ][not_na_path_cells]) + l2)
    #   #inf_list[cells] <<- l2
    #   #return(list(l1,l2))
    #   
    # })
    # 
    # uninf[] <- unlist(uninf_list)
    # inf[] <- unlist(inf_list)
    # 
    # uninfected$rasterbands[[20]] <- raster::as.matrix(uninf)
    # infected$rasterbands[[20]] <- raster::as.matrix(inf)
    
    # uninf[ paths[[1]][[5]] ][!is.na(uninf[ paths[[1]][[5]] ])] <- 
    #   sum(uninf_per_truck[ citr_polys_cells[[1]] ]*trucks_today[1])/length(!is.na(uninf[ paths[[1]][[5]] ]))
    # inf[ paths[[1]][[5]] ][!is.na(inf[ paths[[1]][[5]] ])] <- 
    #   sum(inf_per_truck[ citr_polys_cells[[1]] ]*trucks_today[1])/length(!is.na(inf[ paths[[1]][[5]] ]))

    
    #mySim$uninfected$as.RasterLayer(band = 20)
    
  
  
  # if(past end of sesason){
  #   harvest_season <- harvest_season + years(1)
  # }
  
  return(list(adults, harvest_season))
}


