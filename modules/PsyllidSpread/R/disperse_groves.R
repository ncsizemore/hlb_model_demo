.disperse_groves <- function(adults, adults_cropped_current_hres, adults_cropped_duration_hres, weight_matrix, citrus_rows, citrus_cols, pr_psyllid_stay_cropped_hres, cycle_length, sim){
 # browser()
  
  # raster::plot(raster(sim$acphr), ext=sim$maps$extents$citrus_zoom_1_tight)
  # sim$acphr[sim$sampled_valid]
  # 
  # raster::plot(raster(sim$functions$tens_to_ras(adults_cropped_current_hres)), ext=sim$maps$extents$citrus_zoom_1_tight)
  # raster::plot(raster(sim$functions$tens_to_ras(adults_cropped_prev_hres)), ext=sim$maps$extents$citrus_zoom_1_tight)
  
  # filter on those who have duration equiv to 85 mod 0.
  # this determines those that we need to disperse at grove-scale
  active_logical <- tf$logical_and(tf$equal(tf$math$floormod(adults_cropped_duration_hres, cycle_length), 0), tf$greater(adults_cropped_duration_hres, 0))
  
  # if there are any of those...
   if ( as.logical(tf$reduce_any(active_logical)) ){
          # if any active 
            active_dispersers <- tf$cast(tf$where(active_logical, 1, 0), dtype=adults$dtype)
            
            # dispersers should only be those that have adults_cropped_duration_hres == 0 mod 85
            dispersers <- (1 - pr_psyllid_stay_cropped_hres) * adults_cropped_current_hres * active_dispersers
            
            # disperse with convolution
            k <- tf$expand_dims(tf$expand_dims(weight_matrix, -1L),-1L)
            conv <- tf$nn$conv2d(dispersers, k, strides = shape(1,1,1,1), padding = "SAME")
            
            #adults_hres$assign(pr_psyllid_stay_hres_T * adults_hres)
            adults_cropped_prev_hres <- pr_psyllid_stay_cropped_hres * adults_cropped_current_hres + conv
            
            # then pool back to adults
            adults_cropped_prev <- tf$constant(8*5, dtype=adults$dtype) * tf$nn$avg_pool2d(adults_cropped_prev_hres, ksize = c(8L,5L), strides = c(8L,5L), padding = "SAME")
            #adults_cropped_perc_hres$assign(tf$math$divide_no_nan(adults_cropped_hres,tf$keras$layers$UpSampling2D(size=c(8L,5L), dtype=adults$dtype)(adults_cropped)))
            
            #indices <- tf$constant(as.matrix(expand.grid(0:1, citrus_rows, citrus_cols, 0)), dtype=tf$int32)
            #adults$scatter_nd_update(indices, tf$reshape(adults_cropped_prev, shape(-1L)))
            adults[,citrus_rows,citrus_cols,]$assign(adults_cropped_prev)
  
     } else {
            adults_cropped_prev <- adults[,citrus_rows,citrus_cols,]
            adults_cropped_prev_hres <- adults_cropped_current_hres
     }
  
  # where the duration is 0 but the population is nonzero - this is not needed because we update the duration during advance date
  adults_cropped_duration_hres$assign(tf$where(tf$logical_and(tf$equal(adults_cropped_duration_hres, 0), tf$not_equal(adults_cropped_prev_hres,0)), 1, adults_cropped_duration_hres))

  return(list(adults, adults_cropped_prev, adults_cropped_prev_hres))
}

.disperse_groves_tf <- tf$`function`(.disperse_groves)



#############################

# adults_cropped <- adults[,citrus_rows,citrus_cols,]
# #sim$plot(adults_cropped)
# 
# #sim$plot(tf$cast(tf$where(tf$not_equal(adults_cropped, 0), 1, 0),dtype=adults_cropped_indicator$dtype))
# new_adults_cropped_indicator <- tf$cast(tf$where(tf$not_equal(adults_cropped, 0), 1, 0),dtype=adults_cropped_indicator$dtype) - adults_cropped_indicator # 1 only on newly occupied large square
# #sim$plot(new_adults_cropped_indicator)
# 
# new_adults_cropped <- adults_cropped * new_adults_cropped_indicator # gets adults only on those cells
# #sim$plot(new_adults_cropped)
# 
# # upsample current adults
# adults_cropped_hres <- tf$keras$layers$UpSampling2D(size=c(8L,5L), dtype=adults_cropped_perc_hres$dtype)(adults_cropped) * adults_cropped_perc_hres
# #sim$plot(adults_cropped_hres)
# 
# ncells <- tf$constant(8 * 5, dtype = new_adults_cropped$dtype)
# new_adults_cropped_hres <- tf$keras$layers$UpSampling2D(size=c(8L,5L), dtype = new_adults_cropped$dtype)(new_adults_cropped)/ncells
# #sim$plot(new_adults_cropped_hres)
# 
# # take current population, upsample, multiply by percentages of population within each occupied 
# restored <- adults_cropped_hres + new_adults_cropped_hres

#adults_cropped_current_hres <- updated_adults_cropped_hres(sim)

####


#browser()

#######
#### update arrivals and probabilities
# neq <- tf$not_equal(new_adults_cropped_indicator, 0)
# citrus_hres_arrivals_T$assign(tf$where(neq, tf$constant(1, dtype = citrus_hres_arrivals_T$dtype), citrus_hres_arrivals_T))
# # set probability to zero on indices
# new_arrivals_flat <- tf$expand_dims(tf$reshape(tf$expand_dims(tf$math$reduce_sum(citrus_hres_arrivals_T, axis = 0L), 0L), shape=shape(-1L)), 0L) 
# neq_a <- tf$not_equal(new_arrivals_flat, 0) # if arrivals not equal 0, probability should be 0
# citrus_hres_flat_pr_T$assign(tf$where(neq_a, tf$constant(0, dtype = citrus_hres_flat_pr_T$dtype), citrus_hres_flat_pr_T))



########################

# 
# # two copies of flat citrus hires. one specifies probabilities of initiating arrival into that acre; the other the ones that have been arrived to.  diffing the upscaled new adult population with previous upscaled gives new natural arrivals.  we assign those equally to all cells.  then multiply the new upscaled adults with previous small scale to reconstitute previous small scales.  then do the assignment from the last sentence. then spread. then do a pooling to go back to large scale.
# 
# extent <- extentFromCells(r, which(r[] == 1000)[c(1,3)])
# cropped_citrus <- raster::crop(r, extent)
# d <- terra::disaggregate(cropped_citrus, c(8,5))
# # disaggregate population. multiply by previous small scale spread, giving version with current population on previously spread-to small squares.  divide by number of occupied small squares (or redistribute some other way). insert new infections (place an appropriate probability distribution on the small squares. assume new infected arrivals to are poisson distributed in time; we then place them according to the chosen distribution). spread via convolution. aggregate to get populations into larger squares.  aggregate to get number of squares occupied. 
# d_agg <- terra::aggregate(d,c(8,5),"modal")
# d_agg
# cropped_citrus
# terra::plot(cropped_citrus)
# terra::plot(d_agg)
# 
# k <- tf$expand_dims(tf$expand_dims(weight_matrix, -1L),-1L)
# 
# conv <- tf$nn$conv2d(adults, k, strides = shape(1,1,1,1), padding = "SAME")
# 
# adults$assign(tf$expand_dims(tf$expand_dims(pr_psyllid_stay_T, 0L),-1L) * adults)
# adults$assign_add(conv)
# 
# comparison <- tf$math$greater(adults, tf$constant(1, dtype=adults$dtype))
# adults$assign(tf$where(comparison, adults, 0))