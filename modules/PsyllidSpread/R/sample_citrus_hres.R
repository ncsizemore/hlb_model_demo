.sample_citrus_cropped_hres <- function(adults, adults_cropped_current_hres, citrus_rows, citrus_cols, citrus_cropped_hres, citrus_cropped_hres_pr, psyllids_to_add, sim){

   # browser()
    #sample
    sample <- tf$random$categorical(tf$math$log(citrus_cropped_hres_pr), 1L)
    # ## check sampling::
    # tf$gather(citrus_cropped_hres_pr_T[1,], sample)
    # tf$gather_nd(citrus_cropped_hres_pr_T[1,], tf$reshape(sample, shape=shape(1,1)))

    # update probability to 0
    pr_update <- tf$expand_dims(tf$tensor_scatter_nd_update(citrus_cropped_hres_pr[1,], tf$transpose(sample), tf$constant(0, shape=shape(1L), dtype=citrus_cropped_hres_pr$dtype)),0L)
    citrus_cropped_hres_pr$assign(pr_update)
    # # check
    # tf$gather_nd(tf$squeeze(citrus_cropped_hres_pr), sample)
    
    
    # assign new sampled arrivals
    
    
    # create indicator vector for psyllids to insert into cropped area
    psyllids_to_add_T <- tf$scatter_nd(tf$transpose(sample), tf$constant(1, shape=shape(1L), dtype=citrus_cropped_hres_pr$dtype), citrus_cropped_hres_pr[1,]$shape )
    
    psyllids_to_add_unflat <- sim$.mods$UpdateParameters$unflatten_tensor(psyllids_to_add_T, citrus_cropped_hres)
    
    # finish and assign to previous
    adults_cropped_prev_hres <- adults_cropped_current_hres + tf$stack(c(psyllids_to_add["uninf"] * psyllids_to_add_unflat[1,,,], psyllids_to_add["inf"] * psyllids_to_add_unflat[1,,,]), axis=0L)
    
    #sim$.mods$UpdateParameters$tens_to_ras( (tf$stack(c(psyllids_to_add["uninf"] * psyllids_to_add_unflat[1,,,], psyllids_to_add["inf"] * psyllids_to_add_unflat[1,,,]), axis=0L))[2,,,1])
    
    # then pool back to adults_cropped
    adults_cropped_prev <- tf$constant(8*5, dtype=adults$dtype) * tf$nn$avg_pool2d(adults_cropped_prev_hres, ksize = c(8L,5L), strides = c(8L,5L), padding = "SAME")
    # get percentages:
    #adults_cropped_perc_hres$assign(tf$math$divide_no_nan(adults_cropped_hres, tf$keras$layers$UpSampling2D(size=c(8L,5L), dtype=adults$dtype)(adults_cropped)))
    
    # insert back into adults full
    # indices <- tf$constant(as.matrix(expand.grid(0:1, citrus_rows, citrus_cols, 0)), dtype=tf$int32)
    # adults$scatter_nd_update(indices, tf$reshape(adults_cropped_prev, shape(-1L)))
    
    adults[,citrus_rows,citrus_cols,]$assign(adults_cropped_prev)

   # browser()
    
    return(list(adults, adults_cropped_prev, adults_cropped_prev_hres, citrus_cropped_hres_pr))

}

.sample_citrus_cropped_hres_tf <- tf$`function`(.sample_citrus_cropped_hres)


# # restore hres config
# browser()
# adults_cropped <- adults[,citrus_rows,citrus_cols,]
# 
# new_adults_cropped_indicator <- tf$cast(tf$where(tf$not_equal(adults_cropped, 0), 1, 0),dtype=adults_cropped_indicator$dtype) - adults_cropped_indicator # 1 only on newly occupied large square
# 
# new_adults_cropped <- adults_cropped * new_adults_cropped_indicator # gets adults only on those cells
# 
# # upsample current adults
# adults_cropped_hres <- tf$keras$layers$UpSampling2D(size=c(8L,5L), dtype=adults_cropped_perc_hres$dtype)(adults_cropped) * adults_cropped_perc_hres
# 
# ncells <- tf$constant(8 * 5, dtype = new_adults_cropped$dtype)
# new_adults_cropped_hres <- tf$keras$layers$UpSampling2D(size=c(8L,5L), dtype = new_adults_cropped$dtype)(new_adults_cropped)/ncells
# 
# # take current population, upsample, multiply by percentages of population within each occupied 
# restored <- adults_cropped_hres + new_adults_cropped_hres
# ### end restore