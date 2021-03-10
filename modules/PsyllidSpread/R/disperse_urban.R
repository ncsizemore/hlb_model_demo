.disperse_urban <- function(adults, weight_matrix, dispersers, pr_psyllid_stay){
 # browser()
  
  if (as.logical(tf$reduce_any(tf$less(adults,0)))){
    browser()
  }
    k <- tf$expand_dims(tf$expand_dims(weight_matrix, -1L),-1L)
    
    # should be dispersers instead of adults
    conv <- tf$nn$conv2d(dispersers, k, strides = shape(1,1,1,1), padding = "SAME")
    
    adults$assign(pr_psyllid_stay * adults)

    adults$assign_add(conv)
    
    if (as.logical(tf$reduce_any(tf$less(adults,0)))){
      browser()
    }

    
    # comparison <- tf$math$greater(adults, tf$constant(1, dtype=adults$dtype))
    # adults$assign(tf$where(comparison, adults, 0))
    

  
  return(adults)
}

.disperse_urban_tf <- tf$`function`(.disperse_urban)
