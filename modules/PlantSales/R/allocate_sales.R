.allocate_sales <- function(adults, dgs_rates, dgs_infected_stores, psyllids_per_plant){
 #browser()
  # sample
  sales_samples <- tf$random$poisson(shape(), dgs_rates)

  tmp <- tf$expand_dims(sales_samples * psyllids_per_plant,-1L)
  
  new_psyllids_via_sales <- tf$cast(tf$stack(list(tmp, tmp), axis = 0L), dtype=adults$dtype)
  adults$assign_add(new_psyllids_via_sales)
  
  
  # update infected stores
  dgs_infected_stores$assign(tf$where(tf$not_equal(sales_samples, 0), tf$constant(1, shape=shape(1L), dtype=dgs_infected_stores$dtype), dgs_infected_stores))
  
  
  return(list(adults, dgs_infected_stores))
}

.allocate_sales_tf <- tf$`function`(.allocate_sales)