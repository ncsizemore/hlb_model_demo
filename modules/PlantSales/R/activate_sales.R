.activate_sales <- function(adults, time, homestead, params){
  # Generate binomial samples of uninfected adults in Homestead.
  # transition those that get infected into infected class
  # once more than 20 are infected, initiate sales.
#browser()
  
  homestead_eq <- tf$equal(homestead, 1)
  #tf$reduce_sum(tf$where(homestead_eq, infected, 0))
  
  counts <- tf$cast(tf$math$floor(adults[1,,,]), adults$dtype)
  if (as.logical(tf$reduce_any(tf$math$is_nan(adults)))){
    browser()
  }
 rng <- tf$random$Generator$from_seed(seed = 234L)
  adults_infected <- rng$binomial(shape = adults[1,,,]$shape, counts = counts, probs = tf$constant(0.01, dtype=adults$dtype), dtype=adults$dtype)
  #adults_infected <- tf$random$stateless_binomial(shape = adults[1,,,]$shape, seed = tf$constant(123L,shape=shape(2L)), counts = counts, probs = tf$constant(0.01, dtype=adults$dtype))
  #  rng$binomial(shape = shape(3L), counts = tf$constant(c(1, 2, 3), dtype=tf$float64), probs = tf$constant(0.01, dtype=tf$float64), dtype=adults$dtype)
  adults_infected_homestead <- tf$expand_dims(tf$where(tf$expand_dims(homestead_eq, -1L), adults_infected, 0L), 0L)
  
  # swap the ifs to test sales:
  #if ( as.logical(tf$greater(tf$reduce_sum(adults), 0)) ) {
  if ( as.logical(tf$greater(tf$reduce_sum(adults_infected_homestead), params$activate_sales_threshold)) ) {
    adults[2,,,]$assign(adults[2,,,] + adults_infected_homestead[1,,,])
    adults[1,,,]$assign(adults[1,,,] - adults_infected_homestead[1,,,])
    
    #if ( as.logical(tf$greater(tf$reduce_sum(adults[2,,,]), 20)) ){
      distr_sales_initial_time <- time + 1
      local_sales_initial_time <- time + 1
    #} 
  }  else{
    distr_sales_initial_time <- params$distr_sales_initial_time
    local_sales_initial_time <- params$local_sales_initial_time 
  }
  
  
  return(list(adults, distr_sales_initial_time, local_sales_initial_time))
}

#.activate_sales_tf <- tf$`function`(.activate_sales)

# #if( max(inf[] > 0, na.rm = T)){ # swap if statements to test the local_sales
# if(mean(inf[which(homestead[] == 1)]) > params$activate_sales_threshold){
#   distr_sales_initial_time <- time + 1
#   local_sales_initial_time <- time + 1
# }else{
#   distr_sales_initial_time <- params$distr_sales_initial_time
#   local_sales_initial_time <- params$local_sales_initial_time  
# }