.progress_eggs <- function(adults, developing, adj_egg_rate, developmental_days, egg_transmission_threshold, pr_egg_infection){
#browser()

  # new <- (adults + developing)[,,,params$developmental_days]
  # adults <- tf$expand_dims(new, -1L)
  # 
  adults$assign_add(tf$expand_dims(developing[,,,developmental_days], 3L))

  developing$assign(tf$roll(developing, shift = 1L, axis = -1L))
  
  new_eggs <- adults * adj_egg_rate # note that adj_egg_rate (updated within update_rates.R) already reflects perc_female, pr_stay, etc.
  
  # now check if any cells are above threshold for egg_transmission
  above_threshold_check <- tf$greater(adults[2,,,1], egg_transmission_threshold)
  if (as.logical(tf$reduce_any(above_threshold_check))){
    
    # if so, leave 1 - pr_egg_inf of new eggs as uninfected
    # and move pr_egg_inf new eggs to infected
      new_eggs_infected_cells <- tf$stack(list((1 - pr_egg_infection) * new_eggs[1,,,], 
                    new_eggs[2,,,] + pr_egg_infection * new_eggs[1,,,]),
               axis = 0L)

      new_eggs <- tf$where(tf$expand_dims(tf$expand_dims(above_threshold_check, 0L), -1L),
                           new_eggs_infected_cells, # stuff for infected cells
                           new_eggs)
  
  }
  
  developing[,,,1]$assign(tf$squeeze(new_eggs))

  return(list(adults, developing))
  
}

.progress_eggs_tf <- tf$`function`(.progress_eggs)

# library(bench)
# 
# dev_var <- tf$Variable(developing)
# adults <- adults[,,,1]
# adults <- tf$expand_dims(adults, 3L)
# adults_var <- tf$Variable(adults)
# 
# adults_var
# dev_var[,,,params$developmental_days]
# 
# mark(
#   dev_var[,,,2:19]$assign(dev_var[,,,1:18]),
#   tf$roll(dev_var, shift = 1L, axis = -1L),
#   dev_var$assign(tf$roll(developing, shift = 1L, axis = -1L)),
#   check = FALSE, min_time = 5
# )

