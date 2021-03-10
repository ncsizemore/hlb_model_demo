.lay_eggs <- function(adults, developing, uninf_ind, inf_ind, params){
  #browser()
  
  # All homestead cells are infected
  # Outside homestead, cells are uninfected if they have less than 1 infected psyllid
  # Uninfected cells generate only uninfected eggs
  # Infected cells generate pr_egg_infection% of their eggs as infected eggs
  
 # uninfected[[1]] <- params$.pr_psyllid_survival$as.RasterLayer() * params$.pr_psyllid_stay$as.RasterLayer() * params$.perc_female * params$.egg_rate$as.RasterLayer() * (uninfected[[20]] + infected[[20]]) * uninf_ind$as.RasterLayer()
  
  #infected[[1]] <- params$.pr_psyllid_survival$as.RasterLayer() * params$.pr_psyllid_stay$as.RasterLayer() * params$.perc_female * params$.egg_rate$as.RasterLayer() * (uninfected[[20]] + infected[[20]]) * inf_ind$as.RasterLayer()
  
  new_eggs <- adults * params$adj_egg_rate_T
  
  # developing_sub <- developing[,,,2:19]
  # developing <- tf$concat(c(new_eggs, developing_sub), axis = -1L)
  
  developing[,,,1]$assign(tf$squeeze(new_eggs))
  
  return(developing)
}

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
#   adults_var$assign_add(tf$expand_dims(dev_var[,,,19], 3L)),
#   tf$expand_dims((adults + developing)[,,,params$developmental_days], -1L), check = FALSE, min_time = 5
# )