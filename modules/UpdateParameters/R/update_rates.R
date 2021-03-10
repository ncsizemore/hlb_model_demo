.update_rates <- function(adults, pr_psyllid_survival, pr_egg_survival, egg_rate, adj_egg_rate, pr_psyllid_stay, wind_dispersal_prob, cell_capacity, perc_female, sim){
#browser()

  # where are adults less than capacity?
  tens <- tf$expand_dims(tf$less(tf$math$reduce_sum(adults, axis = 0L), cell_capacity), 0L)

  #ras_to_tens(sim$maps$pr_psyllid_survival)
  # pr_psyllid_survival_init <- sim$maps$tensors$citrus * (P(sim)$pr_psyllid_survival["citrus"] - P(sim)$pr_psyllid_survival["urban"]) + ras_to_tens(sim$maps$landls2) * P(sim)$pr_psyllid_survival["urban"]
  pr_psyllid_survival$assign(tf$where(tens, ras_to_tens(sim$maps$pr_psyllid_survival), tf$constant(0.9, dtype = pr_psyllid_survival$dtype)*pr_psyllid_survival))
  
  # pr_egg_survival_init <- sim$maps$tensors$citrus * (P(sim)$pr_egg_survival["citrus"] - P(sim)$pr_egg_survival["urban"]) + ras_to_tens(sim$maps$landls2) * P(sim)$pr_egg_survival["urban"]
  pr_egg_survival$assign(tf$where(tens, ras_to_tens(sim$maps$pr_egg_survival), tf$constant(0.9, dtype = pr_egg_survival$dtype)*pr_egg_survival))
  
  #egg_rate_init <- sim$maps$tensors$citrus * (P(sim)$egg_rate["citrus"] - P(sim)$egg_rate["urban"]) + ras_to_tens(sim$maps$landls2) * P(sim)$egg_rate["urban"]
  egg_rate$assign(tf$where(tens, ras_to_tens(sim$maps$egg_rate), tf$constant(0.9, dtype = egg_rate$dtype)*egg_rate))
  
  # pr_psyllid_stay_init <- sim$maps$tensors$citrus * (P(sim)$pr_psyllid_stay["citrus"] - P(sim)$pr_psyllid_stay["urban"]) + ras_to_tens(sim$maps$landls2) * P(sim)$pr_psyllid_stay["urban"]
  pr_psyllid_stay$assign(tf$where(tens, ras_to_tens(sim$maps$pr_psyllid_stay), tf$constant(0.8, dtype = pr_psyllid_stay$dtype)*pr_psyllid_stay))
  
# multiply the appropriate female percentage by the corresponding portion of parameter stacks 
  adj_egg_rate$assign(tf$stack(
    list((tf$constant(G(sim)$perc_female["uninf"], dtype = sim$maps$tensors$pr_psyllid_stay$dtype) * sim$maps$tensors$pr_psyllid_stay * sim$maps$tensors$egg_rate)[1,,,], 
         (tf$constant(G(sim)$perc_female["inf"], dtype = sim$maps$tensors$pr_psyllid_stay$dtype) * sim$maps$tensors$pr_psyllid_stay * sim$maps$tensors$egg_rate)[2,,,]
    ), axis=0L))
  
  
  
  #mat <- raster::as.matrix(raster((1 - sim$wind[[3]])*(1 - sim$map_params$pr_psyllid_stay)))
  mat <- tf$expand_dims(tf$expand_dims(tf$constant(1, dtype = wind_dispersal_prob$dtype) - wind_dispersal_prob,0L),-1L) * (tf$constant(1, dtype = pr_psyllid_stay$dtype) - pr_psyllid_stay)
  dispersers <- mat * adults
  
  #tf$reduce_any(tf$less(tf$constant(1, dtype = pr_psyllid_stay$dtype) - pr_psyllid_stay,0))
  return(list(pr_psyllid_survival, pr_egg_survival, egg_rate, pr_psyllid_stay, dispersers, adj_egg_rate))
}

.update_rates_tf <- tf$`function`(.update_rates)




#adults_without_nans <- tf$where(tf$math$is_nan(adults), tf$zeros_like(adults), adults)
#total_adults <- as.numeric(tf$math$reduce_sum(adults_without_nans))
#total_adults <- as.matrix(tf$squeeze(tf$math$reduce_sum(adults, axis = 0L)))

# if total psyllids exceeds, threshold, scale down.
# otherwise, return: initial probabilities on corresponding regions (construct individual rasters, then linear combine)
#adult_survival[] <- ifelse(total_adults > params$cell_capacity, 0.5 * raster::as.matrix(adult_survival), raster::as.matrix(adult_survival) )
# values(adult_survival)[] <- ifelse(total_adults > params$cell_capacity, 0.5 * values(adult_survival), values(adult_survival) )
# adult_survival <- tf$broadcasto(tf$expand_dims(tf$expand_dims(tf$constant(raster::as.matrix(raster(adult_survival))),0L),-1L), adults$shape)
# 
# values(egg_survival)[] <- ifelse(total_adults > params$cell_capacity, 0.25 * values(egg_survival), values(egg_survival) )
# egg_survival <- tf$broadcasto(tf$expand_dims(tf$expand_dims(tf$constant(raster::as.matrix(raster(egg_survival))),0L),-1L), developing$shape)
# 
# values(egg_rate)[] <- ifelse(total_adults > params$cell_capacity, 0.25 * values(egg_rate), values(egg_rate) )
# egg_rate <- tf$broadcasto(tf$expand_dims(tf$expand_dims(tf$constant(raster::as.matrix(raster(egg_rate))),0L),-1L), developing$shape)

# adj_egg_rate_1 <- tf$constant(raster::as.matrix(raster(params$pr_psyllid_stay * params$perc_female * params$egg_rate)))
# adj_egg_rate_2 <- tf$constant(raster::as.matrix(raster(params$pr_psyllid_stay * params$perc_female * params$egg_rate)))
# 
# adj_egg_rate <- tf$expand_dims(tf$stack(c(adj_egg_rate_1, adj_egg_rate_2)), axis = -1L)
