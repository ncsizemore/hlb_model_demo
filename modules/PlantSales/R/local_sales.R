.local_sales <- function(adults, dgs, sim){
    # Simulates number of infected sales by a distribution center, then reallocates them to the neighborhood around one of the centers.
  # ### this will go in local sales:
  
  #browser()

  # do all local poissons in one shot.
  local_sales <- tf$random$poisson(shape(), dgs$infected_stores * dgs$local_rate)

  cell <- tf$expand_dims(tf$cast(tf$where(tf$greater(sim$.mods$UpdateParameters$flatten_tensor(dgs$indicator * local_sales), 0))[,2], tf$int32),0L)
  
  # if cell$shape has all dims not equal to 0, then we have cells to work with and proceed..
  if ( as.logical(tf$reduce_all(tf$not_equal(tf$constant(cell$shape), 0L))) ){

      # select cells which have nonzero sales
      # natural to select via two-dimensional index, but want to slice the distribution array by their numeric index
      idx <- tf$reshape(tf$map_fn(function(x){tf$where(tf$equal(tf$constant(as.integer(dgs$summary$cell-1)), x))}, cell[1,], fn_output_signature=tf$int64), shape(-1L))
      # note that the difference between r and python indexing probably threw off my original method of slice updating citrus region in adults in eg. disperse groves
      distr_to_sample <- tf$gather(dgs$customer_distr, idx)
    
      # check
      #tf$reduce_all(tf$equal(distr_to_sample[2,,],dgs_customer_distr_stack[as.numeric(idx[2]),,]))
      
      # sample according to their distribution to get final location
      rnd <- tfd_one_hot_categorical(logits = tf$math$log(distr_to_sample))
      num_samples <- tf$reduce_max(dgs$indicator * local_sales)
      #num_samples <- 5L
      s <- rnd$sample(num_samples)
    
      # compute number of sales
      sales <- sim$.mods$UpdateParameters$flatten_tensor(local_sales * dgs$indicator)
      
      cell_sales <- tf$gather(tf$squeeze(sales), tf$squeeze(cell)  )
      #cell_sales <- tf$constant(c(1,2,5,3,2,1,4)) #just to test
      # s[1:as.numeric(cell_sales[1]),1,,] 
      # s[1:as.numeric(cell_sales[3]),3,,] # etc
      # 
      # s[tf$range(cell_sales[3], dtype=tf$int32),,,]
      # tf$reduce_sum(s[tf$range(cell_sales[3], dtype=tf$int32),,,], axis=0L)
      
      # r version 
      indicator_of_sales <- tf$stack(map(1:cell$shape[[2]], ~ tf$reduce_sum(s[1:as.integer(cell_sales)[.x],.x,,], axis=0L)), axis=0L)
      
      #sim$plot(indicator_of_sales[3,,])
      
      # would be nice to tflow it...
      #tf$map_fn(function(x){tf$reduce_sum(s[1:as.numeric(cell_sales[3]),3,,], axis=0L)}, cell[1,], fn_output_signature=tf$int64)
      
      psyllids_per_sale <- c("uninf" = 10, "inf" = 10)
      new_psyllids <- tf$expand_dims(tf$stack(list(psyllids_per_sale["uninf"] * tf$cast(tf$reduce_sum(indicator_of_sales, axis = 0L), dtype = adults$dtype), psyllids_per_sale["inf"] * tf$cast(tf$reduce_sum(indicator_of_sales, axis = 0L), dtype = adults$dtype))), -1L)
      
      adults$assign_add(new_psyllids)
  }
  
  # also need to subtract psyllids from the store locations
  
    #browser()

  return(adults)
  
}

# uninf <- uninfected$as.RasterLayer(band=20)
# inf <- infected$as.RasterLayer(band=20)
# 
# # intensity of the process depends on the population around the store
# dgs$intensity <- abs(rnorm(length(dgs$pop_prob), params$.local_sales_intensity, params$.local_sales_intensity/4)) * dgs$pop_prob
# 
# numSales <- rpois(length(dgs$pop_prob), dgs$intensity) * dgs$infected # num sales each dgs would make (multiplied by 0 if the store is not infected)
# 
# receivedSales <- colSums(t(dgs$trans_prob %*% diag(numSales))) # then redistribute these using the trans_prob matrix
# 
# inf_sales <- infected_sales$as.RasterLayer()
# 
# # samples the cells around each dgs
# samp_cells_list <- lapply(lapply(lapply(seq_along(numSales), function(x){sample(1:length(dgs$geometry), numSales[[x]], replace = TRUE, prob = dgs$trans_prob[,x])}), function(x){dgs$local_area[x]}), function(y){lapply(y, function(z){sample(z[,1], size = 1)})})
# samp_cells <- unlist(samp_cells_list)
# samp_cells_list_2 <- lapply(samp_cells_list, unlist)
# 
# 
# dgs$uninfected_psyllids <- uninf[dgs$cell]
# dgs$infected_psyllids <- inf[dgs$cell]
# dgs$inf_rate <- ifelse(dgs$uninfected_psyllids + dgs$infected_psyllids == 0, 0, dgs$infected_psyllids/(dgs$uninfected_psyllids + dgs$infected_psyllids))
# 
# # note that dgs 63 is on an invalid cell...
# #dgs$cell[which(is.na(dgs$inf_rate))]
# #params$landls2$rasterbands[[1]][dgs$cell[which(is.na(dgs$inf_rate))]] <- 1
# 
# # remove psyllids that are transported with plants
# psyllids_per_plant <- 10
# dgs$inf_psyllids_per_plant <- rbinom(length(dgs$inf_rate), psyllids_per_plant, dgs$inf_rate)
# dgs$uninf_psyllids_per_plant <- psyllids_per_plant - dgs$inf_psyllids_per_plant
# 
# for (i in seq_along(samp_cells_list)) {
#   
#   if(!is.null(samp_cells_list_2[[i]])){
#     inf[samp_cells_list_2[[i]]] <- inf[samp_cells_list_2[[i]]] + dgs$inf_psyllids_per_plant[i]
#     uninf[samp_cells_list_2[[i]]] <- uninf[samp_cells_list_2[[i]]] + dgs$uninf_psyllids_per_plant[i]
#     
#     #inf[samp_cells_list_2[[i]]] <- inf[samp_cells_list_2[[i]]] + dgs$inf_psyllids_per_plant
#     #uninf[samp_cells_list_2[[i]]] <- uninf[samp_cells_list_2[[i]]] + dgs$uninf_psyllids_per_plant
#   }
# } 
# 
# uninf[dgs$cell] <- uninf[dgs$cell] - numSales * dgs$uninf_psyllids_per_plant
# inf[dgs$cell] <- inf[dgs$cell] - numSales * dgs$inf_psyllids_per_plant
# 
# infected$rasterbands[[20]] <- raster::as.matrix(inf)
# uninfected$rasterbands[[20]] <- raster::as.matrix(uninf)
# #browser()
# 
# # add new sales to the sampled cells
# if(!is.null(samp_cells)){
#   inf_sales[sort(unique(samp_cells))] <- mapply(sum, inf_sales[sort(unique(samp_cells))], table(samp_cells), na.rm = TRUE)
#   
#   uninfected$rasterbands[[20]][sort(unique(samp_cells))]
#   
#   infected_sales$rasterbands[[1]] <- matrix(values(inf_sales), nrow = nrow(infected_sales$rasterbands[[1]]), ncol = ncol(infected_sales$rasterbands[[1]]), byrow = TRUE)
# }
# 
# infected_sales$rasterbands[[1]] <- ifelse(!is.na(params$landls2$rasterbands[[1]]) & is.na(infected_sales$rasterbands[[1]]), 0, infected_sales$rasterbands[[1]])




###############################################
# 1613507

# local_ind <- lapply(dgs$local_area, function(x){x[,1]})
# 
# lapply(lapply(dgs$local_area, function(x){x[,1]}), function(x){inf_sales[x]})
# 
# inf_sales[[local_ind]]
# 
# lapply(dgs$local_area, function(x){inf_sales[[x]][,1]})
# 
# lapply(seq_along(dgs$local_area), function(x){ inf_sales[dgs$local_area[[x]][,1]]+ receivedSalesPerArea[x]} )
# samp <- sample(1:length(dgs$geometry), numSales[[1]], replace = TRUE, prob = dgs$trans_prob[,1])
# 
# # destinations for sales
# samp_list <- lapply(seq_along(numSales), function(x){sample(1:length(dgs$geometry), numSales[[x]], replace = TRUE, prob = dgs$trans_prob[,x])})
# # then need to place them in the region
# 
# j <- 1 # .. 207
# k <- 1 # sampled receiving dgs
# sample(dgs$local_area[[samp_list[[j]][k]]][,1], size = 1) # square that gets the sale
# lapply(samp_list, function(x){lapply(x, function(y){sample(y[,1], size = 1)})})
# 
