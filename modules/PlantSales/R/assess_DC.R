.assess_DC <- function(dgs, params){
  
  # If all the dgs are not infected,
  if (sum(dgs$infected) < length(dgs$infected)){
    # sum the first column of the sales matrix for each of them. if > threshold, make it infected.
    dgs$infected <- ifelse(sapply(dgs$sales, function(x){sum(x[,1], na.rm = TRUE)}) > params$.distr_inf_threshold, 1, dgs$infected)
  }
  
  return(dgs)
}