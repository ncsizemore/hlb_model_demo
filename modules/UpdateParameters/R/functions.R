maps <- purrr::map(qs::qread(paste0(simPaths$inputPath, "maps_qs.qs")), 
                   ~ if(class(.x) == "PackedSpatRaster"){terra::rast(.x)} else {.x} )

tens_to_ras <- function(tensor){
  # sim <- mySim
  #tensor <- maps$pr_psyllid_survival_T
  # maps$citrus
  #browser()
  tensor_shape <- as.numeric(tensor$get_shape()$as_list()) # as.numeric(tf$constant(tensor$shape))
  if (length(tensor_shape) == 4){
    if (tensor_shape[2] == 1863){
      r <- maps$land
    } else if (tensor_shape[2] == 808){
      r <- maps$citrus_cropped
    } else if (tensor_shape[2] == 6464){
      r <- maps$citrus_cropped_hres
    } else if (tensor_shape[2] == 233){
      r <- maps$citrus_harvesting_locations
    } 
    #browser()
    ras_lst <- as.list(1:tensor$shape[[1]])
    ras_lst <- map(ras_lst, ~ r)
    m_lst <- map(1:tensor$shape[[1]], ~ as.matrix(tensor[.x,,,1]))
    ras <- rast(map2(ras_lst, m_lst, ~ setValues(.x, .y)))
    ras[which(is.na(r[]))] <- NA
    
  } else if (length(tensor_shape) == 2){
    if (tensor_shape[1] == 1863){
      r <- maps$land
    } else if (tensor_shape[1] == 808){
      r <- maps$citrus_cropped
    } else if (tensor_shape[1] == 6464){
      r <- maps$citrus_cropped_hres
    } 
    ras <- r
    m <- as.matrix(tensor)
    ras <- setValues(ras, m)
    ras[which(is.na(r[]))] <- NA
  }
  return(ras)
}

ras_to_tens <- function(ras){
  if (class(ras) == "SpatRaster"){
    if (nlyr(ras) == 1){
      m <- terra::as.matrix(ras, wide=TRUE)
      m[is.na(m)] <- 0
      tensor <- tf$constant(m, dtype = tf$float32)
    } else if (nlyr(ras) == 2){
      m1 <- terra::as.matrix(ras[[1]], wide=TRUE)
      m1[is.na(m1)] <- 0
      m2 <- terra::as.matrix(ras[[2]], wide=TRUE)
      m2[is.na(m2)] <- 0
      tensor <- tf$expand_dims(tf$stack(
        c(tf$constant(m1, dtype = tf$float32), 
          tf$constant(m2, dtype = tf$float32)), axis = 0L), axis = -1L)
    }
  } else if (class(ras) == "RasterLayer"){
    m <- raster::as.matrix(ras)
    m[is.na(m)] <- 0
    tensor <- tf$constant(m, dtype = tf$float32)
  }
  return(tensor)
}

mat_to_ras <- function(mat,...){
  ras <- maps$land
  ras <- setValues(ras, mat)
  ras[maps$na_cells] <- NA
  return(ras)
}

prob_mat <- function(cell){
  rast <- maps$landls2
  idx <- terra::rowColFromCell(rast, cell) 
  i <- idx[1]; j <- idx[2]
  mat <- terra::as.matrix(rast, wide=TRUE)
  dist_mat <- pmax(abs(row(mat) - i), abs(col(mat) - j))
  prob_mat <- exp(-dist_mat)
  prob_mat <- prob_mat * terra::as.matrix(rast, wide=TRUE)
  prob_mat <- prob_mat/sum(prob_mat, na.rm = TRUE)
  prob_mat[is.na(prob_mat[])] <- 0
  return(prob_mat)
}

prob_ras <- function(cell){
  mat <- prob_mat(cell)
  ras <- mat_to_ras(mat)
  return(ras)
}

flatten_tensor <- function(tensor){
  tensor_shape <- as.numeric(tf$constant(tensor$shape))
  
  if (length(tensor_shape) == 2) {
    tensor <- tf$expand_dims(tf$reshape(tensor, shape = -1L),0L)
  }
  
  return(tensor)
}

unflatten_tensor <- function(flat, original){
  return(tf$reshape(flat, shape=original$shape))
}


plt <- function(object,extent=NA, ...){
  if ("SpatRaster" %in% class(object)){
    if (class(extent) %in% c("Extent", "SpatExtent")){
      terra::plot(crop(object, ext(extent)), ...)
    } else{
      terra::plot(object, ...)
    }
  } else if ("RasterLayer" %in% class(object)){
    if (class(extent) %in% c("Extent", "SpatExtent")){
      raster::plot(crop(object, extent(extent)), ...)
    } else{
      raster::plot(object, ...)
    }
  } else if ("tensorflow.tensor" %in% class(object)){
    rast <- tens_to_ras(object)
    if (class(extent) %in% c("Extent", "SpatExtent")){
      terra::plot(crop(rast, ext(extent)), ...)
    } else{
      terra::plot(rast, ...)
    }
  }
}