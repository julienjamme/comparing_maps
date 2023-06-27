#' From matrix of coordinates and weights to raster maps
#'
#' @param coordinates matrix of coordinates (two columns and N rows) for x,y of the raster
#' @param weights matrix of weights for the z values of the raster (at least one column and N rows)
#' if there are several columns, all of them will be available
#'
#' @return raster object
#'
#' @examples
#' data(meuse.grid, package = "sp")
#' coordinates <- as.matrix(meuse.grid[,1:2])
#' weights <- meuse.grid[,"dist"]
#' matrix_to_maps(coordinates, weights) |> plot()
matrix_to_maps <- function(coordinates, weights){
  
  xyz <- cbind(coordinates, weights)
  r <- rasterFromXYZ(xyz = xyz)
  
  
}


from_INSPIRE_to_xy <- function(inspire_coords, crs = "3035", res = "1000m"){
  edge <- as.numeric(gsub("[a-z]*", "", res, perl = TRUE))
  rad <- paste0("CRS",crs,"RES",res)
  xy_coords <- gsub(rad, "", inspire_coords)
  x_coords <- gsub("^N.*E", "", xy_coords, perl = TRUE)
  y_coords <- gsub("(^N|E.*$)", "", xy_coords, perl = TRUE)
  return(cbind(x = as.numeric(x_coords)+edge/2, y = as.numeric(y_coords)+edge/2))
}


from_matrix_to_column <- function(mat) purrr::reduce(mat, rbind)


#' Compute the RMSE between one reference and several estimates
#'
#' @param weights : first col is the reference
#'
#' @return vector of rmses measured between the first column as reference and
#'  each of the others
#'
#' @examples
#' data(meuse.grid, package = "sp")
#' weights <- cbind(
#'   meuse.grid[,"dist"] + 10,
#'   meuse.grid[,"dist"] + 10 + rnorm(nrow(meuse.grid), 0, 1)
#' )
#' rmse(weights)
rmse <- function(weights){
  if(ncol(weights) == 1){
    return(NA)
  }else{
    sapply(2:(ncol(weights)),
           function(c){
             sqdiff <- (weights[,1]-weights[,c])^2
             return(sqrt(mean(sqdiff)))
           }
    )
  }
}

#' Compute the Hellinger distance between one reference and several estimates
#'
#' @param weights : matrix of n column, with n >1, first col is the reference
#'
#' @return vector of Hellinger distances measured between the first column as reference and
#'  each of the others
#'
#' @examples
#' data(meuse.grid, package = "sp")
#' weights <- cbind(
#'   meuse.grid[,"dist"] + 10,
#'   meuse.grid[,"dist"] + 10 + rnorm(nrow(meuse.grid), 0, 1)
#' )
#' hellinger(weights)
hellinger <- function(weights){
  if(ncol(weights) == 1){
    return(NA)
  }else{
    sum_orig <- sum(weights[,1], na.rm = TRUE)
    sapply(2:(ncol(weights)),
           function(c){
             sum_dest <- sum(weights[,c], na.rm = TRUE)
             sqdiff <- (sqrt(weights[,1]/sum_orig)-sqrt(weights[,c]/sum_dest))^2
             return(1/sqrt(2)*sqrt(sum(sqdiff)))
           }
    )
  }
}

#' Compute the average of relative absolute distance between one reference and several estimates
#'
#' @param weights : matrix of n column, with n >1, first col is the reference
#'
#' @return vector of distances measured between the first column as reference and
#'  each of the others
#'
#' @examples
#' data(meuse.grid, package = "sp")
#' weights <- cbind(
#'   meuse.grid[,"dist"] + 10,
#'   meuse.grid[,"dist"] + 10 + rnorm(nrow(meuse.grid), 0, 1)
#' )
#' aad_rel(weights)
aad_rel <- function(weights){
  if(ncol(weights) == 1){
    return(NA)
  }else{
    weights_no_empty <- weights[weights[,1] != 0,]
    sapply(2:(ncol(weights_no_empty)),
           function(c){
             diff <- abs(weights_no_empty[,1]-weights_no_empty[,c])/weights_no_empty[,c]
             return(mean(diff))
           }
    )
  }
}

#' Compute some distances functions on a raster object containing several layers 
#' of values
#'
#' @param raster_map 
#'
#' @return tibble object
#' @export
#'
#' @examples
#' data(meuse.grid, package = "sp")
#' coordinates <- as.matrix(meuse.grid[,1:2])
#' weights <- cbind(
#'   orig = meuse.grid[,"dist"] + 10,
#'   norm_sd1 = meuse.grid[,"dist"] + 10 + rnorm(nrow(meuse.grid), 0, 1)
#' )
#' rmeuse <- matrix_to_maps(coordinates, weights)
#' compare_distances(rmeuse)
compare_distances <- function(raster_map){
  
  res <- list()
  map_names <- raster_map@data@names
  # na_vals <- values(rmeuse)[,1] %>% is.na()
  # coords <- rmeuse %>% coordinates()
  
  test <- apply(values(raster_map), 1, function(r) sum(is.na(r)))
  
  if(sum(test > 0 & test != length(map_names)) > 0){
    raster_map <- reclassify(raster_map, cbind(NA, 0))
  }
  
  res[["kwd"]] <- SpatialKWD::compareOneToMany(
    coordinates(raster_map),
    values(raster_map),
    recode = TRUE
  )$distance

  res[["Hellinger"]] <- hellinger(
    raster_map %>% values() %>% na.omit()
  )
  
  res[["rmse"]] <- rmse(
    raster_map %>% values() %>% na.omit()
  )
  
  res[["raad"]] <- aad_rel(
    raster_map %>% values() %>% na.omit()
  )
  
  purrr::imap_dfr(
    res,
    function(v, n){
      tibble(distance = n, 
             map = map_names[-1],
             val = v
      )
    } 
  )
  
}






