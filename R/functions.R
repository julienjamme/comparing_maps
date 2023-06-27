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
  rad <- paste0("CRS",crs,"RES",res)
  xy_coords <- gsub(rad, "", inspire_coords)
  x_coords <- gsub("^N.*E", "", xy_coords, perl = TRUE)
  y_coords <- gsub("(^N|E.*$)", "", xy_coords, perl = TRUE)
  return(cbind(x = as.numeric(x_coords), y = as.numeric(y_coords)))
}


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
#' @return vector of rmses measured between the first column as reference and
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
    sum_orig <- sum(weights[,1])
    sapply(2:(ncol(weights)),
           function(c){
             sum_dest <- sum(weights[,c])
             sqdiff <- (sqrt(weights[,1]/sum_orig)-sqrt(weights[,c]/sum_dest))^2
             return(1/sqrt(2)*sqrt(sum(sqdiff)))
           }
    )
  }
  
}


