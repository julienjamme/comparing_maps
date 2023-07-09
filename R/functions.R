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


#' Compute the euclidean distance between one reference and several estimates
#'
#' @param weights : first col is the reference
#'
#' @return vector of Euclidean Distances measured between the first column as reference and
#'  each of the others
#'
#' @examples
#' data(meuse.grid, package = "sp")
#' weights <- cbind(
#'   meuse.grid[,"dist"] + 10,
#'   meuse.grid[,"dist"] + 10 + rnorm(nrow(meuse.grid), 0, 1)
#' )
#' rmse(weights)
euclid <- function(weights){
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
             diff <- abs(weights_no_empty[,1]-weights_no_empty[,c])/weights_no_empty[,1]
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
#' compare_distances(coordinates, weights)
compare_distances <- function(coordinates, weights){
  require(SpatialKWD)
  require(tibble)
  
  res <- list()
  
  map_names <- names(as.data.frame(weights))
  kwd <- SpatialKWD::compareOneToMany(
    as.matrix(coordinates),
    as.matrix(weights),
    recode = TRUE
  )
  
  res[["kwd"]] <- kwd$distance
  
  res[["Hellinger"]] <- hellinger(weights)
  
  res[["euclid"]] <- euclid(weights)
  
  res[["aard"]] <- aad_rel(weights)
  
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


#' Compute kwd with `focusArea` from the sdc_raster objects
#'
#' @param orig_raster sdcRaster object of the original distribution
#' @param pert_raster sdcRaster object of the perturbed distribution
#' @param type_value layer value to compare ("count", "sum", "mean")
#' @param center_thq coordinates of the theoretical center you want to use as the center of the focus area
#' @param x_min_thq x coordinate of the furthest point (to compute the radius if `radius` is null)
#' @param radius radius in the unit of the map (`NULL`) by default
#' @param cell_size cell size in the unit of the map
#' @param crs_epsg Coordinate Reference System: the numeric epsg code is enough
#'
#' @return a data.frame with
#' - "kwd" : the raw kwd computed by the `focusArea` function
#' - "kwd_fa" : kwd divided by the total values of the focus area
#' - "kwd_whole" : kwd divided by the total values of the whole map
#' @export
#'
#' @examples
#' \{Don't run}
#' res_fa <- compute_kwd_with_focus_area(
#' orig_raster = hh_200m_raster,
#' pert_raster = hh_200m_qt1,
#' type_value = "count",
#' center_thq = c(341835, 7687185),
#' x_min_thq = 337300,
#' cell_size = 200
#' )
compute_kwd_with_focus_area <- function(
    orig_raster,
    pert_raster,
    type_value = "count",
    center_thq,
    x_min_thq,
    radius = NULL,
    cell_size,
    crs_epsg = 2975
){
  pert_raster <- pert_raster$value$count
  pert_values <- raster::getValues(pert_raster)
  orig_values <- raster::getValues(orig_raster$value$count)
  
  # sum(pert_values, na.rm=TRUE) == sum(orig_values, na.rm=TRUE)
  
  pert_values[is.na(pert_values)] <- 0
  orig_values[is.na(orig_values)] <- 0
  
  xy <- raster::xyFromCell(pert_raster, seq_along(pert_values))
  
  if(nrow(xy[xy[,1] == center_thq[1] & xy[,1] == center_thq[2],]) == 0){
    ref_center <- c(center_thq - center_thq %% cell_size)
    
    # to pick up a cell which is near the theoretical/wanted center  
    #it may fail if the center is far from a 
    res_times <- 1
    no_candidates <- TRUE
    while(res_times <= 10 & no_candidates){
      index_center_candidates <- which(
        xy[,1] > (ref_center[1] - res_times*cell_size) &
          xy[,1] < (ref_center[1] + res_times*cell_size) & 
          xy[,2] > (ref_center[2] - res_times*cell_size) & 
          xy[,2] < (ref_center[2] + res_times*cell_size)
      )
      no_candidates <- length(index_center_candidates) == 0
      res_times <- res_times + 1
    }
    if(no_candidates){
      message("no candidates near the theoretical center")
      return(NULL)
    }
    cat("candidates found in a window of ", res_times*cell_size, " radius")
    
    center_fa <- xy[index_center_candidates[1],]
  }else{
    center_fa <- center_thq
  }
  
  radius_fa <- if(is.null(radius)){
    floor(abs((center[1] - x_min_thq))/cell_size)*cell_size
  }else radius
  
  x_min_fa <- center_fa[1] - radius_fa
  x_max_fa <- center_fa[1] + radius_fa
  y_max_fa <- center_fa[2] + radius_fa
  y_min_fa <- center_fa[2] - radius_fa
  
  c <- cell_size/2
  pts_sw <- c(x_min_fa - c, y_min_fa - c)
  pts_nw <- c(x_min_fa - c, y_max_fa + c)
  pts_se <- c(x_max_fa + c, y_min_fa - c)
  pts_ne <- c(x_max_fa + c, y_max_fa + c)

  st_pol_fa <- list(rbind(pts_sw, pts_nw, pts_ne, pts_se, pts_sw)) %>% 
    sf::st_polygon() %>% 
    sf::st_sfc(crs = crs_epsg)
  
  select_coords_fa <- which(
    xy[,1] >= x_min_fa & 
      xy[,1] <= x_max_fa & 
      xy[,2] <=  y_max_fa & 
      xy[,2] >= y_min_fa
  )
  xy_fa <- xy[select_coords_fa,]
  orig_values_fa <- orig_values[select_coords_fa]
  pert_values_fa <- pert_values[select_coords_fa]
  
  (d <- focusArea(
    Coordinates = xy,
    Weights = cbind(orig_values, pert_values),
    x = center_fa[1],
    y = center_fa[2],
    radius = radius_fa,
    area = "linf",
    method="exact", 
    recode=TRUE, 
    verbosity = "info"))
  
  return(
    list(
      st_pol_fa = st_pol_fa,
      res_kwd = data.frame(
        kwd = d$distance,
        kwd_fa = d$distance/sum(pert_values_fa),
        kwd_whole = d$distance/sum(pert_values)
      )
    )
  )
}




