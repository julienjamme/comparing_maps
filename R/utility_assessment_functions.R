###### fun: calculate utility values for protected maps ##### ###
# x:       sdc_raster object - after map protection
# orig:    sdc_raster object - original before protection
# value:   value to compare ("count", "sum", ...)
# measure: one or several of the following
#           - bin-by-bin: -
#            RMSE: Root Mean Squared Error
#            HD:   Hellinger's Distance
#           - cross-bin: -
#            Moran: change in (global) Moran's I
#            KWD: Kantorovic-Wasserstein Distance
# ...  :   additional parameters for SpatialKWD::compareOneToOne()
# Source: https://github.com/mamoeh/sdcSpatialExperiment_DE/blob/master/02_sdcSpatial_DE.R
get_utility <- function(x, orig, 
                        value = "count", 
                        measure = c("RMSE", "HD", "Moran", "KWD"), 
                        ...) {
  
  u <- vector("numeric", length = length(measure))
  names(u) <- measure
  
  # extract value raster of interest
  r_x <- x$value[[value]]
  r_o <- orig$value[[value]]
  
  # extract cell values
  v_x <- raster::getValues(r_x)
  v_o <- raster::getValues(r_o)
  
  # set NAs to 0
  v_x[is.na(v_x)] <- 0
  v_o[is.na(v_o)] <- 0
  
  for (i in seq_along(u)) {
    
    if (measure[i] == "RMSE") { 
      
      # compute root mean squared error
      u[i]  <- sqrt(mean((v_x - v_o)^2))
    }
    
    if (measure[i] == "HD") {
      
      # compute Hellinger's distance
      u[i] <- sqrt(sum((sqrt(v_o/sum(v_o)) - sqrt(v_x/sum(v_x)))^2)) * (1/(sqrt(2)))
    }
    
    if (measure[i] == "Moran") {
      
      # change in (global) Moran's I
      u[i] <- Moran(r_x) - Moran(r_o)
    }
    
    if (measure[i] == "KWD") {
      
      # rescale to balance mass
      v_x_sc <- v_x * (sum(v_o) / sum(v_x))
      
      # approximate Kantorovic-Wasserstein distance
      xy <- raster::xyFromCell(r_x, 1:raster::ncell(r_x))
      u[i] <- SpatialKWD::compareOneToOne(Coordinates = xy,
                                          Weights = cbind(v_o, v_x_sc),
                                          ...)$distance
    }
  }
  
  u
} 