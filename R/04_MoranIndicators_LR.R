# Compute Moran's I on maps

source("R/01_build_grid_and_microdata_LR.R")

# Neighbors ------------------------------------------------------------------

nb <- spdep::poly2nb(pop_200m, queen = TRUE)

# Weights of the neighbors with a row standardization (style=W) --------------
lw <- spdep::nb2listw(nb, style="W", zero.policy=TRUE)

# Compute Moran's I of original map -------------------------------------------

# Imoran_orig_1 <- spdep::moran(
#   pop_200m$Men, 
#   lw,
#   length(nb),
#   spdep::Szero(lw), 
#   zero.policy=TRUE
# )
# Imoran_orig_1$I

# Or with the moran.test function which delivers a p.value 

Imoran_orig <- spdep::moran.test(
  pop_200m$Men, 
  lw, 
  alternative="greater", # We expect that the correlation is positive
  zero.policy = TRUE
)



# Compute Moran's I of the perturbed data ---------------------------------


compute_Moran_I_from_sdcRaster <- function(
    sdcRaster,
    sf_ref,
    value = "count",
    res = 200
){
  
  raster_count <- sdcRaster$value[[value]]
  n_cells <- raster::ncell(raster_count)
  xy_coords <- raster::xyFromCell(raster_count, seq_len(n_cells))
  
  crs_ref <- sf::st_crs(sf_ref)
  
  points <- as.data.frame(xy_coords) %>%
    mutate(
      x1 = x - res/2,
      y1 = y - res/2,
      x2 = x - res/2,
      y2 = y + res/2,
      x3 = x + res/2,
      y3 = y + res/2,
      x4 = x + res/2,
      y4 = y - res/2,
      x5 = x - res/2,
      y5 = y - res/2
    )
  list_mat <- list()
  for(i in seq_len(nrow(points))){
    list_mat[[i]] <- matrix(
      as.numeric(points[i,-(1:2)]), 
      ncol = 2,
      byrow = TRUE
    )
  }
  grid_poly <- sf::st_polygon(list_mat) %>% st_as_sf()
    
    sf::st_as_sf(coords = c("x","y"), crs = crs_ref) %>% 
    cbind(data.frame(count = raster::getValues(raster_count))) %>% 
    filter(!is.na(count)) %>% 
    st_make_grid(sf_ref, cellsize = 200, offset = st_bbox(sf_ref)[c("xmin", "ymin")]) %>%
    sf::st_as_sf() %>% str()
    left_join()
    filter(!is.na(count)) %>%
    left_join(
      sf_ref %>%
        cbind(
          sf_ref %>% sf::st_centroid() %>% sf::st_coordinates()
        ), by = c("x" = "X", "y" = "Y")
    ) %>% sf::st_as_sf()
  
  nb <- spdep::poly2nb(sf_counts, queen = TRUE)
  lw <- spdep::nb2listw(nb, style="W", zero.policy=TRUE)
  
  return(
    spdep::moran.test(
      sf_counts$count, 
      lw, 
      alternative="greater", # We expect that the correlation is positive
      zero.policy = TRUE
    )
  )
}

MoranTest_list <- purrr::map(
  maps_list,
  compute_Moran_I_from_sdcRaster,
  sf_ref = pop_200m
)

compute_Moran_I_from_sdcRaster(sdcRaster = hh_200m_qt1, sf_ref = pop_200m)


