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
    cbind(data.frame(count = raster::getValues(raster_count))) %>% 
    filter(!is.na(count)) %>% 
    mutate(square_num = 1:n()) %>%
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
    ) %>% 
    rename(x_centr = x, y_centr = y) %>% 
    tidyr::unite(x1,y1, col = "x1_y1",sep = "_",remove = TRUE) %>% 
    tidyr::unite(x2,y2, col = "x2_y2",sep = "_",remove = TRUE) %>% 
    tidyr::unite(x3,y3, col = "x3_y3",sep = "_",remove = TRUE) %>% 
    tidyr::unite(x4,y4, col = "x4_y4",sep = "_",remove = TRUE) %>% 
    tidyr::unite(x5,y5, col = "x5_y5",sep = "_",remove = TRUE) %>%
    tidyr::pivot_longer(matches("_y"), names_to = "pt", values_to = "x_y") %>% 
    tidyr::separate(x_y, into =c("x", "y"), sep = "_") %>% 
    mutate(across(all_of(c("x","y")), as.numeric))
  
  counts_grid_sf <- points %>%
    st_as_sf(coords = c("x", "y"), crs = crs_ref) %>%
    group_by(square_num, count) %>%
    summarise(geometry = st_combine(geometry), .groups = "drop") %>%
    st_cast("POLYGON")
  
  nb <- spdep::poly2nb(counts_grid_sf, queen = TRUE)
  lw <- spdep::nb2listw(nb, style="W", zero.policy=TRUE)
  
  return(
    spdep::moran.test(
      counts_grid_sf$count, 
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

Moran_orig <- compute_Moran_I_from_sdcRaster(sdcRaster = hh_200m_raster, sf_ref = pop_200m)


