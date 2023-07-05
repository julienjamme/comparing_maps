library(raster)
library(sf)
library(ggplot2)
library(viridis)
library(purrr)
library(dplyr)
library(SpatialKWD)
library(sdcSpatial)
library(data.table)
library(mapview)

borders_mun_sf <- sf::st_read("data/commune_dep_974_2019.gpkg")
pop_grid_200m_sf <- sf::st_read("data/Filosofi2017_carreaux_200m_reun.gpkg")
pop_grid_nat_sf <- sf::st_read("data/Filosofi2017_carreaux_nivNaturel_reun.gpkg")
str(pop_grid_200m_sf)

# Centroides of the 200m grid --------
centroides <- sf::st_centroid(pop_grid_200m_sf)

wkt <- sf::st_crs(pop_grid_200m_sf)
epsg <- stringr::str_extract(wkt$wkt, "EPSG\",[1-9]*]]$") %>% 
  gsub(pattern = "EPSG\",", replacement = "") %>% 
  gsub(pattern = "]]", replacement = "")

# Select the useful variables of the 200m grid --------
pop_200m <- pop_grid_200m_sf %>%
  select(IdInspire = Idcar_200m, Idcar_1km, Ind, Men, Ind_snv)


# Make a microdata from the grid ------------------------------------------
# Units = households

hh_200m <- pop_200m %>% 
  sf::st_drop_geometry() %>% 
  select(IdInspire, Men) %>% 
  mutate(Men = ceiling(Men)) %>% 
  as.data.table()

hh_200m <- hh_200m[rep(seq(.N), Men), ]
hh_200m[, id := 1:.N]
nrow(hh_200m) == sum(ceiling(pop_200m$Men))
str(hh_200m)

# Add the geographic information in the microdata -------------------------

hh_200m <- hh_200m %>% 
  left_join(
    centroides %>% 
      select(IdInspire = Idcar_200m, Idcar_1km) %>%
      bind_cols(
        sf::st_coordinates(centroides)
      ) %>% 
      mutate(
        x_sw = as.integer(gsub("E","",stringr::str_extract_all(IdInspire, "E[0-9]*$", simplify = TRUE))),
        y_sw = as.integer(gsub("(N|E)","",stringr::str_extract_all(IdInspire, "N[0-9]*E", simplify = TRUE)))
      ) %>% 
      rename(
        x_centr = X,
        y_centr = Y
      ),
    by = "IdInspire"
  ) %>% 
  sf::st_as_sf(coords = c("x_centr", "y_centr"))
