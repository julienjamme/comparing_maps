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

# data available here:
# download.file("https://www.insee.fr/fr/statistiques/fichier/6215140/Filosofi2017_carreaux_1km_gpkg.zip", "filo2017_1km_gpkg.zip")
# download.file("https://www.insee.fr/fr/statistiques/fichier/6215140/Filosofi2017_carreaux_200m_gpkg.zip", "filo2017_200m_gpkg.zip")

# borders_mun_sf <- sf::st_read("data/commune_dep_974_2019.gpkg")
pop_grid_200m_sf <- sf::st_read("data/Filosofi2017_carreaux_200m_reun.gpkg")
pop_grid_1km_sf <- sf::st_read("data/Filosofi2017_carreaux_1km_reun.gpkg")
# pop_grid_nat_sf <- sf::st_read("data/Filosofi2017_carreaux_nivNaturel_reun.gpkg")
# str(pop_grid_200m_sf)

# pal <- c("#FDE333", "#BBDD38", "#6CD05E", "#00BE7D", "#00A890"
#          , "#008E98",  "#007094", "#185086", "#422C70", "#4B0055")

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


# Centroides of the 1km grid --------
centroides_1km <- sf::st_centroid(pop_grid_1km_sf)

wkt <- sf::st_crs(pop_grid_1km_sf)
epsg <- stringr::str_extract(wkt$wkt, "EPSG\",[1-9]*]]$") %>% 
  gsub(pattern = "EPSG\",", replacement = "") %>% 
  gsub(pattern = "]]", replacement = "")

# Select the useful variables of the 200m grid --------
pop_1km <- pop_grid_1km_sf %>%
  select(IdInspire = Idcar_1km, Ind, Men, Ind_snv)


# Make a microdata from the grid ------------------------------------------
# Units = households

hh_1km <- pop_1km %>% 
  sf::st_drop_geometry() %>% 
  select(IdInspire, Men) %>% 
  mutate(Men = ceiling(Men)) %>% 
  as.data.table()

hh_1km <- hh_1km[rep(seq(.N), Men), ]
hh_1km[, id := 1:.N]
nrow(hh_1km) == sum(ceiling(pop_1km$Men))
str(hh_1km)

# Add the geographic information in the microdata -------------------------

hh_1km <- hh_1km %>% 
  left_join(
    centroides_1km %>%
      select(IdInspire = Idcar_1km) %>%
      bind_cols(
        sf::st_coordinates(centroides_1km)
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


save(hh_200m, pop_200m, pop_grid_200m_sf, pop_1km, hh_1km, file = "data/sf_objects_LR.RData")
