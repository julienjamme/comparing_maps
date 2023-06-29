library(raster)
library(sf)
library(ggplot2)
library(viridis)
library(purrr)
library(dplyr)
library(SpatialKWD)
library(sdcSpatial)
library(data.table)

# download.file("https://www.insee.fr/fr/statistiques/fichier/6215140/Filosofi2017_carreaux_1km_gpkg.zip", "filo2017_1km_gpkg.zip")
# download.file("https://www.insee.fr/fr/statistiques/fichier/6215140/Filosofi2017_carreaux_200m_gpkg.zip", "filo2017_200m_gpkg.zip")
pop_grid_200m_sf <- sf::st_read("data/Filosofi2017_carreaux_200m_reun.gpkg")
str(pop_grid_200m_sf)

centroides <- sf::st_centroid(pop_grid_200m_sf)

wkt <- sf::st_crs(pop_grid_200m_sf)
epsg <- stringr::str_extract(wkt$wkt, "EPSG\",[1-9]*]]$") %>% 
  gsub(pattern = "EPSG\",", replacement = "") %>% 
  gsub(pattern = "]]", replacement = "")

pop_200m <- pop_grid_200m_sf %>%
  select(IdInspire = Idcar_200m, Idcar_1km, Ind)
#   mutate(
#     x_sw = as.integer(gsub("E","",stringr::str_extract_all(IdInspire, "E[0-9]*$", simplify = TRUE))),
#     y_sw = as.integer(gsub("(N|E)","",stringr::str_extract_all(IdInspire, "N[0-9]*E", simplify = TRUE)))
#   ) %>% 
#   bind_cols(
#     sf::st_coordinates(centroides)
#   ) %>% 
#   rename(
#     x_centr = X,
#     y_centr = Y
#   )

ggplot(pop_200m) +
  geom_sf(aes(fill = Ind), color = NA) +
  scale_color_viridis_c(alpha = 0.75)


ind_200m <- pop_200m %>% 
  sf::st_drop_geometry() %>% 
  select(IdInspire, Ind) %>% 
  mutate(Ind = round(Ind)) %>% 
  as.data.table()

ind_200m <- ind_200m[rep(seq(.N), Ind), ]
ind_200m[, id := 1:.N]
nrow(ind_200m) == sum(round(pop_200m$Ind))

ind_200m <- ind_200m %>% 
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
  sf::st_as_sf(geometry = "geom")

str(ind_200m)



r <- raster::raster(
  xmn = min(pop_200m$x_sw), xmx = max(pop_200m$x_sw + 200),
  ymn = min(pop_200m$y_sw), ymx = max(pop_200m$y_sw + 200),
  res = 200,
  crs = wkt
)

pop_200m_raster <- sdcSpatial::sdc_raster(
  ind_200m,
  r         = 200
)
plot(pop_200m_raster, value="sum", sensitive = FALSE)
summary(pop_200m$Ind)
table(pop_200m$Ind < 2)

pal <- rev(viridis(10))
plot(pop_200m_raster, col = pal)


raster_sens <- is_sensitive(pop_200m_raster)


