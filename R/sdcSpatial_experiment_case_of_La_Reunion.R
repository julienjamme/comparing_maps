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
  select(IdInspire = Idcar_200m, Idcar_1km, Ind, Men)
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
  geom_sf(aes(fill = Men), color = NA) +
  scale_color_viridis_c(alpha = 0.75)


hh_200m <- pop_200m %>% 
  sf::st_drop_geometry() %>% 
  select(IdInspire, Men) %>% 
  mutate(Men = round(Men)) %>% 
  as.data.table()

hh_200m <- hh_200m[rep(seq(.N), Men), ]
hh_200m[, id := 1:.N]
nrow(hh_200m) == sum(round(pop_200m$Men))

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
str(hh_200m)

hh_200m_raster <- sdcSpatial::sdc_raster(
  hh_200m,
  variable = 1,
  r = 200,
  min_count = 11,
  max_risk = 1
)

plot(hh_200m_raster, value="sum", sensitive = FALSE)
summary(pop_200m$Men)
table(pop_200m$Men < 10)

pal <- rev(viridis(10))
plot(hh_200m_raster, value = "sum", col = pal)


# quadtree
hh_200m_qt <- protect_quadtree(hh_200m_raster)
col <- c("#FDE333", "#BBDD38", "#6CD05E", "#00BE7D", "#00A890"
         , "#008E98",  "#007094", "#185086", "#422C70", "#4B0055")
plot(hh_200m_raster, "sum", col=col)
plot(hh_200m_qt, "sum", col=col)
