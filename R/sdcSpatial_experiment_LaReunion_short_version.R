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

source("R/utility_assessment_functions.R", encoding = "UTF-8")

# download.file("https://www.insee.fr/fr/statistiques/fichier/6215140/Filosofi2017_carreaux_1km_gpkg.zip", "filo2017_1km_gpkg.zip")
# download.file("https://www.insee.fr/fr/statistiques/fichier/6215140/Filosofi2017_carreaux_200m_gpkg.zip", "filo2017_200m_gpkg.zip")
borders_mun_sf <- sf::st_read("data/commune_dep_974_2019.gpkg")
pop_grid_200m_sf <- sf::st_read("data/Filosofi2017_carreaux_200m_reun.gpkg")
pop_grid_nat_sf <- sf::st_read("data/Filosofi2017_carreaux_nivNaturel_reun.gpkg")
str(pop_grid_200m_sf)

pal <- c("#FDE333", "#BBDD38", "#6CD05E", "#00BE7D", "#00A890"
         , "#008E98",  "#007094", "#185086", "#422C70", "#4B0055")


centroides <- sf::st_centroid(pop_grid_200m_sf)

wkt <- sf::st_crs(pop_grid_200m_sf)
epsg <- stringr::str_extract(wkt$wkt, "EPSG\",[1-9]*]]$") %>% 
  gsub(pattern = "EPSG\",", replacement = "") %>% 
  gsub(pattern = "]]", replacement = "")

pop_200m <- pop_grid_200m_sf %>%
  select(IdInspire = Idcar_200m, Idcar_1km, Ind, Men, Ind_snv)

hh_200m <- pop_200m %>% 
  sf::st_drop_geometry() %>% 
  select(IdInspire, Men) %>% 
  mutate(Men = ceiling(Men)) %>% 
  as.data.table()

hh_200m <- hh_200m[rep(seq(.N), Men), ]
hh_200m[, id := 1:.N]
nrow(hh_200m) == sum(ceiling(pop_200m$Men))

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
head(hh_200m)

hh_200m_raster <- sdcSpatial::sdc_raster(
  hh_200m,
  variable = 1,
  r = 200,
  min_count = 11,
  max_risk = 1
)

plot(hh_200m_raster, "count", col=pal, alpha = 0.85)

# Sensitivity
sdcSpatial::sensitivity_score(hh_200m_raster)
prop.table(table(ceiling(pop_200m$Men) < 11))
sum(pop_200m$Men[pop_200m$Men <11])/sum(pop_200m$Men)
nrow(pop_grid_200m_sf)

# remove sensitive cells
hh_200m_rm <- sdcSpatial::remove_sensitive(hh_200m_raster)
plot(hh_200m_rm, "count", col = pal, alpha = 0.85)

length(hh_200m_rm$value$count)

# quadtree
hh_200m_qt1 <- sdcSpatial::protect_quadtree(hh_200m_raster, max_zoom = 2)
plot(hh_200m_qt1, "count", col = pal, alpha = 0.85)
hh_200m_qt2 <- sdcSpatial::protect_quadtree(hh_200m_raster, max_zoom = 3)
plot(hh_200m_qt2, "count", col = pal, alpha = 0.85)
#Different result from the one seen before: 
#hypothesis: the function does not take the nesting of Inspire grid layers.
#Ok for SDC if no use of Inspire grid at higher levels


# smoothing
hh_200m_sm_200 <- sdcSpatial::protect_smooth(hh_200m_raster, bw = 200)
hh_200m_sm_400 <- sdcSpatial::protect_smooth(hh_200m_raster, bw = 400)
plot(hh_200m_sm_200, "count", col = pal, alpha = 0.85)
plot(hh_200m_sm_400, "count", col = pal, alpha = 0.85)


pop_200m_df <- pop_grid_200m_sf %>% 
  sf::st_drop_geometry() %>% 
  dplyr::bind_cols(as.data.frame(sf::st_coordinates(centroides))[,1:2]) %>%
  dplyr::select(x=X,y=Y,Ind,Men) 

pop_200m_smbtb_400 <- btb::btb_smooth(
  pop_200m_df, 
  sEPSG = "2975", 
  iCellSize = 200, 
  iBandwidth = 400
) %>% 
  rename_with(~paste0(.,"_sm400"), .cols = all_of(c("Ind","Men"))) %>% 
  dplyr::full_join(pop_200m_df %>% st_drop_geometry(), by = c("x","y")) %>% 
  dplyr::relocate(Ind, Men, .after = y) %>% 
  mutate(across(matches("^(Ind|Men)"), ~ifelse(is.na(.),0,.)))
str(pop_200m_smbtb_400)


# Utility assessment ------------------------------------------------------

maps_list <- list(
  remove = hh_200m_rm,
  quadtreeI = hh_200m_qt1,
  quadtreeII = hh_200m_qt2,
  smooth200 = hh_200m_sm_200,
  smooth400 = hh_200m_sm_400
)

# utility_assess <- purrr::imap_dfr(
#   maps_list,
#   function(r_pert){
#     get_utility(r_pert, hh_200m_raster, value = "count", measure = c("RMSE", "HD", "KWD"))
#   }
# )

# do.call("rbind", args = utility_assess) %>% 
#   as.data.frame()

utility_assess <- purrr::imap_dfr(
  maps_list,
  function(r_pert, map_name){
    cbind.data.frame(
      map = map_name,
      t(get_utility(r_pert, hh_200m_raster, value = "count", measure = c("RMSE", "HD", "KWD")))
    )
  }
)

residual_risk <- purrr::imap_dfr(
  maps_list,
  function(r_pert, map_name){
    data.frame(
      map = map_name,
      residual_risk = sdcSpatial::sensitivity_score(r_pert)
    )
  }
)

df_la_reunion <- pop_200m_smbtb_400 %>% sf::st_drop_geometry()

distances_st_denis <- compare_distances(
  coordinates = df_la_reunion %>% dplyr::select(x,y),
  weights = df_la_reunion %>% dplyr::select(dplyr::starts_with("Men"))
)


save(utility_assess, residual_risk, maps_list, file = "risk_utility_la_reunion.RData")


# sensitive_cells_qt1 <- sdcSpatial::is_sensitive(hh_200m_qt1)
# plot(sensitive_cells_qt1)
# sum(values(sensitive_cells_qt1$sensitive$count), na.rm = TRUE)
# 
# sens_cells_qt1 <- sensitive_cells_qt1@data@values
# sens_cells_qt1[is.na(sens_cells_qt1)] <- FALSE
# 
# sum(sens_cells_qt1)
# 
# sdcSpatial::sensitivity_score(hh_200m_qt1)
# length(hh_200m_raster$value$count)
# length(hh_200m_qt1$value$count)
# prop.table(table(raster::values(hh_200m_qt1$value$count) < 11))
# sum(raster::values(sdcSpatial::is_sensitive(hh_200m_qt1)), na.rm=TRUE)
# sum(raster::values(sdcSpatial::is_sensitive(hh_200m_raster)), na.rm=TRUE)
# length(raster::values(sdcSpatial::is_sensitive(hh_200m_qt1)))
# 8142/91200
# 
# sum(raster::values(sdcSpatial::is_sensitive(hh_200m_qt1)) * raster::values(hh_200m_qt1$value$count), na.rm=TRUE)
# sum(raster::values(hh_200m_qt1$value$count), na.rm=TRUE)







