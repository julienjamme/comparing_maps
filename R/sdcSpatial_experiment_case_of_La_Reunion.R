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

# download.file("https://www.insee.fr/fr/statistiques/fichier/6215140/Filosofi2017_carreaux_1km_gpkg.zip", "filo2017_1km_gpkg.zip")
# download.file("https://www.insee.fr/fr/statistiques/fichier/6215140/Filosofi2017_carreaux_200m_gpkg.zip", "filo2017_200m_gpkg.zip")
borders_mun_sf <- sf::st_read("data/commune_dep_974_2019.gpkg")
pop_grid_200m_sf <- sf::st_read("data/Filosofi2017_carreaux_200m_reun.gpkg")
pop_grid_nat_sf <- sf::st_read("data/Filosofi2017_carreaux_nivNaturel_reun.gpkg")
str(pop_grid_200m_sf)

centroides <- sf::st_centroid(pop_grid_200m_sf)

wkt <- sf::st_crs(pop_grid_200m_sf)
epsg <- stringr::str_extract(wkt$wkt, "EPSG\",[1-9]*]]$") %>% 
  gsub(pattern = "EPSG\",", replacement = "") %>% 
  gsub(pattern = "]]", replacement = "")

pop_200m <- pop_grid_200m_sf %>%
  select(IdInspire = Idcar_200m, Idcar_1km, Ind, Men, Ind_snv)
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

# ggplot(pop_200m) +
#   geom_sf(aes(fill = Men), color = NA) +
#   geom_sf(data = borders_mun_sf, fill = NA) +
#   scale_color_viridis_c(alpha = 0.75)


qt <- quantile(pop_200m$Men, probs = seq(0,1,0.2))
qt[1] <- 0
qt_nat <- quantile(pop_grid_nat_sf$Men, probs = seq(0,1,0.2))
qt_nat[1] <- 0
pal <- c("#FDE333", "#BBDD38", "#6CD05E", "#00BE7D", "#00A890"
         , "#008E98",  "#007094", "#185086", "#422C70", "#4B0055")
pal5 <- pal[seq(1,10,2)]
mapview(
  borders_mun_sf,
  col.regions = NA,
  color = c("grey15"),
  alpha = 0.95,
  alpha.regions = 0,
  map.types = c("OpenStreetMap","Esri.WorldImagery")
) + mapview(
  pop_200m, 
  z = c("Men"),
  at = qt,
  lwd = 0,
  color.regions = pal5,
  alpha.regions = 0.85,
  layer.name = "Households"
) +
  mapview(
  pop_grid_nat_sf, 
  z = c("Men"),
  at = qt_nat,
  lwd = 0.3,
  color.regions = pal5,
  alpha.regions = 0.85,
  na.color = NA,
  layer.name = "Natural Level"
)
#The quadtree is very sensitive to the structure of the grid
#Side effect of quadtree method applied on Inspire nested grid layers
#(200m,1km,2km,etc.).


summary(pop_200m$Men)
table(pop_200m$Men < 11)

hh_200m <- pop_200m %>% 
  sf::st_drop_geometry() %>% 
  select(IdInspire, Men) %>% 
  mutate(Men = ceiling(Men)) %>% 
  as.data.table()

hh_200m <- hh_200m[rep(seq(.N), Men), ]
hh_200m[, id := 1:.N]
nrow(hh_200m) == sum(ceiling(pop_200m$Men))

LaReunion_hh_200m <- hh_200m

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

plot(hh_200m_raster, "count", col=pal, alpha = 0.85)

# Sensitivity
sdcSpatial::sensitivity_score(hh_200m_raster)
prop.table(table(ceiling(pop_200m$Men) < 11))

# remove sensitive cells
hh_200m_rm <- sdcSpatial::remove_sensitive(hh_200m_raster)
plot(hh_200m_rm, "count", col = pal, alpha = 0.85)

length(hh_200m_rm$value$count)

# quadtree
hh_200m_qt <- sdcSpatial::protect_quadtree(hh_200m_raster, max_zoom = Inf)
plot(hh_200m_qt, "count", col = pal, alpha = 0.85)
#Different result from the one seen before: 
#hypothesis: the function does not take the nesting of Inspire grid layers.
#Ok for SDC if no use of Inspire grid at higher levels


# smoothing
hh_200m_sm_400 <- sdcSpatial::protect_smooth(hh_200m_raster, bw = 400)
hh_200m_sm_1000 <- sdcSpatial::protect_smooth(hh_200m_raster, bw = 1000)
plot(hh_200m_sm_400, "count", col = pal, alpha = 0.85)
plot(hh_200m_sm_1000, "count", col = pal, alpha = 0.85)

# The smoothing is quite low (several minutes to smooth only 14000 cells)
# and the result is a little bit "imprecise": it's a way to protect the data,
#but maybe we will make a lot of false positive (empty cells becoming non empty)
# PROP: smoothing with btb package seems to be more refined
# The btb package uses a quadratic kernel and a Diggle correction is implemented
# to take into account - as far as possible - the existence of borders.
# Presentation in Spatial Analysis Handbook, chapter 8
# https://www.insee.fr/en/information/3635545

pop_200m_df <- pop_grid_200m_sf %>% 
  sf::st_drop_geometry() %>% 
  dplyr::bind_cols(as.data.frame(sf::st_coordinates(centroides))[,1:2]) %>%
  dplyr::select(x=X,y=Y,Ind,Men) 

pop_200m_sm_400 <- btb::btb_smooth(pop_200m_df, sEPSG = "2975", iCellSize = 200, iBandwidth = 400)
pop_200m_sm_1000 <- btb::btb_smooth(pop_200m_df, sEPSG = "2975", iCellSize = 200, iBandwidth = 1000)
pop_200m_sm_2000 <- btb::btb_smooth(pop_200m_df, sEPSG = "2975", iCellSize = 200, iBandwidth = 2000)

mapview(
  pop_200m_sm_400, 
  z = c("Men"),
  at = qt,
  lwd = 0,
  color.regions = pal,
  alpha.regions = 0.65,
  na.color = NA,
  layer.name = "Smooth - 400m"
) +
  mapview(
    pop_200m_sm_1000, 
    z = c("Men"),
    at = qt,
    lwd = 0,
    color.regions = pal,
    alpha.regions = 0.65,
    na.color = NA,
    layer.name = "Smooth - 1km",
    hide = TRUE
  ) +
  mapview(
    pop_200m_sm_2000, 
    z = c("Men"),
    at = qt,
    lwd = 0,
    color.regions = pal,
    alpha.regions = 0.65,
    na.color = NA,
    layer.name = "Smooth - 2km",
    hide = TRUE
  ) +
  mapview(
    pop_200m %>% filter(Men < 11), 
    lwd = 0.2,
    color = "red",
    alpha.regions = 0,
    layer.name = "Sensitive"
  )

# Smoothing as SDC:
# - a larger bw (comp. to the resolution) ensures more protection
# - a too large bw will create a lot of non-sense cells
# - In dense area => smoothing could be useful to tackle some rare pbs
# - but with large maps (regional, state) mixing dense areas and rural areas,
# - the smoothing could lead to a high loss of utility.

# Case when smoothing could be helpful as a SDC method without loosing too much utility of the map
# - maps in quite dense areas
# - and in a quite homogoneic territory (not too many barriers creating too many no man's land)

# Assess the utility of a map (and not only the loss of utility)
# - Are we creating a lot of non empty cells ? 
# => difficult to defend when we can compare grid cells with satellite images for example

# ex: smoothing 

# => To avoid this kind of negative side effects, it's possible to 
# - smooth not too strongly : ie choosing a bandwidth that doesn't create not too many false populated cells
# - and remove the remaining sensitive cells.


# Assess the utility ------------------------------------------------------

# Hellinger distance
# KWD (with precautions and just as project)
# + share of false populated cells
# Moan's I ?




# Moran's I ---------------------------------------------------------------

# Neighbors
nb <- spdep::poly2nb(pop_200m, queen = TRUE)
# Weights of the neighbors with a row standardization (style=W)
lw <- spdep::nb2listw(nb, style="W", zero.policy=TRUE)

Imoran <- spdep::moran(pop_200m$Ind_snv, lw, length(nb), spdep::Szero(lw), zero.policy=TRUE)
Imoran$I
spdep::moran.test(pop_200m$Ind_snv, lw, alternative="greater", zero.policy = TRUE)




# pop_200m$Ind_snv_STD <- scale(pop_200m$Ind_snv)
# 
# spdep::moran.plot(
#   as.numeric(pop_200m$Ind_snv_STD),
#   listw = lw,
#   xlab = "Revenus disponibles médians par Iris",
#   ylab = "Moyenne des revenus des voisins",
#   main = "Diagramme de Moran"
# )
# 
# spdep::moran.plot(as.numeric(scale(pop_200m$Ind_snv)), lw, labels = FALSE, xlab ="",ylab="")

# library(quadtree)
# 
# r <- raster(
#   xmn = min(hh_200m$x_sw),
#   xmx = max(hh_200m$x_sw) + 200,
#   ymn = min(hh_200m$y_sw),
#   ymx = max(hh_200m$y_sw) + 200,
#   res = 200,
#   crs = 2975
# )
# 
# rast <- raster::rasterize(x = hh_200m %>% sf::st_coordinates(), y = r)
# values(rast) <- values(hh_200m_raster$value$count)
# qt <- quadtree::quadtree(
#   rast, 
#   # split_threshold = 100, 
#   split_method = "custom",
#   split_fun = function(vals,args){
#     if(all(is.na(vals))){
#       return(FALSE)
#     }else{
#       return(min(vals, na.rm=TRUE) >= args$threshold) 
#     }
#   },
#   split_args = list(threshold = 11),
#   combine_method = "custom",
#   combine_fun = function(vals,args) sum(vals[!is.na(vals)]),
#   adj_type = "expand"
# )
# plot(qt, crop = TRUE, border_lwd = .3, na_col = NULL)


