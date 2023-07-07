
pert_raster <- hh_200m_qt1$value$count
pert_values <- raster::getValues(pert_raster)
orig_values <- raster::getValues(hh_200m_raster$value$count)

sum(pert_values, na.rm=TRUE) == sum(orig_values, na.rm=TRUE)

xy <- raster::xyFromCell(pert_raster, seq_along(pert_values))


# search coordinates for the center of the focus area ---------------------

st_denis_borders <- borders_mun_sf %>% filter(libelle == "Saint-Denis") %>% st_geometry()

st_denis_grid_200m_sf <- sf::st_intersection(
  pop_grid_200m_sf,
  st_denis_borders
)

box <- st_bbox(st_denis_grid_200m_sf)
radius = c(box[["ymax"]]-box[["ymin"]], 
             box[["xmax"]]-box[["xmin"]])/2
center_thq <- c((box[["ymax"]]+box[["ymin"]])/2, 
                (box[["xmax"]]+box[["xmin"]])/2)
center_candidates <- xy[xy[,1] > 338000 & xy[,1] < 338400 & xy[,2] > 7686600 & xy[,2] < 7687200,]

(d <- focusArea(
  Coordinates = xy,
  Weights = cbind(orig_values, pert_values),
  x=center_candidates[1,1],
  y=center_candidates[1,2],
  radius=as.integer(min(radius)),
  area = "linf",
  method="approx", recode=TRUE, verbosity = "info"))
