
pert_raster <- hh_200m_qt1$value$count
pert_values <- raster::getValues(pert_raster)
orig_values <- raster::getValues(hh_200m_raster$value$count)

sum(pert_values, na.rm=TRUE) == sum(orig_values, na.rm=TRUE)

pert_values[is.na(pert_values)] <- 0
orig_values[is.na(orig_values)] <- 0

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
index_center_candidates <- which(xy[,1] > 338000 & xy[,1] < 338400 & xy[,2] > 7686600 & xy[,2] < 7687200)

(d <- focusArea(
  Coordinates = xy,
  Weights = cbind(orig_values, pert_values),
  x = xy[index_center_candidates[1],1],
  y = xy[index_center_candidates[1],2],
  radius = 20,
  area = "linf",
  method="approx", 
  recode=TRUE, 
  verbosity = "info"))
# value of the distance is > 7000 !

# try different radius values
kwd_diff_rads <- lapply(
  c(20,200,2000,20000),
  function(r){
    focusArea(
      Coordinates = xy,
      Weights = cbind(orig_values, pert_values),
      x = xy[index_center_candidates[1],1],
      y = xy[index_center_candidates[1],2],
      radius = r,
      area = "linf",
      method="approx", 
      recode=TRUE, 
      verbosity = "info")
  }
)
sapply(kwd_diff_rads, function(a) a$distance)
# kwd result: 7544.573  94759.535 120782.736 120782.736

# With integer coordinates ------------------------------------------------

(d <- focusArea(
  Coordinates = cbind(seq_along(xy), seq_along(xy)),
  Weights = cbind(orig_values, pert_values),
  x = index_center_candidates[1], #xy[index_center_candidates[1],1],
  y = index_center_candidates[1], #xy[index_center_candidates[1],2],
  radius = 20,
  area = "linf",
  method="approx", 
  verbosity = "info"))
# Long and return an error for presence of negative weights!


# use compareOneToOne directly on the area --------------------------------
center <- center_candidates[1,]
radius <- 2000 #meters

select_coords_fa <- which(
  xy[,1] > center[1] - radius & 
    xy[,1] < center[1] + radius & 
    xy[,2] < center[2] + radius & 
    xy[,2] > center[2] - radius
)
xy_fa <- xy[select_coords_fa,]
orig_values_fa <- orig_values[select_coords_fa]
pert_values_fa <- pert_values[select_coords_fa]

compareOneToOne(xy, cbind(orig_values, pert_values))$distance
compareOneToOne(xy_fa, cbind(orig_values_fa, pert_values_fa))$distance
# We retrieve here some realistic values
