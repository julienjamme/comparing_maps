x = hh_200m_qt2
orig = hh_200m_raster
value = "count"

# extract value raster of interest
r_x <- x$value[[value]]
r_o <- orig$value[[value]]

# extract cell values
v_x <- raster::getValues(r_x)
v_o <- raster::getValues(r_o)

# set NAs to 0
v_x[is.na(v_x)] <- 0
v_o[is.na(v_o)] <- 0

# check masses
diff_mass <- sum(v_o) - sum(v_x)

# v_x_sc <- v_x * (sum(v_o) / sum(v_x))

# approximate Kantorovic-Wasserstein distance
xy <- raster::xyFromCell(r_x, 1:raster::ncell(r_x))

SpatialKWD::compareOneToOne(
  Coordinates = xy,
  Weights = cbind(v_o, v_x)
)$distance

