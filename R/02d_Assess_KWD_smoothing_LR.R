x = hh_200m_sm_200
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
diff_mass/sum(v_o)*100

# v_x_sc <- v_x * (sum(v_o) / sum(v_x))


# different way to deal with missing mass ---------------------------------


# spread the missing mass randomly in cells (even the empty ones)
v_x_rand <- v_x
set.seed(123)
sel_cells <- sample(seq_along(v_x_rand), ceiling(diff_mass), replace = FALSE)
v_x_rand[sel_cells[-1]] <- v_x_rand[sel_cells[-1]] + 1
v_x_rand[sel_cells[1]] <- v_x_rand[sel_cells[1]] + (diff_mass - floor(diff_mass))
sum(v_x_rand) == sum(v_o)

# spread the missing mass randomly in populated cells (even the empty ones)
v_x_rand_pop <- v_x
set.seed(123)
sel_cells <- sample(which(v_x_rand_pop > 0), ceiling(diff_mass), replace = FALSE)
v_x_rand_pop[sel_cells[-1]] <- v_x_rand_pop[sel_cells[-1]] + 1
v_x_rand_pop[sel_cells[1]] <- v_x_rand_pop[sel_cells[1]] + (diff_mass - floor(diff_mass))
sum(v_x_rand_pop) == sum(v_o)

# For smoothing, another way unexplored here would be to extend the map


# approximate Kantorovic-Wasserstein distance
xy <- raster::xyFromCell(r_x, 1:raster::ncell(r_x))

# SpatialKWD::compareOneToOne(
#   Coordinates = xy,
#   Weights = cbind(v_o, v_x_rand_pop)
# )$distance


result_kwd_smoothing200_with_bal_mass <- purrr::imap_dfr(
  list(
    no_treat = v_x,
    random = v_x_rand,
    random_pop = v_x_rand_pop
  ),
  function(v, name_treat){
    data.frame(
      treatment = name_treat,
      kwd = SpatialKWD::compareOneToOne(
        Coordinates = xy,
        Weights = cbind(v_o, v)
      )$distance
    )
  }
)

