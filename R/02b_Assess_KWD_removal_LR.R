
x = hh_200m_rm
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

# approximate Kantorovic-Wasserstein distance
xy <- raster::xyFromCell(r_x, 1:raster::ncell(r_x))

# Put the missing mass on the top of Le Piton de la Fournaise (Volcano in activity in th south east of the island)
v_x_vol <- v_x
v_x_vol[which(xy[,1] == 367300 & xy[,2] == 7650300)] <- diff_mass
sum(v_x_vol) == sum(v_o)

# spread the missing mass randomly in all the cells (even the empty ones)
v_x_rand <- v_x
set.seed(123)
sel_cells <- sample(seq_along(v_x_rand), diff_mass, replace = FALSE)
v_x_rand[sel_cells] <- v_x_rand[sel_cells] + 1
sum(v_x_rand) == sum(v_o)

# Put the missing mass on the summits of the bbox
v_x_bbox <- v_x
bbox = apply(xy, 2, range)
v_x_bbox[which(xy[,1] == bbox[1,1] & xy[,2] == bbox[1,2])] <- diff_mass/4 #sw
v_x_bbox[which(xy[,1] == bbox[1,1] & xy[,2] == bbox[2,2])] <- diff_mass/4 #nw
v_x_bbox[which(xy[,1] == bbox[2,1] & xy[,2] == bbox[2,2])] <- diff_mass/4 #ne
v_x_bbox[which(xy[,1] == bbox[2,1] & xy[,2] == bbox[1,2])] <- diff_mass/4 #se
sum(v_x_bbox) == sum(v_o)

# Put the missing mass in the most populated square
v_x_pop <- v_x
v_x_pop[which.max(v_x_pop)] <- v_x_pop[which.max(v_x_pop)] + diff_mass
sum(v_x_pop) == sum(v_o)

# spread the missing mass randomly in populated cells (even the empty ones)
v_x_rand_pop <- v_x
set.seed(123)
sel_cells <- sample(which(v_x_rand_pop > 0), ceiling(diff_mass), replace = FALSE)
v_x_rand_pop[sel_cells[-1]] <- v_x_rand_pop[sel_cells[-1]] + 1
v_x_rand_pop[sel_cells[1]] <- v_x_rand_pop[sel_cells[1]] + (diff_mass - floor(diff_mass))
sum(v_x_rand_pop) == sum(v_o)

result_kwd_removal_with_bal_mass <- purrr::imap_dfr(
  list(
    no_treat = v_x,
    top_volcano = v_x_vol,
    random = v_x_rand,
    bbox = v_x_bbox,
    most_pop = v_x_pop,
    rand_pop = v_x_rand_pop
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


# for v_x with unbalanced cost => 0
# for v_x_sc => 2.94
# for miss mass put in one square at the top of a volcano => 14.07672
# for miss mass put in randomly drawn squares => 2.87
# for miss mass put in the corners of the bbox = 11.58428
# treatment       kwd
# 1    no_treat  2.940318
# 2 top_volcano 14.076724
# 3      random  2.877652
# 4        bbox 11.584279
# 5    most_pop 12.174640

