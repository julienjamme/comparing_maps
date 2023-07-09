qt <- quantile(pop_200m$Men, probs = seq(0,1,0.2))
qt[1] <- 0
qt_nat <- quantile(pop_grid_nat_sf$Men, probs = seq(0,1,0.2))
qt_nat[1] <- 0
pal <- c("#FDE333", "#BBDD38", "#6CD05E", "#00BE7D", "#00A890"
         , "#008E98",  "#007094", "#185086", "#422C70", "#4B0055")
pal5 <- pal[seq(1,10,2)]

st_focus_areas <- c(
  res_kwd_on_fa[[1]][["st_denis"]]$st_pol_fa,
  res_kwd_on_fa[[1]][["st_pierre"]]$st_pol_fa,
  res_kwd_on_fa[[1]][["plaine"]]$st_pol_fa,
  res_kwd_on_fa[[1]][["st_gilles"]]$st_pol_fa
)


mapview(
  st_focus_areas,
    color = c("red"),
    alpha.regions = 0,
    layer.name = "focus areas"
  ) +
  mapview(
    pop_200m, 
    z = c("Men"),
    at = qt,
    lwd = 0,
    color.regions = pal5,
    alpha.regions = 0.85,
    layer.name = "Households"
  )

ggplot() +
  geom_sf(data = st_pol_fa, fill = NA, color = "red", size = 1) +
  geom_sf(data = pop_200m, aes(fill = Men), alpha = 0.5)


st_crs(st_pol_fa) == st_crs(pop_200m)
