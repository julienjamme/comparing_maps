
source("R/01_build_grid_and_microdata_LR.R")

hh_200m_raster <- sdcSpatial::sdc_raster(
  hh_200m,
  variable = 1,
  r = 200,
  min_count = 11,
  max_risk = 1
)

plot(hh_200m_raster, "count", col=pal, alpha = 0.85)

# Sensitivity ------------------------------------------------------------

sdcSpatial::sensitivity_score(hh_200m_raster)
prop.table(table(ceiling(pop_200m$Men) < 11))

# remove sensitive cells -------------------------------------------------

hh_200m_rm <- sdcSpatial::remove_sensitive(hh_200m_raster)
plot(hh_200m_rm, "count", col = pal, alpha = 0.85)

length(hh_200m_rm$value$count)

# quadtree ---------------------------------------------------------------

hh_200m_qt1 <- sdcSpatial::protect_quadtree(hh_200m_raster, max_zoom = 3)
hh_200m_qt2 <- sdcSpatial::protect_quadtree(hh_200m_raster, max_zoom = 4)
plot(hh_200m_qt1, "count", col = pal, alpha = 0.85)
plot(hh_200m_qt2, "count", col = pal, alpha = 0.85)
#Different result from the one seen before: 
#hypothesis: the function does not take the nesting of Inspire grid layers.
#Ok for SDC if no use of Inspire grid at higher levels

# smoothing --------------------------------------------------------------

hh_200m_sm_200 <- sdcSpatial::protect_smooth(hh_200m_raster, bw = 200)
hh_200m_sm_400 <- sdcSpatial::protect_smooth(hh_200m_raster, bw = 400)
plot(hh_200m_sm_200, "count", col = pal, alpha = 0.85)
plot(hh_200m_sm_400, "count", col = pal, alpha = 0.85)


# Utility assessment ------------------------------------------------------

maps_list <- list(
  remove = hh_200m_rm,
  quadtreeI = hh_200m_qt1,
  quadtreeII = hh_200m_qt2,
  smooth200 = hh_200m_sm_200,
  smooth400 = hh_200m_sm_400
)

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






