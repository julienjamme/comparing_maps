


# choice of the areas -----------------------------------------------------

desc_areas <- list(
  st_denis = list(center = c(344835, 7686185), x_min = 337300),
  st_pierre = list(center = c(343900, 7646100), x_min = 333100),
  plaine = list(center = c(357100, 7661500), x_min = 353000),
  st_gilles = list(center = c(321700, 7670800), x_min = 312600)
)


# compute kwd -------------------------------------------------------------

maps_list <- list(
  quadtreeI = hh_200m_qt1
  # quadtreeII = hh_200m_qt2,
  # smooth200 = hh_200m_sm_200,
  # smooth400 = hh_200m_sm_400
)

res_kwd_on_fa <- purrr::map(
  maps_list,
  \(m){
    purrr::map(
      desc_areas,
      \(d){
        compute_kwd_with_focus_area(
          orig_raster = hh_200m_raster,
          pert_raster = m,
          type_value = "count",
          center_thq = d$center,
          x_min_thq = d$x_min,
          cell_size = 200
        )
      }
    )
  }
)



# Visualize the areas -----------------------------------------------------




# KWD Results -----------------------------------------------------------------





# 
# res_fa <- compute_kwd_with_focus_area(
#   orig_raster = hh_200m_raster,
#   pert_raster = hh_200m_qt1,
#   type_value = "count",
#   center_thq = c(341835, 7687185),
#   x_min_thq = 337300,
#   cell_size = 200
#   )