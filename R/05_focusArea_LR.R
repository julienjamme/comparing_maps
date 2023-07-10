
source("R/functions.R")

# choice of the areas -----------------------------------------------------

desc_areas <- list(
  st_denis = list(center = c(344835, 7686185), x_min = 337300),
  st_pierre = list(center = c(341900, 7646100), x_min = 331100),
  plaine = list(center = c(357100, 7661500), x_min = 353000),
  st_gilles = list(center = c(321700, 7670800), x_min = 312600)
)


# Build areas -------------------------------------------------------------

list_st_focus_areas <- purrr::map(
  desc_areas,
  \(area){
    create_focus_area(
      orig_raster = hh_200m_raster,
      type_value = "count",
      center_thq = area$center,
      x_min_thq = area$x_min,
      cell_size = 200
    )
  }
)


# Area of each focus areas ------------------------------------------------

purrr::imap_dfr(
  list_st_focus_areas, 
  \(x,name_area){
    data.frame(
      focus_area = name_area,
      area_km2 = st_area(x) %>% units::set_units(km^2)
    )
  }
) %>% 
  knitr::kable(format = "latex", booktabs = TRUE, caption = "Focus areas by size")


# Visualize the focus areas in situ ---------------------------------------


st_focus_areas <- st_focus_areas <- c(
  list_st_focus_areas[["st_denis"]],
  list_st_focus_areas[["st_pierre"]],
  list_st_focus_areas[["plaine"]],
  list_st_focus_areas[["st_gilles"]]
)

qtls <- quantile(pop_200m$Men, probs = (0:10)/10)

mapview(
  st_focus_areas,
  color = c("red"),
  alpha.regions = 0,
  layer.name = "focus areas"
) +
  mapview(
    pop_200m, 
    z = c("Men"),
    at = qtls[seq(0,10,2)],
    lwd = 0,
    color.regions = pal[seq(1,10,2)],
    alpha.regions = 0.85,
    layer.name = "Nb of Households"
  ) 


# compute kwd -------------------------------------------------------------

maps_list <- list(
  quadtreeI = hh_200m_qt1,
  quadtreeII = hh_200m_qt2,
  smooth200 = hh_200m_sm_200,
  smooth400 = hh_200m_sm_400
)

res_kwd_on_fa <- purrr::map(
  maps_list,
  \(m){
    purrr::map(
      list_st_focus_areas,
      \(area){
        compute_kwd_with_focus_area(
          orig_raster = hh_200m_raster,
          pert_raster = m,
          type_value = "count",
          st_pol_fa = area
        )
      }
    )
  }
)
str(res_kwd_on_fa)



# KWD Results -----------------------------------------------------------------

st_pol_fa <- create_focus_area(
orig_raster = hh_200m_raster,
type_value = "count",
center_thq = desc_areas$plaine$center,
x_min_thq = desc_areas$plaine$x_min,
cell_size = 200
)

(res_fa <- compute_kwd_with_focus_area(
orig_raster = hh_200m_raster,
pert_raster = hh_200m_sm_200,
type_value = "count",
st_pol_fa
))



# 
# res_fa <- compute_kwd_with_focus_area(
#   orig_raster = hh_200m_raster,
#   pert_raster = hh_200m_qt1,
#   type_value = "count",
#   center_thq = c(341835, 7687185),
#   x_min_thq = 337300,
#   cell_size = 200
#   )