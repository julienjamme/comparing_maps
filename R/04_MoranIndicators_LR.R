# Compute Moran's I on maps

source("R/01_build_grid_and_microdata_LR.R")

# Neighbors ------------------------------------------------------------------

nb <- spdep::poly2nb(pop_200m, queen = TRUE)

# Weights of the neighbors with a row standardization (style=W) --------------
lw <- spdep::nb2listw(nb, style="W", zero.policy=TRUE)

# Compute Moran's I ----------------------------------------------------------

Imoran_1 <- spdep::moran(
  pop_200m$Ind_snv, 
  lw,
  length(nb),
  spdep::Szero(lw), 
  zero.policy=TRUE
)
Imoran$I

# Or with the moran.test function which delivers a p.value 

Imoran_2 <- spdep::moran.test(
  pop_200m$Ind_snv, 
  lw, 
  alternative="greater", # We expect that the correlation is positive
  zero.policy = TRUE
)
