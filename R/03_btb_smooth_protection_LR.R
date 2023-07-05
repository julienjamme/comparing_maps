
source("R/01_build_grid_and_microdata_LR.R")

pop_200m_df <- pop_grid_200m_sf %>% 
  sf::st_drop_geometry() %>% 
  dplyr::bind_cols(as.data.frame(sf::st_coordinates(centroides))[,1:2]) %>%
  dplyr::select(x=X,y=Y,Ind,Men) 

pop_200m_smbtb_400 <- btb::btb_smooth(
  pop_200m_df, 
  sEPSG = "2975", 
  iCellSize = 200, 
  iBandwidth = 400
) %>% 
  rename_with(~paste0(.,"_sm400"), .cols = all_of(c("Ind","Men"))) %>% 
  dplyr::full_join(pop_200m_df %>% st_drop_geometry(), by = c("x","y")) %>% 
  dplyr::relocate(Ind, Men, .after = y) %>% 
  mutate(across(matches("^(Ind|Men)"), ~ifelse(is.na(.),0,.)))

str(pop_200m_smbtb_400)