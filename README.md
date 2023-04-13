# comparing_maps

## Objectives

- Tests on different ways to compare maps
- Compare maps from same original data on which different SDC methods have been applied
  - methods in mind: multilevel grids / CKM / Swapping

## Distances/Metrics

- One based on comparison of Moran's I
- Kullback-Leibler (based on Entropy)
- The most relevant one seems to be the Kantorovitch-Wasserstein Distance implemented in the [Spatial_KWD package](https://github.com/eurostat/Spatial-KWD).

## TODO

- build a microdataset 
- prepare microdata for ckm (key to individuals)
- compute grid data on 1km squares grid:
  - original data
  - perturbed data with CKM
  - perturbed data with multilevel grid method
- compare the utility of the different solutions with the KW distance.
