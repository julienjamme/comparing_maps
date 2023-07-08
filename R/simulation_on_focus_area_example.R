library(SpatialKWD)

set.seed(40889)
n <- 500
res <- matrix(nrow = n, ncol=3)
colnames(res) <- c("kwd","kwd_fa","kwd_whole")

for(i in seq_len(n)){
  N = 90
  Xs <- as.integer(runif(N, 0, 31))
  Ys <- as.integer(runif(N, 0, 31))
  coordinates <- matrix(c(Xs, Ys), ncol=2, nrow=N)
  index_coordinates_fa <- coordinates[
    coordinates[,1] >= 10 &
      coordinates[,2] >= 10 &
      coordinates[,1] <= 20 &
      coordinates[,2] <= 20,
  ]
  # Random weights
  test1 <- matrix(runif(2*N, 0, 1), ncol=2, nrow=N)
  test1_fa <- test1[index_coordinates_fa,]
  # Compute distance
  print("Compare one-to-one with exact algorithm:")
  d <- focusArea(coordinates, Weights=test1,
                 x=15, y=15, radius=5,
                 method="exact", recode=TRUE, verbosity = "info")
  res[i,] <- c(d$distance, d$distance/sum(test1_fa[,2]), d$distance/sum(test1[,2]))
}

d_means <- colMeans(res)
cummeans <- apply(res, 2, cummean)

plot(cummeans[,"kwd"], type="l")
plot(cummeans[,"kwd_fa"], type="l")
plot(cummeans[,"kwd_whole"], type="l")

(range_kwd <- apply(simulation_on_focus_area_example_results$res, 2, range))


