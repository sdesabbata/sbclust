test_that("Test sample-based clustering", {

  # Create test dataset
  # using approach suggested by Wang et al. (2008)
  # https://onlinelibrary.wiley.com/doi/10.1002/int.20268
  test_dataset_size <- 3000000
  centres_prop <- c(0.2, 0.2, 0.3, 0.1, 0.2)
  centres_mean <- array(
    c(-3, 0, 4, 5, -5, -3, 0, 3, -2, 4),
    c(5, 2)
  )
  centres_sd <- array(
    c(1, 1, 0.1, 0.5, 0.2, 0.2, 1, 1, 1, 1),
    c(5, 2)
  )
  values_x <- c()
  values_y <- c()
  values_cluster <- c()
  for (i in 1:5) {
    values_x <-
      c(
        values_x,
        rnorm(n = test_dataset_size*centres_prop[i], mean = centres_mean[i, 1], sd = centres_sd[i, 1])
      )
    values_y <-
      c(
        values_y,
        rnorm(n = test_dataset_size*centres_prop[i], mean = centres_mean[i, 2], sd = centres_sd[i, 2])
      )
    values_cluster <-
      c(
        values_cluster,
        rep(as.character(i), test_dataset_size*centres_prop[i])
      )
  }
  test_dataset <- data.frame(
    x = values_x,
    y = values_y,
    cluster = values_cluster
  )

  # plot(
  #   test_dataset$x, test_dataset$y,
  #   col = test_dataset$cluster,
  #   pch = 19,
  #   cex = 0.2
  # )

  clustering_result <-
    sbclust(
      test_dataset[,c("x", "y")],
      centers = 5,
      iter.max = 5000,
    )

  clustering_result_centres <-
    centers.sbclust(clustering_result, 5)

  centres_mean
  clustering_result_centres

  test_dataset["sbclust"] <- clustering_result$cluster

  # plot(
  #   test_dataset$x, test_dataset$y,
  #   col = test_dataset$sbclust,
  #   pch = 19,
  #   cex = 0.2
  # )
})
