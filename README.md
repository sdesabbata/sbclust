
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sbclust

<!-- badges: start -->
<!-- badges: end -->

The goal of sbclust is to provide an approach to cluster large datasets
through a sample-based bagged clustering algorithm, which is effectively
just the `bclust` function from the
[e1071](https://cran.r-project.org/web/packages/e1071/index.html)
package with one tiny edit that allows to grab a sample of the dataset
for each run instead of the whole dataset.

## Installation

You can install the development version of sbclust from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sdesabbata/sbclust")
```

## Example

This is a basic example of clustering a large dataset using `sbclust`:

``` r
library(sbclust)
```

Create test dataset containing three million cases with two attributes
and five clusters, using approach suggested by [Wang et
al. (2008)](https://onlinelibrary.wiley.com/doi/10.1002/int.20268).

``` r
# Size
test_dataset_size <- 3000000

# Proportions of the five clusters
centres_prop <- c(0.2, 0.2, 0.3, 0.1, 0.2)

# Clusters' centers
centres_mean <- array(
  c(-3, 0, 4, 5, -5, -3, 0, 3, -2, 4),
  c(5, 2)
)

# Clusters standard deviation
centres_sd <- array(
  c(1, 1, 0.1, 0.5, 0.2, 0.2, 1, 1, 1, 1),
  c(5, 2)
)

# Values dataset
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

# Create dataset
test_dataset <- data.frame(
  x = values_x,
  y = values_y,
  cluster = values_cluster
)
```

Plot the example dataset.

``` r
plot(
  test_dataset$x, test_dataset$y,
  col = test_dataset$cluster,
  pch = 19,
  cex = 0.2
)
```

<img src="man/figures/README-plot_dataset-1.png" width="100%" />

Run the clustering algorithm and save the results as a new column of the
dataset.

``` r
# Clustering
start_time <- Sys.time()

clustering_result <-
  sbclust(
    test_dataset[,c("x", "y")],
    centers = 5,
    iter.max = 5000,
  )
#> Committee Member: 1(1) 2(1) 3(1) 4(1) 5(1) 6(1) 7(1) 8(1) 9(1)
#> Warning: Quick-TRANSfer stage steps exceeded maximum (= 1500000)
#>  10(1)
#> Computing Hierarchical Clustering

end_time <- Sys.time()

# Check time lapsed
end_time - start_time
#> Time difference of 2.874979 secs

# Save results
test_dataset["sbclust"] <- clustering_result$cluster
```

Plot the clustering results.

``` r
plot(
  test_dataset$x, test_dataset$y,
  col = test_dataset$sbclust,
  pch = 19,
  cex = 0.2
)
```

<img src="man/figures/README-plot_results-1.png" width="100%" />

Compare the list of test centers with the results of the clustering
procedure.

``` r
# Comparison
centres_mean
#>      [,1] [,2]
#> [1,]   -3   -3
#> [2,]    0    0
#> [3,]    4    3
#> [4,]    5   -2
#> [5,]   -5    4
clustering_result$centers
#>             [,1]        [,2]
#> [1,]  4.00117785  2.95504323
#> [2,]  4.99936706 -2.06528615
#> [3,] -2.97749694 -2.98535817
#> [4,]  0.02980787  0.02093155
#> [5,] -4.99781581  3.98430610
```

Run the `e1071::bclust` algorithm for comparison.

``` r
# Clustering
e1071_bclust_start_time <- Sys.time()

e1071_bclust_result <-
  e1071::bclust(
    test_dataset[,c("x", "y")],
    centers = 5,
    iter.max = 5000,
  )
#> Committee Member: 1(1)
#> Warning: Quick-TRANSfer stage steps exceeded maximum (= 150000000)
#>  2(1)
#> Warning: Quick-TRANSfer stage steps exceeded maximum (= 150000000)
#>  3(1)
#> Warning: Quick-TRANSfer stage steps exceeded maximum (= 150000000)
#>  4(1)
#> Warning: Quick-TRANSfer stage steps exceeded maximum (= 150000000)
#>  5(1)
#> Warning: Quick-TRANSfer stage steps exceeded maximum (= 150000000)
#>  6(1)
#> Warning: Quick-TRANSfer stage steps exceeded maximum (= 150000000)
#>  7(1)
#> Warning: Quick-TRANSfer stage steps exceeded maximum (= 150000000)
#>  8(1)
#> Warning: Quick-TRANSfer stage steps exceeded maximum (= 150000000)
#>  9(1)
#> Warning: Quick-TRANSfer stage steps exceeded maximum (= 150000000)
#>  10(1)
#> Warning: Quick-TRANSfer stage steps exceeded maximum (= 150000000)
#> 
#> Computing Hierarchical Clustering

e1071_bclust_end_time <- Sys.time()

# Check time lapsed
e1071_bclust_end_time - e1071_bclust_start_time
#> Time difference of 48.94187 secs

# Save results
test_dataset["e1071_bclust"] <- e1071_bclust_result$cluster
```

Plot the clustering results.

``` r
plot(
  test_dataset$x, test_dataset$y,
  col = test_dataset$e1071_bclust,
  pch = 19,
  cex = 0.2
)
```

<img src="man/figures/README-plot_results_e1071_bclust-1.png" width="100%" />

Compare the list of test centers with the results of the clustering
procedure.

``` r
# Comparison
centres_mean
#>      [,1] [,2]
#> [1,]   -3   -3
#> [2,]    0    0
#> [3,]    4    3
#> [4,]    5   -2
#> [5,]   -5    4
e1071_bclust_result$centers
#>             [,1]        [,2]
#> [1,]  0.01850614 -0.05947054
#> [2,] -3.00438862 -2.98098877
#> [3,] -4.99873225  3.99163683
#> [4,]  4.00214013  3.02187993
#> [5,]  4.98028792 -1.98272758
```

The examples above illustrates how, when working with large datasets,
`sbclust` can achieve similar results as `e1071::bclust` in a fraction
(5.87%) of the time.
