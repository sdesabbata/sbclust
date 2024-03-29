% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sbclust.R
\name{sbclust}
\alias{sbclust}
\title{Sample-based bagged clustering}
\usage{
sbclust(
  x,
  centers = 2,
  iter.base = 10,
  minsize = 0,
  dist.method = "euclidean",
  hclust.method = "average",
  base.method = "kmeans",
  base.centers = 20,
  verbose = TRUE,
  final.kmeans = FALSE,
  docmdscale = FALSE,
  resample = TRUE,
  sample_prop = 0.01,
  weights = NULL,
  maxcluster = base.centers,
  ...
)
}
\arguments{
\item{x}{Matrix of inputs (or object of class "bclust" from the e1071 package for plot).}

\item{centers, }{k Number of clusters.}

\item{iter.base}{Number of runs of the base cluster algorithm.}

\item{minsize}{Minimum number of points in a base cluster.}

\item{dist.method}{Distance method used for the hierarchical clustering, see dist for available distances.}

\item{hclust.method}{Linkage method used for the hierarchical clustering, see hclust for available methods.}

\item{base.method}{Partitioning cluster method used as base algorithm.}

\item{base.centers}{Number of centers used in each repetition of the base method.}

\item{verbose}{Output status messages.}

\item{final.kmeans}{If TRUE, a final kmeans step is performed using the output of the bagged clustering as initialization.}

\item{docmdscale}{Logical, if TRUE a cmdscale result is included in the return value.}

\item{resample}{Logical, if TRUE the base method is run on bootstrap samples of x, else directly on x.}

\item{sample_prop}{Proportion of the whole dataset to sample. The default value 0.01 results in samples which are 1\% of the size of the original dataset.}

\item{weights}{Vector of length nrow(x), weights for the resampling. By default all observations have equal weight.}

\item{maxcluster}{Maximum number of clusters memberships are to be computed for.}

\item{...}{Optional arguments top be passed to the base method in sbclust, ignored in plot.}
}
\value{
return objects of class "bclust" from the e1071 package.
}
\description{
A bagged clustering function derived from the bclust function of the e1071 package but using only a sample of the original dataset.
See: https://cran.r-project.org/web/packages/e1071
}
