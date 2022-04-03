#' Sample-based bagged clustering
#'
#' A bagged clustering function derived from the bclust function of the e1071 package but using only a sample of the original dataset.
#' See: https://cran.r-project.org/web/packages/e1071
#'
#' @param x Matrix of inputs (or object of class "bclust" for plot).
#' @param centers, k Number of clusters.
#' @param iter.base Number of runs of the base cluster algorithm.
#' @param minsize Minimum number of points in a base cluster.
#' @param dist.method Distance method used for the hierarchical clustering, see dist for available distances.
#' @param hclust.method Linkage method used for the hierarchical clustering, see hclust for available methods.
#' @param base.method Partitioning cluster method used as base algorithm.
#' @param base.centers Number of centers used in each repetition of the base method.
#' @param verbose Output status messages.
#' @param final.kmeans If TRUE, a final kmeans step is performed using the output of the bagged clustering as initialization.
#' @param docmdscale Logical, if TRUE a cmdscale result is included in the return value.
#' @param resample Logical, if TRUE the base method is run on bootstrap samples of x, else directly on x.
#' @param sample_prop Proportion of the whole dataset to sample. The default value 0.01 results in samples which are 1% of the size of the original dataset.
#' @param weights Vector of length nrow(x), weights for the resampling. By default all observations have equal weight.
#' @param maxcluster Maximum number of clusters memberships are to be computed for.
#' @param ... Optional arguments top be passed to the base method in bclust, ignored in plot.
#'
#' @return return objects of class "bclust" from the e1071 package.
#' @export

"sbclust" <-
  function (x, centers = 2, iter.base = 10, minsize = 0,
            dist.method = "euclidean", hclust.method = "average",
            base.method = "kmeans", base.centers = 20,
            verbose = TRUE, final.kmeans = FALSE, docmdscale=FALSE,
            resample=TRUE, sample_prop = 0.01, weights=NULL, maxcluster=base.centers, ...)
  {
    x <- as.matrix(x)
    xr <- nrow(x)
    xc <- ncol(x)
    CLUSFUN <- get(base.method)

    object <- list(allcenters =
                     matrix(0, ncol = xc, nrow = iter.base * base.centers),
                   allcluster = NULL,
                   hclust = NULL,
                   members = NULL,
                   cluster = NULL,
                   centers = NULL,
                   iter.base = iter.base,
                   base.centers = base.centers,
                   prcomp = NULL,
                   datamean = apply(x, 2, mean),
                   colnames = colnames(x),
                   dist.method = dist.method,
                   hclust.method = hclust.method,
                   maxcluster = maxcluster)

    class(object) <- "bclust"

    optSEM <- getOption("show.error.messages")
    if(is.null(optSEM)) optSEM <- TRUE
    on.exit(options(show.error.messages = optSEM))

    if (verbose) cat("Committee Member:")
    for (n in 1:iter.base) {
      if (verbose){
        cat(" ", n, sep = "")
      }
      x1 <-
        x[
          sample(
            xr,
            # Sample a percentage of the dataset proportional to sample_prop
            # or at least a number of cases equal to base.centers
            size = max(
              base.centers,
              floor(xr*sample_prop)
            ),
            prob = weights
          ),
        ]

      for(m in 1:20){
        if(verbose) cat("(",m,")",sep="")
        options(show.error.messages = FALSE)
        tryres <- try(CLUSFUN(x1, centers = base.centers, ...))
        if(!inherits(tryres, "try-error")) break
      }
      options(show.error.messages = optSEM)
      if(m==20)
        stop(paste("Could not find valid cluster solution in 20 replications\n"))

      object$allcenters[((n - 1) * base.centers + 1):(n * base.centers),] <-
        tryres$centers
    }
    object$allcenters <-
      object$allcenters[complete.cases(object$allcenters),,drop=FALSE]
    object$allcluster <- knn1(object$allcenters, x,
                                     factor(1:nrow(object$allcenters)))

    if(minsize > 0){
      object <- prune.bclust(object, x, minsize=minsize)
    }

    if (verbose)
      cat("\nComputing Hierarchical Clustering\n")
    object <- hclust.bclust(object, x = x, centers = centers,
                            final.kmeans = final.kmeans,
                            docmdscale=docmdscale)
    object
  }

"centers.bclust" <- function (object, k)
{
  centers <- matrix(0, nrow = k, ncol = ncol(object$allcenters))
  for (m in 1:k) {
    centers[m, ] <-
      apply(object$allcenters[object$members[,k-1] == m, , drop = FALSE], 2, mean)
  }
  centers
}

"clusters.bclust" <- function (object, k, x=NULL)
{
  if(missing(x))
    allcluster <- object$allcluster
  else
    allcluster <- knn1(object$allcenters, x,
                       factor(1:nrow(object$allcenters)))

  return(object$members[allcluster, k - 1])
}


"hclust.bclust" <-
  function (object, x, centers, dist.method = object$dist.method,
            hclust.method = object$hclust.method, final.kmeans = FALSE,
            docmdscale = FALSE, maxcluster=object$maxcluster)
  {
    d <- dist(object$allcenters, method = dist.method)
    if(hclust.method=="diana"){
      if (system.file(package = "cluster") == "")
        stop("Could not load required package 'cluster'!")
      object$hclust <- stats::as.hclust(cluster::diana(d, diss=TRUE))
    }
    else
      object$hclust <- stats::hclust(d, method = hclust.method)

    if(docmdscale){
      object$cmdscale <- cmdscale(d)
    }

    object$members <- cutree(object$hclust, 2:maxcluster)
    object$cluster <- clusters.bclust(object, centers)
    object$centers <- centers.bclust(object, centers)
    if (final.kmeans) {
      kmeansres <- kmeans(x, centers = object$centers)
      object$centers <- kmeansres$centers
      object$cluster <- kmeansres$cluster
    }
    object
  }



"plot.bclust" <-
  function (x, maxcluster=x$maxcluster,
            main = deparse(substitute(x)), ...)
  {
    opar <- par(c("mar", "oma"))
    on.exit(par(opar))

    par(oma = c(0, 0, 3, 0))
    layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE))
    par(mar = c(0, 4, 4, 1))
    plot(x$hclust, labels = FALSE, hang = -1)
    x1 <- 1:maxcluster
    x2 <- 2:maxcluster
    y <- rev(x$hclust$height)[x1]
    z <- abs(diff(y))
    par(mar = c(4, 4, 1, 2))
    plot(x1, ((y - min(y))/(max(y) - min(y))), type = "l", xlab = "",
         ylab = "", ylim = c(0, 1))
    lines(x2, z/sum(z), col = "grey")
    text(x2, z/sum(z), labels = as.character(x2))
    #    lx2 <- length(x2)
    #    abline(h=qexp(.95, rate = length(x2)), lty=3, col="grey")
    #    abline(h=qexp(.95^(1/lx2), rate = length(x2)), lty=3, col="grey")
    mtext(main, outer = TRUE, cex = 1.5)
    layout(1)
  }

"boxplot.bclust" <-
  function (x, n = nrow(x$centers), bycluster = TRUE,
            main = deparse(substitute(x)), oneplot=TRUE,
            which=1:n, ...)
  {
    N <- length(which)

    opar <- par(c("mfrow", "oma", "mgp","xpd"))
    on.exit(par(opar))
    par(xpd=NA)

    memb <- x$members[, (n - 1)]
    tmemb <- table(memb)
    cendf <- as.data.frame(x$allcenters)
    ylim <- range(x$allcenters)

    if (bycluster) {
      if(oneplot){
        if (N <= 3) {
          par(mfrow = c(N, 1))
        }
        else {
          par(mfrow = c(ceiling(N/2), 2))
        }
      }
      tcluster <- table(clusters.bclust(x, n))
      for (k in which) {
        boxplot(cendf[memb == k, ], col = "grey",
                names = rep("",ncol(cendf)),
                ylim = ylim, ...)
        if (!is.null(x$datamean)) {
          lines(x$datamean, col = "red")
        }
        if(!is.null(x$colnames)){
          text(1:length(x$colnames)+0.2,
               par("usr")[3],
               adj=1,srt=35,
               paste(x$colnames, "  "))
        }

        title(main = paste("Cluster ", k, ": ", tmemb[k],
                           " centers, ", tcluster[k], " data points", sep = ""))
      }
    }
    else {
      a <- ceiling(sqrt(ncol(cendf)))
      if(oneplot){
        par(mfrow = c(a, ceiling(ncol(cendf)/a)))
      }
      memb <- as.factor(memb)
      for (k in 1:ncol(cendf)) {
        boxplot(cendf[, k] ~ memb, col = "grey", ylim = ylim, ...)
        title(main = x$colnames[k])
        abline(h = x$datamean[k], col = "red")
      }
    }
  }

### prune centers that contain not at least minsize data points
prune.bclust <- function(object, x, minsize=1, dohclust=FALSE, ...){

  ok <- FALSE
  while(!all(ok)){
    object$allcluster <- knn1(object$allcenters, x,
                              factor(1:nrow(object$allcenters)))

    ok <- table(object$allcluster) >= minsize
    object$allcenters <- object$allcenters[ok, ]
  }
  if(dohclust){
    object <- hclust.bclust(object, x, nrow(object$centers), ...)
  }
  object
}
