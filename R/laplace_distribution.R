#' The Laplace distribution
#'
#' Density, distribution function, quantile function and random generation for the
#' Laplace distribution.
#'
#' @rdname laplace_distribution
#' @name Laplace
#'
#' @param x, q vector of quantiles
#' @param mean vector of means
#' @param scale vector of scale parameters (> 0)
#' @param n number of observations. If length(n) > 1, the length is taken
#'   to be the number required
NULL

#' @rdname laplace_distribution
#' @export
dlaplace <- function(x, mean, scale) {
  0.5 * stats::dexp(abs(x - mean), 1 / scale)
}

#' @rdname laplace_distribution
#' @export
plaplace <- function(q, mean, scale) {
  p <- q
  ii <- q >= mean
  p[ii] <- (1 + stats::pexp(q[ii] - mean, 1/scale)) / 2
  p[!ii] <- 1 - (1 + stats::pexp(mean - q[!ii], 1/scale)) / 2
  p
}

#' @rdname laplace_distribution
#' @export
rlaplace <- function(n, mean, scale) {
  sgn <- sample(c(-1,1), n, replace=TRUE)
  mean - scale * sgn * log(runif(n))
}
