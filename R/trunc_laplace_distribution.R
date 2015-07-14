#' The truncated Laplace distribution
#'
#' Density, distribution function and random generation for the truncated
#' Laplace distribution.
#'
#' @rdname trunc_laplace_distribution
#' @name Laplace (truncated)
#'
#' @param x, q vector of quantiles
#' @param mean vector of means
#' @param scale vector of scale parameters (> 0)
#' @param lower lower bound of distribution
#' @param upper upper bound of distribution
#' @param n number of observations. If length(n) > 1, the length is taken
#'   to be the number required
NULL

#' @rdname trunc_laplace_distribution
#' @export
dtrunclaplace <- function(x, mean, scale, lower, upper) {
  p <- numeric(length(x))
  ii <- x > lower & x <= upper
  p[ii] <- dlaplace(x[ii], mean, scale) /
    (plaplace(upper, mean, scale) - plaplace(lower, mean, scale))
  p
}

#' @rdname trunc_laplace_distribution
#' @export
ptrunclaplace <- function(q, mean, scale, lower, upper) {
  p <- numeric(length(q))
  ii <- q > lower & q <= upper

  p[ii] <- plaplace(q[ii], mean, scale)

  ii <- q > lower & q < mean
  p[ii] <- p[ii] - plaplace(lower, mean, scale) * ((mean - q[ii]) / (mean - lower))

  ii <- q > mean & q < upper
  p[ii] <- p[ii] + (1 - plaplace(upper, mean, scale)) * ((q[ii] - mean) / (upper - mean))

  p[q >= upper] <- 1.0

  p
}

#' @rdname trunc_laplace_distribution
#'
#' @examples
#' \dontrun{
#' # Compare the distribution of random angles from a truncated
#' # Laplace versus those from a standard Laplace distribution.
#' N <- 1000
#' as <- rlaplace(N, mean=0 scale=1.0)
#' at <- rtrunclaplace(N, mean=0, scale=1.0, lower=-pi/2, upper=pi/2)
#'
#' library(ggplot2)
#' dat <- data.frame(angle = c(as, at), distribution = rep(c("standard", "truncated"), each=N))
#' ggplot() +
#'   geom_histogram(data=dat, aes(x=angle, fill=distribution), position="dodge", binwidth=pi/10) +
#'   theme_bw()
#' }
#'
#' @export
#'
rtrunclaplace <- function(n, mean, scale, lower, upper) {
  r.out <- numeric(n)
  nrem <- n
  begin <- 1

  repeat {
    ntake <- max(nrem, 100)
    r <- rlaplace(ntake, mean, scale)
    ii <- r > lower & r <= upper

    nr <- sum(ii)
    end <- min(n, begin + nr - 1)
    r.out[begin:end] <- r[ii][1:min(nr, nrem)]

    nrem <- n - end
    if (nrem <= 0)
      break
    else {
      begin <- end + 1
    }
  }

  r.out
}
