#' Draws a random turning angle from a truncated Laplace distribution.
#'
#' Draws a random turning angle in the range (-pi, pi] from a truncated
#' Laplace distribution with the given scale. This is a short-cut for
#' \code{rtrunclaplace(1, 0, scale, -pi, pi)}.
#'
#' @param scale scale parameter for the truncated Laplace distribution
#'
#' @return random angle (radians).
#'
#' @seealso \code{\link{rtrunclaplace}}
#'
#' @export
#'
angle_laplace <- function(scale = 1) {
  rtrunclaplace(1, 0, scale, -pi, pi)
}
