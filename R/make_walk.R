#' Creates a random walk path.
#'
#' This function creates a two-dimensional movement path as a series of steps.
#' Movement can be correlated (current step length and direction depend on
#' previous step) or uncorrelated. The initial position and orientation of the
#' moving object are given by the arguments \code{x0, y0, dir0}, while the
#' length and direction of each step, together with the conditions for accepting
#' a proposed step and detecting when movement should end are given by functions
#' supplied as arguments. The algorithm proceeds as follows:
#' \enumerate{
#' \item The \code{stop.cond} function is called to check if movement is
#' finished. Note: the first check takes place before any steps have been taken
#' so a function can reject an unacceptable start position or orientation.
#' \item An angle is drawn from the function \code{next.angle} and the
#' object's orientation is updated. If \code{turning.angles} is \code{TRUE} the
#' angle is treated as a turning angle which is added to the previous orientation.
#' If \code{turning.angles} is \code{FALSE}, the object's new orientation is simply
#' set to the angle. In both cases, the new orientation is normalized to (-pi, pi].
#' \item A step length is chosen using function \code{next.step}.
#' \item A new candidate position is calculated for the given orientation and
#' step length.
#' \item The candidate position is tested with the \code{accept.pos} function;
#' if accepted, the object moves to this position; otherwise a new candidate
#' position is chosen. If no position is accepted after \code{accept.limit}
#' attempts, the movement terminates.
#' }
#' This process continues until the \code{stop.cond} function signals completion
#' or no more candidate positions can be found.
#'
#' @param x0,y0 Ordinates of the starting location.
#' @param dir0 Initial orientation (radians). If NULL or missing, a random
#'   angle in the interval (-pi, pi] will be used.
#' @param next.angle A function to generate angles. The function should take
#'   a single argument (a walk.data object).
#' @param next.steplen Either a function to generate step lengths, or a numeric
#'   value for constant step length. If a function, it should take a single argument
#'   (a walk.data object).
#' @param accept.pos A function to test if a candidate position is acceptable.
#'   The function should take two arguments: \code{x, y} ordinates of the proposed
#'   position.
#' @param stop.cond A function to test if movement is finished.
#'   The function should take a single argument (a walk.data object).
#' @param turning.angles If \code{TRUE} (default), angles from the \code{next.angle}
#'   function are taken as turning angles, and the orientation of the moving
#'   object at each step is the sum of its previous orientation and the turning
#'   angle. This results in a correlated random walk. If \code{FALSE}, angles
#'   are taken as the new orientation for each step.
#' @param accept.limit Maximum number of candidate positions to test at each move.
#'   If exceeded, the movement is finished.
#' @param id An identifer (integer or character) to store in the \code{id} field
#'   of the returned \code{walk.data} object. If missing or NULL it will be set to 1.
#'
#' @return A named list (class \code{walk.data}) with elements:
#' \describe{
#' \item{id}{identifier (integer or character)}
#' \item{x}{vector of X ordinates}
#' \item{y}{vector of Y ordinates}
#' \item{dir}{vector of orientations (radians)}
#' \item{xstart}{first X ordinate (same as \code{x[1]})}
#' \item{xend}{last X ordinate (same as \code{x[ length(x) ]})}
#' \item{ystart}{first Y ordinate (same as \code{y[1]})}
#' \item{yend}{last Y ordinate (same as \code{y[ length(y) ]})}
#' \item{steplen}{vector of step lengths (first element will be 0)}
#' \item{dist.direct}{direct line distance from the start to the end of the path}
#' \item{dist.path}{distance along the path (sum of step lengths)}
#' \item{nsteps}{number of steps taken}
#' \item{status}{one of: \code{'active'} while movement is continuing (ie. when
#'   walk data is passed to angle, step length and stopping functions);
#'   \code{'finished'} if movement terminated normally;
#'   \code{'failed'} if movement terminated because no further acceptable positions
#'   could be found.}
#' }
#'
#' @examples
#' \dontrun{
#' # Start position and orientation
#' x0 <- 0
#' y0 <- 0
#' d0 <- runif(1, -pi, pi)
#'
#' # Function to draw turning angles from a truncated Laplace distribution
#' # (walk.data argument is ignored)
#' angle <- function(wdat) rtrunclaplace(1, 0, 1, -pi, pi)
#'
#' # Function for step lengths (walk.data argument ignored)
#' steplen <- function(wdat) 1.0
#'
#' # Function to test acceptance of a proposed new position.
#' # As an example, we will reject any positions below a given Y.
#' posfn <- function(x, y) y > 10
#'
#' # Function to test if movement is finished.
#' # As an example, we stop when either the direct distance from
#' # the start point or the total path distance exceed given values.
#' stopfn <- function(wdat) wdat$dist.direct > 20 || wdat$dist.path > 50
#'
#' w <- make_walk(
#'   x0, y0, d0,
#'   next.angle = anglefn,
#'   next.step = lenfn,
#'   accept.pos = posfn,
#'   stop.cond = stopfn)
#'
#' # simple plot of the movement path with base graphics
#' with(w, plot(x, y, type="l"))
#' }
#'
#' @export
#'
make_walk <- function(x0, y0, dir0,
                      next.angle,
                      next.steplen = 1.0,
                      accept.pos = function(x, y) TRUE,
                      stop.cond = function(wdat) wdat$nsteps >= 100,
                      turning.angles = TRUE,
                      accept.limit = 100,
                      id = 1) {

  if (missing(dir0) || is.null(dir0))
    dir0 <- .normalize_angle(runif(1, -pi, pi))

  if (missing(next.angle) || !is.function(next.angle))
    stop("next.angle argument must be provided as a function")

  if (is.numeric(next.steplen)) {
    len <- next.steplen[1]
    next.steplen <- function(...) len
  }
  else if (!is.function(next.steplen))
    stop("next.steplen argument must a numeric value or a function")

  if (missing(id) || is.null(id))
    id <- 1
  else if ( !(is.integer(id) || is.character(id)) )
    stop("id must be an integer or character value")

  if (length(id) > 1) {
    warning("length(id) > 1; only using first element")
    id <- id[1]
  }

  w <- list(id = id,
            nsteps = 0,
            x = x0,
            y = y0,
            xstart = x0,
            xend = x0,
            ystart = y0,
            yend = y0,
            steplen = 0,
            dir = dir0,
            dist.direct = 0,
            dist.path = 0,
            status = "active")

  class(w) <- "walk.data"

  i <- 1
  repeat {
    if (stop.cond(w)) {
      w$status <- "finished"
      break
    }

    if (turning.angles)
      a <- .normalize_angle( w$dir[i] + next.angle(w) )
    else
      a <- .normalize_angle( next.angle(w) )

    len <- next.steplen(w)

    try <- 0
    ok <- FALSE
    while (try < accept.limit && !ok) {
      x <- w$x[i] + len * cos(a)
      y <- w$y[i] + len * sin(a)
      if (accept.pos(x, y))
        ok <- TRUE
      else
        try <- try + 1
    }

    if (!ok) {
      w$status <- "failed"
      break
    }

    w$x <- c(w$x, x)
    w$xend <- x
    w$y <- c(w$y, y)
    w$yend <- y
    w$steplen <- c(w$steplen, len)

    w$dir <- c(w$dir, a)
    w$nsteps <- i

    w$dist.path <- w$dist.path + len
    w$dist.direct <- sqrt( (x0 - x)^2 + (y0 - y)^2 )

    i <- i + 1
  }

  w
}


.normalize_angle <- function(a) {
  while (a < -pi) a <- a + 2*pi
  while (a > pi) a <- a - 2*pi
  a
}
