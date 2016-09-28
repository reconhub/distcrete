##' Discretise a distribution.
##' @title Discretise a distribution
##' @param name The name of a distribution function (e.g.,
##'   \code{norm}, \code{gamma}).  The distribution must have a cdf
##'   function (e.g., \code{pnorm}) and a qualtile function (e.g.,
##'   \code{qnorm}) defined.
##'
##' @param interval The interval to discretise the interval onto.
##'
##' @param ... Parameters to \code{cdf}.  Can be matched positionally
##'   or by name.
##'
##' @param w How to weight the endpoints; must be between 0 and 1.  If
##'   0.5 then integration happens centred around the interval, if 0
##'   floor, if 1 then ceiling.
##'
##' @param anchor Any location that is a valid \code{x}
##' @export
##' @author Rich FitzJohn
distcrete <- function(name, interval, ..., w = 0.5, anchor = 0) {
  ## TODO: the offset / reverse parts should be done against the
  ## returned objects because they alter only the range?

  ## TODO: What about non-uniform specification of interval?

  ## NOTE: Once we have Anne's thing in here it's mostly going to be a
  ## case of storing a series of the integrations of u f(u) du for a
  ## range of values; we can immediately do that over basically the
  ## whole range and store it as a vector (do this for all numbers
  ## from (eps to 1-eps) and do the rest on demand).  Alternatively we
  ## could probably just compute it against a spline.

  cdf <- match.fun(paste0("p", name))
  qf <- match.fun(paste0("q", name))
  assert_in_range(w, 0, 1)
  assert_scalar_numeric(anchor)
  d <- list(cdf = function(x, log = FALSE) cdf(x, ..., log.p = log),
            qf = function(x, log = FALSE) qf(x, ..., log.p = log),
            interval = interval,
            w = w,
            anchor = anchor,
            ## offset = 0,
            ## reverse = FALSE,
            parameters = list(...))
  d$d <- function(x, log = FALSE) distcrete_d(d, x, log)
  d$p <- function(q, log = FALSE) distcrete_p(d, q, log)
  d$q <- function(p, log = FALSE) distcrete_q(d, p, log)
  d$r <- function(n = 1) distcrete_r(d, n)
  class(d) <- "distcrete"
  d
}

## The nice thing about this is that it holds for _all_ offsets.  But
## I think that we do want to get this for a given anchor.
distcrete_d <- function(d, x, log = FALSE, strict = FALSE) {
  check_interval(x, d$anchor, d$interval, d$w, strict)

  x0 <- x - d$w * d$interval
  p0 <- d$cdf(x0, log)
  p1 <- d$cdf(x0 + d$interval, log)
  if (log) {
    log_minus(p1, p0)
  } else {
    p1 - p0
  }
}

## TODO: make the argument log.p rather than log?
distcrete_p <- function(d, q, log = FALSE, strict = FALSE) {
  check_interval(q, d$anchor, d$interval, d$w, strict)

  d$cdf(q - d$w * d$interval, log)
}

## TODO: make a %/% function that does this "safely"
##
## NOTE: can get catastrophic information loss on semi-infinite
## distributions with big w
distcrete_q <- function(d, p, log = FALSE) {
  x <- d$qf(p, log)

  n <- find_interval(x, d$anchor, d$interval, d$w, FALSE)
  d$anchor + n * d$interval # + d$w ?
}

distcrete_r <- function(d, n = 1, ...) {
  distcrete_q(d, runif(n))
}

##' @export
print.distcrete <- function(x, ...) {
  cat("A discrete distribution\n")
}

log_minus <- function(lx, ly) {
  lx + log1p(- exp(ly - lx))
}

## This is going to be the workhorse for re-relating continuous 'x'
## onto our new scale.
find_interval <- function(x, anchor, interval, w, strict = FALSE) {
  ## (fabs((x) - R_forceint(x)) > 1e-7*fmax2(1., fabs(x)))
  xs <- x / interval - anchor + w
  ## "safely" drop all the floating point noise
  xs <- zapsmall(xs, -trunc(log10(.Machine$double.eps) / 2))
  if (strict) {
    if (any(xs != floor(xs))) {
      stop("Values do not align")
    }
  } else {
    xs <- floor(xs)
  }
  xs * interval + anchor
}

check_interval <- function(x, anchor, interval, w, strict = FALSE) {
  if (strict) {
    find_interval(x, anchor, interval, w, strict)
  }
}
