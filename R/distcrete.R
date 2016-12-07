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
  d$d <- function(x, log = FALSE, strict = FALSE) distcrete_d(d, x, log, strict)
  d$p <- function(q, log = FALSE, strict = FALSE) distcrete_p(d, q, log, strict)
  d$q <- function(p, log = FALSE) distcrete_q(d, p, log)
  d$r <- function(n = 1) distcrete_r(d, n)
  d$name <- name
  class(d) <- "distcrete"
  d
}

## The nice thing about this is that it holds for _all_ offsets.  But
## I think that we do want to get this for a given anchor.
distcrete_d <- function(d, x, log = FALSE, strict = FALSE) {
  ## TODO: what is the logic here?  This seems like a 1st order
  ## appoximation perhaps?  Worth documenting this properly somewhere
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
  d$cdf(q + (1 - d$w) * d$interval, log)
}

## TODO: I'm still not 100% sure about the -1 here!
distcrete_q <- function(d, p, log = FALSE) {
  x <- d$qf(p, log)
  find_interval(x, d$anchor, d$interval, d$w, FALSE) - d$interval
}

## TODO: consider a lower/uppper argument to _q so that this
## (+interval-interval) bit can be avoided...
distcrete_r <- function(d, n = 1, ...) {
  distcrete_q(d, runif(n)) + d$interval
}

##' @export
print.distcrete <- function(x, ...) {
  cat("A discrete distribution\n")
  cat(sprintf("  name: %s\n", x$name))
  p <- x$parameters
  ## This might collect a few too many parameters, but will at least
  ## capture defaults.
  if (is.null(names(p)) || !all(nzchar(names(p)))) {
    f <- environment(x$cdf)$cdf
    p <- as.list(match.call(f, as.call(c(list(quote(f)), list(1), p))))[-(1:2)]
  }
  if (length(p) == 0L) {
    cat("  <with no parameters>\n")
  } else {
    cat(sprintf("  parameters:\n"))
    to_str <- function(x) if (length(x) == 1) as.character(x) else ""
    cat(sprintf("    %s: %s\n", names(p), vapply(p, to_str, character(1))),
        sep="")
  }
  invisible(x)
}

## TODO: find a reference for this bit of hackery:
log_minus <- function(lx, ly) {
  dyx <- ly - lx
  lx + ifelse(dyx > -log(2), log(-expm1(dyx)), log1p(-exp(dyx)))
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
