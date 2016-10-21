assert_in_range <- function(x, min, max, name = deparse(substitute(x))) {
  assert_scalar_numeric(x, name)
  if (x < min || x > max) {
    stop(sprintf("%s must be in range [%s, %s]", name, min, max))
  }
}

assert_scalar <- function(x, name=deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", name), call.=FALSE)
  }
}

assert_numeric <- function(x, name=deparse(substitute(x))) {
  if (!is.numeric(x)) {
    stop(sprintf("%s must be numeric", name), call.=FALSE)
  }
}

assert_nonmissing <- function(x, name=deparse(substitute(x))) {
  if (any(is.na(x))) {
    stop(sprintf("%s must not be NA", name), call.=FALSE)
  }
}

assert_function <- function(x, name=deparse(substitute(x))) {
  if (!is.function(x)) {
    stop(sprintf("%s must be a function", name), call.=FALSE)
  }
}

assert_scalar_numeric <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_numeric(x, name)
  assert_nonmissing(x, name)
}
