context("distcrete")

test_that("basic", {
  shape <- 3
  rate <- 1.15

  for (w in c(0, 0.25, 0.5, 0.99)) {
    d <- distcrete("gamma", 1, shape, rate, w = w)
    x <- seq(0, 20, by = d$interval)

    expect_equal(sum(d$d(x)), d$cdf(max(x) + 1 - w), tolerance = 1e-14)
    px <- d$p(x)
    expect_equal(d$q(px), x)
    expect_equal(d$p(d$q(px)), px)
  }
})

## With a uniform distribution it should be pretty easy to work with
## this:
test_that("boundary conditions", {
  dx <- 0.1
  x <- seq(0, 1, by = dx)
  for (w in c(0, 0.5, 1)) {
    d <- distcrete("unif", dx, w = w)
    p <- d$d(x)
    expect_equal(sum(p), 1)
    n <- length(p)
    i <- c(1, n)
    expect_equal(p[-i], rep(dx, n - 2))
    if (w == 0) {
      pi <- c(dx, 0)
    } else if (w == 0.5) {
      pi <- c(dx, dx) / 2
    } else {
      pi <- c(0, dx)
    }
    expect_equal(p[i], pi)

    ## As above, for the gamma case:
    px <- d$p(x)
    if (w < 1) {
      expect_equal(d$q(px), x)
    } else {
      expect_equal(d$q(px), c(dx, x[-1]))
    }
    expect_equal(d$p(d$q(px)), px)
  }
})

## With an interval != 1, things are tricky:
test_that("invertability of uniform", {
  dx <- 0.1
  x <- seq(0, 1, by = dx)
  for (w in c(0, 0.5, 1)) {
    d <- distcrete("unif", dx, w = w)
    px <- d$p(x)
    if (w < 1) {
      expect_equal(d$q(x), x)
      expect_equal(d$p(d$q(px)), px)
    } else {
      ## The lossyness of the first test makes me think that w should
      ## not be able to be 1 exactly, but should be limited to be '1 -
      ## eps'
      px <- d$p(x)
      expect_equal(d$q(px), c(0.1, x[-1]))
      expect_equal(d$p(d$q(px)), px)
    }
  }
})

test_that("out of bounds", {
  dx <- 0.1
  x <- seq(0, 1, by = dx)
  w <- 0
  d <- distcrete("unif", dx, w = w)

  ## The CDF is defined over the whole interval
  expect_equal(d$p(-1), 0) # same as punif(-1)
  expect_equal(d$p(2), 1) # same as punif(2)

  ## The PDF is also defined over this interval
  expect_equal(d$d(-1), 0) # same as dunif(-1)
  expect_equal(d$d(2), 0) # same as dunif(2)

  ## The quantile function is not though
  expect_equal(suppressWarnings(d$q(-1)), NaN) # same as qunif(-1)
  expect_equal(suppressWarnings(d$q(2)), NaN) # same as dunif(2)
})

test_that("strict mode", {
  dx <- 0.1
  x <- seq(0, 1, by = dx)
  w <- 0
  d <- distcrete("unif", dx, w = w)

  expect_error(d$p(dx / 2, strict = TRUE),
               "Values do not align")
  expect_error(d$d(dx / 2, strict = TRUE),
               "Values do not align")
})

test_that("print method, no args", {
  d <- distcrete("norm", 1)
  capture.output(expect_identical(d, print(d)))
  str <- capture.output(print(d))
  expect_match(str, "name: norm", all = FALSE)
  expect_match(str, "with no parameters", all = FALSE)
})

test_that("print method, withargs", {
  d <- distcrete("norm", 1, mean = 1)
  capture.output(expect_identical(d, print(d)))
  str <- capture.output(print(d))
  expect_match(str, "name: norm", all = FALSE)
  expect_match(str, "mean: 1", all = FALSE)

  ## Unnamed:
  expect_equal(capture.output(print(distcrete("norm", 1, 1))), str)
})
