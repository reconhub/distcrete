context("distcrete")

test_that("basic", {
  shape <- 3
  rate <- 1.15

  d <- distcrete("gamma", 1, shape, rate, w = 0.5)
  x <- seq(0, 20, by = d$interval)

  expect_equal(sum(d$d(x)), d$cdf(max(x) + 0.5), tolerance = 1e-14)
  expect_equal(d$q(d$p(x)), x)
})
