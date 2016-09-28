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
