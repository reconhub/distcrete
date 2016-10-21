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

test_that("print method, no args", {
  d <- distcrete("norm", 1)
  expect_identical(d, print(d))
  str <- capture.output(print(d))
  expect_match(str, "name: norm", all = FALSE)
  expect_match(str, "with no parameters", all = FALSE)
})

test_that("print method, withargs", {
  d <- distcrete("norm", 1, mean = 1)
  expect_identical(d, print(d))
  str <- capture.output(print(d))
  expect_match(str, "name: norm", all = FALSE)
  expect_match(str, "mean: 1", all = FALSE)

  ## Unnamed:
  expect_equal(capture.output(print(distcrete("norm", 1, 1))), str)
})
