
<!-- README.md is generated from README.Rmd. Please edit that file -->
distcrete
=========

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![](http://cranlogs.r-pkg.org/badges/distcrete)](http://cran.rstudio.com/packages=distcrete)

[![Travis-CI Build Status](https://travis-ci.org/reconhub/distcrete.svg?branch=master)](https://travis-ci.org/reconhub/distcrete) [![Build status](https://ci.appveyor.com/api/projects/status/uy5out71p9qkh93i/branch/master?svg=true)](https://ci.appveyor.com/project/thibautjombart/distcrete/branch/master) [![codecov.io](https://codecov.io/github/reconhub/distcrete/coverage.svg?branch=master)](https://codecov.io/github/reconhub/distcrete?branch=master)

> Discrete Distribution Approximations

`distcrete` takes a distribution and a set of parameters and returns a list object with functions as elements. Each one is the equivalent to the function calls we typically expect to be able to do in R for a given distribution:

-   Density e.g.`dnorm`
-   Distribution function e.g. `pnorm`
-   Quantile function e.g. `qnorm`
-   Random generation e.g. `rnorm`

Each function created by `distcrete` corresponds to the first letter of the non-discrete equivalent.

``` r
set.seed(415)
d0 <- distcrete::distcrete("gamma", 1, shape = 3, w = 0)
d0$d(1:10)
#>  [1] 0.243022187 0.253486335 0.185086776 0.113451286 0.062683215
#>  [6] 0.032332641 0.015882196 0.007521773 0.003462799 0.001558522
d0$p(c(.1,.5))
#> [1] 0.09958372 0.19115317
d0$q(c(.1,.5))
#> [1] 0 1
d0$r(10)
#>  [1]  2  3  1  2 12  2  2  4  5  3
```

Allowed distributions
---------------------

You can use any distribution that conforms to the following expectations:

-   It has a distribution function like `p[dist name]` available
-   It has a quantile function like `q[dist name]` available

These can be loaded from a package or created on the fly, but must exist when the `distcrete()` function is called.

Installation
------------

You can install it from CRAN with:

``` r
install.packages("distcrete")
```

You can install `distcrete` the most up to date version from github with:

``` r
# install.packages("devtools")
devtools::install_github("reconhub/distcrete")
```

Tests
-----

``` r
devtools::test()
#> Loading distcrete
#> Loading required package: testthat
#> Testing distcrete
#> distcrete: .....................................................................................................................................
#> utils: ...............
#> 
#> DONE ======================================================================
```
