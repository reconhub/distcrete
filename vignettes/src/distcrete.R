## ---
## title: "distcrete"
## author: "Rich FitzJohn"
## date: "`r Sys.Date()`"
## output: rmarkdown::html_vignette
## vignette: >
##   %\VignetteIndexEntry{distcrete}
##   %\VignetteEngine{knitr::rmarkdown}
##   %\VignetteEncoding{UTF-8}
## ---

##+ echo = FALSE, results = "hide"
knitr::opts_chunk$set(
  error = FALSE,
  fig.width = 7,
  fig.height = 5)
set.seed(1)

## `distcrete` discretises probability distributions.

## ## Why?

## This vignette needs a narative angle added by someone who
## understands how these models are used in practice

## ## How it works

## This section covers both the _interface_ in the package and the
## ideas behind how the distributions are generated.  There are many
## possible ways of discretising a given distribution onto a set of
## values and this section tries to justify the approaches taken here
## and explain how the user can modify them.

## ### Mapping continuous values to an underlying discrete grid

## This section applies to all the content below.  Suppose we have a
## continuous distribution that is defined on some domain [a, b]
## (possibly infinite).  For example, a gamma distribution with shape
## 3 and rate 1:
curve(dgamma(x, 3), 0, 10, n = 301)

## To discretise this we define:

## * `interval`: the (uniform) frequency of discretisation; perhaps 1.
##   There is no default here as you have to decide how your
##   distribution is to be discretised.
##
## * `anchor`: a single value point at which discretisation is valid;
##   perhaps 0 (which is the default).  Once an anchor is specified we
##   know that valid 'x' positions are {..., `anchor - 2 * interval`,
##   `anchor - interval`, `anchor`, `anchor + interval`, `anchor + 2 *
##   interval`, ...} where those are on the domain of our underlying
##   distribution.
##
## * `w`: how to collapse intermediate values, and probabilities into
##   the interval.  Given that we want a discrete probability mass for
##   position `x` (which is one of the valid values above), this
##   defines the domain of the underlying function that we integrate
##   over.  Specifically we integrate from `x - w * interval` to `x -
##   (1 - w) * interval`.  So specifying `interval = 1, anchor = 0, w
##   = 0` means that looking up position `0` integrates the
##   dstribution over `[0, 1]`.  In contrast specifying `w = 0.5` will
##   integrate over `[-0.5, 0.5]`.

## To illustrate:
d0 <- distcrete::distcrete("gamma", 1, shape = 3, w = 0)
d1 <- distcrete::distcrete("gamma", 1, shape = 3, w = 0.5)
x <- 0:10
y0 <- d0$d(x)
y1 <- d1$d(x)

par(mar=c(4.1, 4.1, 0.5, 0.5))
col <- c("gold", "steelblue3")
r <- barplot(rbind(y0, y1), names.arg = 0:10, beside = TRUE,
             xlab = "x", ylab = "Probability mass", las = 1,
             col = col)
legend("topright", c("w = 0", "w = 0.5"), bty = "n", fill = col)

## Summed over all 'x', these two distributions will both equal 1
sum(d0$d(0:100))
sum(d1$d(0:100))

## but the way that probability distribution is spready over the
## discrete 'x' locations differs.

## ### Cumulative density function

## The cumulative density function (CDF) is key to all calculations
## throughout.

## ### Probability density function to probability mass function

## We have an underlying continuous distribution with a known
## discretised CDF $F_d(x)$.  To compute the density at $x$ we compute
## how much probability is consumed between `x` and `x + interval`, we
## can compute $F_d(x + interval) - F_d(x)$.
