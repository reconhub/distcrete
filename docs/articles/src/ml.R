## ---
## title: "ML estimation with distcrete"
## author: "Thibaut Jombart"
## date: "`r Sys.Date()`"
## output: rmarkdown::html_vignette
## vignette: >
##   %\VignetteIndexEntry{distcrete_ML}
##   %\VignetteEngine{knitr::rmarkdown}
##   %\VignetteEncoding{UTF-8}
## ---

##+ echo = FALSE, results = "hide"
knitr::opts_chunk$set(
  error = FALSE,
  fig.width = 7,
  fig.height = 5)
set.seed(1)


## In this vignette, we illustrate how *distcrete* can be combined with
## classical optimisation procedures available in R to derive maximum-likelihood
## (ML) of parameters of distributions of interest. We simulate simple data from
## a discretised exponential distribution, and then attempt to recover the
## original parameters using ML.


## ## Simulating data

## Simulating data from a discretised distribution is very easy using
## *distcrete*, as discretised distributions already contain a random variate
## generator in the `$r` component. We illustrate this by generating a
## discretised exponential distribution `x` with rate parameter 0.123:
library(distcrete)
rate <- 0.123
plot(function(x) dexp(x, rate), xlim = c(0, 60),
     main = "Original Exponential distribution")
x <- distcrete("exp", interval = 1L, rate)
x$r


## We simulate 200 draws from the distribution `x`:
##+ simulation
set.seed(1)
sim <- x$r(200)
head(sim)
summary(sim)
plot(table(sim), lwd=3, xlab = "x", ylab = "Frequency",
     main = "Simulated sample")




## ## ML estimation

## We will use the base function `optim` to find maximum likelihood estimates of
## the rate of the distribution `x`. The log-likelihood function to be optimised
## can be written as:

## + target
ll <- function(param) {
    d <- distcrete("exp", interval = 1L, param)$d
    sum(d(sim, log = TRUE))
}


## For instance, the log-likelihood with shapes and rates of 1.234 can be computed as:
##+ ll_example
param <- c(rate = 1.234)
ll(param)



## We can feed this function and initial parameters to `optimise`:
##+ optimise
opt <- optimise(ll, c(0, 20), maximum = TRUE)
opt


## The ML estimate for the rate of the distribution is `r opt$maximum`, which is
## close enough to the original value of `r rate`.




## ## Discretised gamma example

## We repeat the same exercise with a gamma distribution. The only difference
## with the previous example is that `optim` needs to be used, as the parameter
## space has now 2 dimensions.

## We start by simulating a sample from a discretised gamma distribution:
##+ gamma
shape <- 10
rate <- 1.2
y <- distcrete("gamma", interval = 1L, shape, rate)
set.seed(1)
sim2 <- y$r(300)

head(sim2)
summary(sim2)
plot(table(sim2), lwd = 10, xlab = "x", ylab = "Frequency",
     main = "Simulated sample")



## We create a log-likelihood function for these data, as well as a deviance (it
## is easier to minimise the deviance with `optim`):
##+ ll_gamma
ll2 <- function(param) {
  if (param[[2]] <= 0) { # really, the '=' part is contentious...
    -Inf
  } else {
    d <- distcrete("gamma", interval = 1L, param[1], param[2])$d
    sum(d(sim2, log = TRUE))
  }
}

dev2 <- function(param) -2 * ll2(param)

optim(c(1,1), dev2)


## We can verify that the estimates are not dependent on the initial state:
##+ verif
optim(c(0.5,20), dev2)


## Finally, we can plot the likelihood surface to see how tightly the distribution fits the data:

x <- seq(0, 20, length = 30)
y <- seq(0, 5, length = 30)
grid <- expand.grid(x, y)
names(grid) <- c("shape", "rate")
ll2.val <- apply(grid, 1, ll2)
df <- cbind.data.frame(grid, ll = ll2.val)

library(ggplot2)
ggplot(df, aes(x = shape, y = rate, fill = ll)) +
    geom_raster() +
    geom_contour(aes(z = ll), color = "black")

## Interestingly, it seems that quite a few combinations of parameters of the
## gamma distribution can lead to similar fits. Let us examine a few:

param1 <- c(5, 0.5)
param2 <- c(20, 2)

d1 <- distcrete("gamma", interval = 1L, param1[1], param1[2])
d2 <- distcrete("gamma", interval = 1L, param2[1], param2[2])

plot(table(d1$r(300)), lwd = 10, xlab = "x", ylab = "Frequency",
           main = "shape: 5, rate: 0.1")

plot(table(d2$r(300)), lwd = 10, xlab = "x", ylab = "Frequency",
           main = "shape: 20, rate: 2")

