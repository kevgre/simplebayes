
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simplebayes

<!-- badges: start -->
<!-- badges: end -->

The goal of simplebayes is to allow the user to perform simple tasks in
Bayesian statistics without requiring him or her to learn how to use a
complex package such as Stan. This will allow the user to focus on the
results of the model being built instead of how to build the model
itself.

## Installation

You can install the development version of simplebayes from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kevgre/simplebayes")
```

## Example

A common task in Bayesian statistics is to find the probability of a
certain value in the posterior distribution given a binomial likelihood.
The first step is to choose a prior. For a binomial likelihood, a common
choice of prior is the beta distribution. With this, the posterior can
be found as follows given some data:

``` r
library(simplebayes)
successes <- 20
sample_size <- 100

pbeta_post(0.3, successes, sample_size, likelihood = "binom")
#> [1] 0.9878169
```
