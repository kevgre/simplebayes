
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

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
