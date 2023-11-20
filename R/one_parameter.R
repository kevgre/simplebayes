#' Sample Posterior Beta PDF
#'
#' `dbeta_post()` will sample the posterior beta probability distribution function
#' in a beta-binomial model.
#'
#' @param quantile vector of quantiles
#' @param successes number of success in the binomial experiment
#' @param sample_size number of trials in the binomial experiment
#' @param ... Placeholder for future capabilities. Currently ignored.
#' @param alpha_prior The prior for the alpha parameter
#' @param beta_prior The prior for the beta parameter
#'
#' @returns A vector of probabilities the same length as quantile
#' @export
#'
#' @examples
dbeta_post <- function(
    quantile, successes, sample_size, ..., alpha_prior = 1/2, beta_prior = 1/2
    ) {
  failures <- sample_size - successes
  stats::dbeta(quantile, successes + alpha_prior, failures + beta_prior, ...)
}

#' Sample from beta CDF
#'
#' Samples from the Cumulative Density Function for the beta distribution.
#'
#' @param quantile Vector of quantiles
#' @param successes The number of successes in the binomial experiment
#' @param sample_size The sample size for the binomial experiment
#' @param ... Pass additional arguments to the pbeta function
#' @param alpha_prior The prior value for alpha
#' @param beta_prior The prior value for beta
#'
#' @returns A vector of probabilities the same length as the quantiles
#' @export
#'
#' @examples
pbeta_post <- function(
    quantile, successes, sample_size, ..., alpha_prior = 1/2, beta_prior = 1/2
    ) {
  failures <- sample_size - successes
  stats::pbeta(quantile, successes + alpha_prior, failures + beta_prior, ...)
}

# TODO: update documentation to appear in a single file
rbeta_post <- function(
    n, successes, sample_size, ..., alpha_prior = 1/2, beta_prior = 1/2
) {
  failures <- sample_size - successes
  stats::rbeta(n = n, successes + alpha_prior, failures + beta_prior, )
}

# TODO: add Jefferies priors
dgamma_post <- function(quantile, x, sample_size, ..., alpha_prior, beta_prior) {
  x <- sum(x)
  stats::dgamma(quantile, x + alpha_prior, rate = sample_size + beta_prior, ...)
}

pgamma_post <- function(quantile, x, sample_size, ..., alpha_prior, beta_prior) {
  x <- sum(x)
  stats::pgamma(quantile, x + alpha_prior, sample_size + beta_prior, ...)
}

rgamma_post <- function(n, x, sample_size, ..., alpha_prior, beta_prior) {
  x <- sum(x)
  stats::rgamma(n = n, shape = x + alpha_prior, rate = sample_size + beta_prior)
}
