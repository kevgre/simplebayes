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
