rgamma_post <- function(n, x, sample_size, ..., alpha_prior, beta_prior) {
  x <- sum(x)
  stats::rgamma(n = n, shape = x + alpha_prior, rate = sample_size + beta_prior)
}
