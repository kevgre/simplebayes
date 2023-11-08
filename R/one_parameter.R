dbeta_post <- function(
    x, successes, sample_size, ..., alpha_prior = 1/2, beta_prior = 1/2
    ) {
  failures <- sample_size - successes
  stats::dbeta(x = x, shape1 = successes + alpha_prior, shape2 = failures + beta_prior)
}
