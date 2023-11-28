compute_beta_posterior_vals <- function(
    a_prior, b_prior, likelihood, ...,
    result = NULL, s_size = NULL, f_rate = NULL) {
  if (likelihood == "binom") {
    alpha_post <- result + a_prior
    beta_post <- s_size - result + b_prior
  }
  if (likelihood == "nbinom") {
    alpha_post <- a_prior + f_rate * s_size
    beta_post <- b_prior + result
  }
  if (likelihood == "geometric") {
    alpha_post <- a_prior + s_size
    beta_post <- b_prior + result
  }
  c(alpha_post, beta_post)
}

# dbeta_post <- function(quantile, likelihood, ..., alpha_prior = 0.5, beta_prior = 0.5) {
#   rlang::arg_match(likelihood, c("binom", "nbinom", "geometric"))
#
#   dbeta(quantile, alpha_post, beta_post, ...)
# }
