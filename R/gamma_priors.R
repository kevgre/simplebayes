update_gamma_parameters <- function(
    a_prior, b_prior, l_hood, ..., obs = NULL, s_size = NULL, a_obs = NULL
    ) {
  if (l_hood == "pois" | l_hood == "exp") {
    alpha_post <- a_prior + sum(obs)
    beta_post <- b_prior + s_size
  }
  if (l_hood == "gamma") {
    alpha_post <- a_prior + s_size * a_obs
    beta_post <- b_prior + sum(obs)
  }
  if (l_hood == "invgamma") {
    alpha_post <- a_prior + s_size * a_obs
    beta_post <- b_prior + sum(1/obs)
  }
  c(alpha_post, beta_post)
}

dgamma_post <- function(
    quantile, observations, sample_size, likelihood, ...,
    alpha_observed = NULL, alpha_prior = 1, beta_prior = 1) {
  rlang::arg_match(likelihood, c("pois", "exp", "gamma", "invgamma"))
  post_vals <- update_gamma_parameters(
    alpha_prior, beta_prior, likelihood, obs = observations, s_size = sample_size,
    a_obs = alpha_observed)
  dgamma(quantile, post_vals[1L], post_vals[2L], ...)
}
