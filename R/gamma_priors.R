update_gamma_parameters <- function(
    a_prior, b_prior, l_hood, ..., obs = NULL, sample_size = NULL, a_obs = NULL
    ) {
  if (l_hood == "pois" | l_hood == "exp") {
    alpha_post <- a_prior + sum(obs)
    beta_post <- b_prior + sample_size
  }
  if (l_hood == "gamma") {
    alpha_post <- a_prior + sample_size * a_obs
    beta_post <- b_prior + sum(obs)
  }
  if (l_hood == "invgamma") {
    alpha_post <- a_prior + sample_size * a_obs
    beta_post <- b_prior + sum(1/obs)
  }
  c(alpha_post, beta_post)
}
