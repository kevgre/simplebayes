#' Update Normal Parameters
#'
#' @param mu_prior The prior value for the mean
#' @param var_prior The prior value for the variance
#' @param obs The observations from the model
#' @param ... Placeholder for future capabilities. Currently ignored
#' @param prec_prior The prior value for the precision
#'
#' @returns A vector of length 2 where the posterior mean is first followed by
#' the posterior variance
update_normal_parameters <- function(
    mu_prior, var_prior, obs, ..., prec_prior = 1/var_prior
    ) {
  prec <- 1/var(obs)
  nprec <- length(obs) * prec
  mu_numerator <- prec_prior * mu_prior + prec * sum(obs)
  prec_post <- (prec_prior + nprec)
  mu_post <- mu_numerator/prec_post

  c(mu_post, 1/prec_post)
}


dnorm_post <- function(
    quantiles, observations, ...,
    mean_prior = 1, variance_prior = 1 / sd(observations),
    precision_prior = 1/variance_prior) {
  norm_post <- update_normal_parameters()
}
