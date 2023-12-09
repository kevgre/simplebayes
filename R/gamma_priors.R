#' Update Gamma Parameters
#'
#' `update_gamma_parameters()` will update the parameters for a gamma prior for
#' a given likelihood. Currently, these are the Poisson, exponential, gamma, and
#' inverse gamma likelihoods.
#'
#' @param a_prior The shape parameter
#' @param b_prior The rate parameter
#' @param l_hood The likelihood of the model
#' @param ... Placeholder for future capabilities. Currently ignored
#' @param obs The observations from the experiment
#' @param s_size The sample size from the experiment
#' @param a_obs The observed shape parameter
#'
#' @returns A vector of length two where the shape parameter is first and then
#' the rate parameter
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
  if (l_hood == "normal") {
    alpha_post <- a_prior + s_size/2
    beta_post <- b_prior + sum((obs - mean(obs))^2)/2
  }
  c(alpha_post, beta_post)
}

#' Posterior Gamma Distribution
#'
#' `dgamma_post()` computes the posterior PDF for a model with a gamma prior.
#'
#' `pgamma_post()` computes the posterior CDF for a model with a gamma prior.
#'
#' `qgamma_post()` computes the quantile for a given probability from a model
#' with a gamma prior.
#'
#' `rgamma_post()` generates random numbers from the posterior distribution from
#' a model with a gamma prior.
#'
#' @param quantile A vector of quantiles
#' @param n The number of random values to generate
#' @param observations The observations from the model
#' @param sample_size The sample size from the model
#' @param likelihood The likelihood used
#' @param ... Pass arguments to dgamma
#' @param alpha_observed The observed shape parameter
#' @param alpha_prior The prior shape parameter
#' @param beta_prior The prior rate parameter
#'
#' @returns A vector of PDF results that is the same length as quantile.
#' @export
#'
#' @examples
#'
#' x <- seq(0, 1, by = 0.1)
#' dgamma_post(x, 10, 100, "pois")
#' pgamma_post(x, 10, 100, "pois")
#' qgamma_post(x, 10, 100, "pois")
#'
#' # Random numbers can also be generated
#' rgamma_post(5, 10, 100, "pois")
dgamma_post <- function(
    quantile, observations, sample_size, likelihood, ...,
    alpha_observed = NULL, alpha_prior = 0, beta_prior = 0) {
  rlang::arg_match(likelihood, c("pois", "exp", "gamma", "invgamma"))
  post_vals <- update_gamma_parameters(
    alpha_prior, beta_prior, likelihood, obs = observations, s_size = sample_size,
    a_obs = alpha_observed)
  stats::dgamma(quantile, post_vals[1L], post_vals[2L], ...)
}

#' @rdname dgamma_post
#' @export
pgamma_post <- function(
    quantile, observations, sample_size, likelihood, ...,
    alpha_observed = NULL, alpha_prior = 0, beta_prior = 0) {
  rlang::arg_match(likelihood, c("pois", "exp", "gamma", "invgamma"))
  post_vals <- update_gamma_parameters(
    alpha_prior, beta_prior, likelihood, obs = observations, s_size = sample_size,
    a_obs = alpha_observed)
  stats::pgamma(quantile, post_vals[1L], post_vals[2L], ...)
}

#' @rdname dgamma_post
#' @export
qgamma_post <- function(
    percentile, observations, sample_size, likelihood, ...,
    alpha_observed = NULL, alpha_prior = 0, beta_prior = 0) {
  rlang::arg_match(likelihood, c("pois", "exp", "gamma", "invgamma"))
  post_vals <- update_gamma_parameters(
    alpha_prior, beta_prior, likelihood, obs = observations, s_size = sample_size,
    a_obs = alpha_observed)
  stats::qgamma(percentile, post_vals[1L], post_vals[2L], ...)
}

#' @rdname dgamma_post
#' @export
rgamma_post <- function(
    n, observations, sample_size, likelihood, ...,
    alpha_observed = NULL, alpha_prior = 0, beta_prior = 0) {
  rlang::arg_match(likelihood, c("pois", "exp", "gamma", "invgamma"))
  post_vals <- update_gamma_parameters(
    alpha_prior, beta_prior, likelihood, obs = observations, s_size = sample_size,
    a_obs = alpha_observed)
  stats::rgamma(n = n, post_vals[1L], post_vals[2L], ...)
}
