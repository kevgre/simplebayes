#' Compute Beta Posterior Parameters
#'
#' `update_beta_parameters()` will compute the posterior parameter values
#' for a model with a beta prior.
#'
#' @param a_prior The alpha prior value
#' @param b_prior The beta prior value
#' @param l_hood The likelihood for the model
#' @param res The result of the experiment (either successes or failures)
#' @param s_size The sample size of the experiment
#' @param f_rate The failure rate for the model. Only used for the negative
#' binomial likelihood.
#'
#' @returns A vector of length two where the first value is the alpha parameter
#' and the second value is the beta parameter.
#'
update_beta_parameters <- function(
    a_prior, b_prior, l_hood, res = NULL, s_size = NULL, f_rate = NULL) {
  if (l_hood == "binom") {
    alpha_post <- res + a_prior
    beta_post <- s_size - res + b_prior
  }
  if (l_hood == "nbinom") {
    alpha_post <- a_prior + f_rate * s_size
    beta_post <- b_prior + res
  }
  if (l_hood == "geometric") {
    alpha_post <- a_prior + s_size
    beta_post <- b_prior + res
  }
  c(alpha_post, beta_post)
}

#' Posterior Beta Distribution
#'
#' `dbeta_post()` will compute the posterior CDF of a model with a beta
#' prior. Currently, the binomial, negative binomial, and geometric likelihoods
#' can be used.
#'
#' `pbeta_post()` will compute the posterior PDF of a model with a beta
#' prior.
#'
#' `rbeta_post()` will generate random numbers from the posterior distribution
#' of a model with a beta prior.
#'
#' @param quantile A vector of quantiles
#' @param n The number of random values to generate
#' @param result The result from the experiment as successes or failures
#' @param sample_size The sample size from the experiement
#' @param likelihood The likelihood to be used
#' @param ... Pass arguments to the underlying xbeta functions.
#' @param failure_rate The failure rate of a negative binomial experiement
#' @param alpha_prior The prior value for the alpha parameter
#' @param beta_prior The prior value for the beta parameter
#'
#' @returns A vector of the same length as `quantile`.
#' @export
#'
#' @examples
#'
#' x <- seq(0, 1, length.out = 10)
#' dbeta_post(x, 5, 10, "binom")
dbeta_post <- function(
    quantile, result, sample_size, likelihood, ...,
    failure_rate = NULL, alpha_prior = 0.5, beta_prior = 0.5) {
  rlang::arg_match(likelihood, c("binom", "nbinom", "geometric"))
  post_vals <- update_beta_parameters(
    alpha_prior, beta_prior, likelihood, result, sample_size, failure_rate
    )
  stats::dbeta(quantile, post_vals[1], post_vals[2], ...)
}

#' @rdname dbeta_post
pbeta_post <- function(
    quantile, result, sample_size, likelihood, ...,
    failure_rate = NULL, alpha_prior = 0.5, beta_prior = 0.5) {
  rlang::arg_match(likelihood, c("binom", "nbinom", "geometric"))
  post_vals <- update_beta_parameters(
    alpha_prior, beta_prior, likelihood, result, sample_size, failure_rate
  )
  stats::pbeta(quantile, post_vals[1], post_vals[2], ...)
}

#' @rdname dbeta_post
rbeta_post <- function(
    n, result, sample_size, likelihood, ...,
    failure_rate = NULL, alpha_prior = 0.5, beta_prior = 0.5) {
  rlang::arg_match(likelihood, c("binom", "nbinom", "geometric"))
  post_vals <- update_beta_parameters(
    alpha_prior, beta_prior, likelihood, result, sample_size, failure_rate
  )
  stats::rbeta(n = n, post_vals[1], post_vals[2], ...)
}
