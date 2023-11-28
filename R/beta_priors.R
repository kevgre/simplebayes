#' Compute Beta Posterior Parameters
#'
#' `compute_beta_posterior_vals()` will compute the posterior parameter values
#' for a model with a beta prior.
#'
#' @param a_prior The alpha prior value
#' @param b_prior The beta prior value
#' @param l_hood The likelihood for the model
#' @param ... Placeholder for future capabilities. Currently ignored
#' @param result The result of the experiment (either successes or failures)
#' @param s_size The sample size of the experiment
#' @param f_rate The failure rate for the model. Only used for the negative
#' binomial likelihood.
#'
#' @returns A vector of length two where the first value is the alpha parameter
#' and the second value is the beta parameter.
#'
compute_beta_posterior_vals <- function(
    a_prior, b_prior, l_hood, ...,
    result = NULL, s_size = NULL, f_rate = NULL) {
  if (l_hood == "binom") {
    alpha_post <- result + a_prior
    beta_post <- s_size - result + b_prior
  }
  if (l_hood == "nbinom") {
    alpha_post <- a_prior + f_rate * s_size
    beta_post <- b_prior + result
  }
  if (l_hood == "geometric") {
    alpha_post <- a_prior + s_size
    beta_post <- b_prior + result
  }
  c(alpha_post, beta_post)
}

#' Sample Posterior Beta PDF
#'
#' `dbeta_post()` will compute the posterior distribution of a model with a beta
#' prior. Currently, the binomial, negative binomial, and geometric likelihoods
#' can be used.
#'
#' @param quantile A vector of quantiles
#' @param likelihood The likelihood to be used
#' @param ... Pass further arguments such as the sample size, experiment result,
#' or failure rate.
#' @param alpha_prior The prior value for the alpha parameter
#' @param beta_prior The prior value for the beta parameter
#'
#' @returns A vector of the same length as quantile.
#' @export
#'
#' @examples
dbeta_post <- function(quantile, likelihood, ..., alpha_prior = 0.5, beta_prior = 0.5) {
  rlang::arg_match(likelihood, c("binom", "nbinom", "geometric"))
  post_vals <- compute_beta_posterior_vals(alpha_prior, beta_prior, likelihood, ...)
  dbeta(quantile, post_vals[1], post_vals[2])
}
