#' Sample Posterior Beta PDF
#'
#' `dbeta_post()` will sample the posterior beta probability distribution function
#' in a beta-binomial model.
#'
#' @param x vector of quantiles
#' @param successes number of success in the binomial experiment
#' @param sample_size number of trials in the binomial experiment
#' @param ... Placeholder for future capabilities. Currently ignored.
#' @param alpha_prior The prior for the alpha parameter
#' @param beta_prior The prior for the beta parameter
#'
#' @returns A vector of probabilities the same length as x
#' @export
#'
#' @examples
dbeta_post <- function(
    x, successes, sample_size, ..., alpha_prior = 1/2, beta_prior = 1/2
    ) {
  failures <- sample_size - successes
  stats::dbeta(x = x, shape1 = successes + alpha_prior, shape2 = failures + beta_prior)
}
