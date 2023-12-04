#' Update Normal Parameters
#'
#' @param mean_prior The prior value for the mean
#' @param var_prior The prior value for the variance
#' @param obs The observations from the model
#' @param ... Placeholder for future capabilities. Currently ignored
#' @param precision_prior The prior value for the precision
#'
#' @returns A vector of length 2 where the posterior mean is first followed by
#' the posterior variance
update_normal_parameters <- function(
    mean_prior, var_prior, obs, ..., precision_prior = 1/var_prior
    ) {
  precision <- 1/var(obs)
  nprecision <- length(obs) * precision
  mean_numerator <- precision_prior * mean_prior + precision * sum(obs)
  precision_post <- (precision_prior + nprecision)
  mean_post <- mean_numerator/precision_post

  c(mean_post, 1/precision_post)
}
