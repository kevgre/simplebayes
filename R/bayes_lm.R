#' Initialize linear model priors
#'
#' `initialize_priors()` ensures that both beta and the variance have priors.
#'
#' @param Y A vector of response values
#' @param X A matrix of input values
#' @param g_frac The unit information divided by itself plus 1
#' @param b_prior The prior values for beta
#' @param v_prior The prior values for the variance
#'
#' @returns A list of the priors. The beta priors are first then the variance.
initialize_priors <- function(Y, X, g_frac, b_prior, v_prior) {
  lm_fit <- stats::lm(Y ~ 0 + X)
  if (is.null(b_prior)) {
    b_prior <- g_frac * lm_fit$coefficients
  }
  if (is.null(v_prior)) {
    v_prior <- summary(lm_fit)$sigma^2
  }
  list("beta_prior" = b_prior, "variance_prior" = v_prior)
}

#' Compute Posterior Variance
#'
#' `variance_post()` computes the posterior variance for the linear model.
#'
#' @param Y A vector of response values
#' @param X A matrix of input values
#' @param iters The number of iterations to perform
#' @param g The unit information. Equivalent to the length of Y
#' @param mu_prior The prior for mu
#' @param s2_prior The prior for the variance
#'
#' @returns A vector of posterior variance values
variance_post <- function(Y, X, iters, g, mu_prior, s2_prior) {
  g_frac <- g / (1 + g)
  hat_matrix <- X %*% tcrossprod(solve(Matrix::nearPD(crossprod(X))$mat), X)
  SSRg <- crossprod(Y, diag(g, g) - g_frac * hat_matrix) %*% Y
  1 / stats::rgamma(iters, (mu_prior + g) / 2, (s2_prior * mu_prior + SSRg) / 2)
}

#' Compute Posterior Regression Coefficients
#'
#' `coefficients_post()` computes the posterior regression coefficients for the
#' linear regression model
#'
#' @param X A matrix of input values
#' @param iters The number of iterations to perform
#' @param G The unit information prior. Equivalent to the number of rows in X
#' @param b_prior The prior values of the regression coefficients
#' @param v_post The posterior values for the variance
#'
#' @returns A matrix of regression coefficients where each column represents an
#' iteration in the Gibbs Sampler.
coefficients_post <- function(X, iters, G, b_prior, v_post) {
  temp <- G / (G + 1) * solve(Matrix::nearPD(crossprod(X))$mat)
  beta_out <- matrix(0, length(b_prior), iters)
  for (i in seq_len(iters)) {
    beta_out[, i] <- mvtnorm::rmvnorm(1L, b_prior, v_post[i] * temp)
  }
  beta_out
}

#' Bayesian Linear Model
#'
#' `bayes_lm()` uses a Gibbs Sampler to create a Bayesian linear model.
#'
#' @param y A vector of response values
#' @param x A matrix of input values
#' @param ... Placeholder for future capabilities. Currently ignored.
#' @param iterations The number of iterations in the Gibbs Sampler
#' @param mean_prior The prior value for the mean
#' @param beta_prior The prior values for the regression coefficients
#' @param variance_prior The prior value for variance
#'
#' @returns A matrix of posterior values where each column is a parameter. The
#' regression coefficients are first, then the variance.
#' @export
#'
#' @examples
#' # Simulate data
#' X <- matrix(rnorm(1000, 5, 2), nrow = 100)
#' betas <- runif(10, -2, 2)
#' Y <- X %*% betas + rnorm(100, 0, 3)
#'
#' # Perform Bayesian Linear Regression
#' bayes_lm(Y, X, iterations = 10)
bayes_lm <- function(
    y, x, ..., iterations = 10000, mean_prior = 1,
    beta_prior = NULL, variance_prior = NULL) {
  if (iterations <= 0) {
    rlang::abort("iterations must be greater than 0")
  }
  if (length(y) != nrow(x)) {
    rlang::abort("The length of y must equal the number of rows in x")
  }
  g <- length(y)
  g_fraction <- g / (g + 1)
  priors <- initialize_priors(y, x, g_fraction, beta_prior, variance_prior)

  if (ncol(x) != length(priors$beta_prior)) {
    rlang::abort("Number of parameters do not match")
  }

  variance_posterior <- variance_post(
    y, x, iterations,
    g = g, mean_prior, priors$variance_prior
  )

  beta_out <- coefficients_post(
    x, iterations, g, priors$beta_prior, variance_posterior
  )
  out <- t(rbind(beta_out, variance_posterior))
  if (length(colnames(x = x)) != 0) {
    colnames(out) <- c(colnames(x), "variance")
  } else {
  colnames(out) <- c(paste0("beta", seq_along(priors$beta_prior)), "variance")
  }
  out
}
