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

variance_post <- function(Y, X, iters, g, mu_prior, s2_prior) {
  g_frac <- g/(1 + g)
  hat_matrix <- X %*% tcrossprod(solve(crossprod(X)), X)
  SSRg <- crossprod(Y, diag(g) - g_frac * hat_matrix) %*% Y
  1/rgamma(iters, (mu_prior + g)/2, (s2_prior * mu_prior + SSRg)/2)
}

beta_post <- function(X, iters, G, b_prior, v_post) {
  temp <- G/(G + 1) * solve(crossprod(X))
  beta_out <- matrix(0, length(b_prior), iters)
  for (i in seq_len(iters)) {
    beta_out[, i] <- mvtnorm::rmvnorm(1L, b_prior, v_post[i] * temp)
  }
  beta_out
}

bayes_lm <- function(
    y, x, ..., iterations = 10000, mean_prior = 1,
    beta_prior = NULL, variance_prior = NULL
    ) {
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
    y, x, iterations, g = g, mean_prior, priors$variance_prior
    )

  beta_out <- beta_post(
    x, iterations, g, priors$beta_prior, variance_posterior
    )
  out <- cbind(beta_out, variance_posterior)
  colnames(out) <- c(paste0("beta", seq_along(priors$beta_prior)), "variance")
  out
}
