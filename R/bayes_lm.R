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

bayes_lm <- function(
    y, x, ..., iterations = 10000, mean_prior = 1, beta_prior = NULL, variance_prior = NULL
    ) {
  g <- length(y)
  g_fraction <- g / (g + 1)
  priors <- initialize_priors(y, x, g_fraction, beta_prior, variance_prior)

  hat_matrix <- x %*% tcrossprod(solve(crossprod(x)), x)
  SSRg <- t(y) %*% (diag(g) - g_fraction * hat_matrix) %*% y
  variance_post <- 1/rgamma(
    iterations, (mean_prior + g)/2, (variance_prior * mean_prior + SSRg)/2
    )

  temp <- g_fraction * solve(crossprod(x))
  beta_out <- matrix(0, nrow = length(beta_prior), ncol = iterations)
  for (i in seq_len(iterations)) {
    Sigma.n <- variance_post[i] * temp
    beta_out[, i] <- mvtnorm::rmvnorm(1L, beta_prior, Sigma.n)
  }
  out <- cbind(beta_out, variance_post)
  colnames(out) <- c(paste0("beta", seq_along(beta_prior)), "variance")
  out
}
