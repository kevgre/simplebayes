bayes_lm <- function(
    y, x, ..., iterations = 10000, mean_prior = 1, beta_prior = NULL, variance_prior = NULL
    ) {
  g <- length(y)
  g_fraction <- g / (g + 1)
  if (is.null(beta_prior)) {
    g <- length(y)
    beta_prior <- g_fraction * stats::lm(y ~ 0 + x)$coefficients
    variance_prior <- summary(lm.fit)$sigma^2
  }

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
