test_that("initialize_priors works", {
  beta_prior <- runif(10, 0, 10)
  variance_prior <- 0.112
  y <- rnorm(100, 1, 3)
  x <- matrix(rnorm(1000, 2, 3), nrow = 100)
  expect_equal(
    initialize_priors(y, x, 100 / 101, NULL, variance_prior)$variance_prior,
    variance_prior
  )
  expect_equal(
    initialize_priors(y, x, 100 / 101, beta_prior, NULL)$beta_prior,
    beta_prior
  )
  expect_length(initialize_priors(y, x, 100 / 101, beta_prior, variance_prior), 2)
  expect_equal(
    initialize_priors(y, x, 100 / 101, NULL, NULL),
    list(
      "beta_prior" = 100 / 101 * lm(y ~ 0 + x)$coefficients,
      "variance_prior" = summary(lm(y ~ 0 + x))$sigma^2
    ),
    ignore_attr = TRUE
  )
})

test_that("variance post works", {
  y <- rnorm(100, 1, 3)
  x <- matrix(rnorm(1000, 2, 3), nrow = 100)
  v_prior <- initialize_priors(y, x, 100 / 101, NULL, NULL)$variance_prior
  iters <- 10
  expect_length(variance_post(y, x, 10, 100, 1, v_prior), 10)
  expect_true(all(variance_post(y, x, 10, 100, 1, v_prior) > 0))
})

test_that("coefficients post works", {
  y <- rnorm(100, 1, 3)
  x <- matrix(rnorm(1000, 2, 3), nrow = 100)
  priors <- initialize_priors(y, x, 100 / 101, NULL, NULL)
  iters <- 10
  var_post <- variance_post(y, x, iters = iters, 100, 1, priors$variance_prior)
  expect_equal(
    dim(coefficients_post(x, iters = iters, 100, priors$beta_prior, var_post)),
    c(length(priors$beta_prior), iters)
  )
  expect_true(all(
    !is.na(coefficients_post(x, iters = iters, 100, priors$beta_prior, var_post))
  ))
})

test_that("bayes_lm works", {
  Y <- rnorm(100, 1, 3)
  X <- matrix(rnorm(1000, 2, 3), nrow = 100)
  expect_error(bayes_lm(rnorm(10), matrix(0, nrow = 9)))
  expect_error(bayes_lm(Y, X, iterations = 0))
  expect_error(bayes_lm(Y, X, iterations = -100))
  expect_error(bayes_lm(Y, X, beta_prior = runif(9, 0, 4)))
  expect_equal(dim(bayes_lm(Y, X, iterations = 100)), c(100, ncol(X) + 1))
  colnames(X) <- paste0("name", seq_len(ncol(X)))
  expect_equal(
    colnames(bayes_lm(Y, X, iterations = 100)), c(colnames(X), "variance")
    )
})

test_that("credible intervals compute", {
  Y <- rnorm(100, 1, 3)
  X <- matrix(rnorm(1000, 2, 3), nrow = 100)
  out <- bayes_lm(Y, X, iterations = 100)
  expect_equal(
    credible_itervals(out), apply(out, 2, quantile, prob = c(0.05, 0.95))
    )
  expect_equal(
    credible_itervals(out, level = 0.95),
    apply(out, 2, quantile, prob = c(0.025, 0.975))
    )
})
