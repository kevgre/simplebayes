test_that("initialize_priors works", {
  beta_prior <- runif(10, 0, 10)
  variance_prior <- 0.112
  y <- rnorm(100, 1, 3)
  x <- matrix(rnorm(1000, 2, 3), nrow = 100)
  expect_equal(
    initialize_priors(y, x, 100/101, NULL, variance_prior)$variance_prior,
    variance_prior
    )
  expect_equal(
    initialize_priors(y, x, 100/101, beta_prior, NULL)$beta_prior,
    beta_prior
  )
  expect_length(initialize_priors(y, x, 100/101, beta_prior, variance_prior), 2)
  expect_equal(
    initialize_priors(y, x, 100/101, NULL, NULL),
    list("beta_prior" = 100/101 * lm(y ~ 0 + x)$coefficients,
         "variance_prior" = summary(lm(y ~ 0 + x))$sigma^2),
    ignore_attr = TRUE
    )
})

test_that("variance post works", {
  y <- rnorm(100, 1, 3)
  x <- matrix(rnorm(1000, 2, 3), nrow = 100)
  v_prior <- initialize_priors(y, x, 100/101, NULL, NULL)$variance_prior
  iters <- 10
  expect_length(variance_post(y, x, 10, 100, 1, v_prior), 10)
  expect_true(all(variance_post(y, x, 10, 100, 1, v_prior) > 0))
})
