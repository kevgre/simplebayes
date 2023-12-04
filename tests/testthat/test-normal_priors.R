test_that("normal parameters update", {
  obs <- rnorm(100, 5, 4)
  mu_prior <- 3
  s2_prior <- 2
  tau_prior <- 1 / s2_prior
  prec <- 1 / var(obs)
  nprec <- length(obs) * prec
  mu_num <- tau_prior * mu_prior + prec * sum(obs)
  expect_equal(
    update_normal_parameters(mu_prior, s2_prior, obs = obs),
    update_normal_parameters(
      mu_prior, s2_prior, obs = obs, precision_prior = 1/s2_prior
      )
    )
  expect_equal(
    update_normal_parameters(mu_prior, s2_prior, obs = obs),
    c(mu_num/(tau_prior + nprec),
      1/(tau_prior + nprec))
    )
})
