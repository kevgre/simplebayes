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
      mu_prior, s2_prior,
      obs = obs, precision_prior = 1 / s2_prior
    )
  )
  expect_equal(
    update_normal_parameters(mu_prior, s2_prior, obs = obs),
    c(
      mu_num / (tau_prior + nprec),
      1 / (tau_prior + nprec)
    )
  )
})

test_that("dorm_post works", {
  obs <- rnorm(100, 4, 2)
  norm_post <- update_normal_parameters(
    mu_prior = 1, var_prior = 1 / sd(obs), obs = obs
  )
  expect_equal(
    dnorm_post(mean(obs), obs), dnorm(mean(obs), norm_post[1], norm_post[2])
  )
  expect_equal(
    dnorm_post(1, obs, log = TRUE),
    dnorm(1, norm_post[1], norm_post[2], log = TRUE)
  )
})

test_that("porm_post works", {
  obs <- rnorm(100, 4, 2)
  norm_post <- update_normal_parameters(
    mu_prior = 1, var_prior = 1 / sd(obs), obs = obs
  )
  expect_equal(
    pnorm_post(mean(obs), obs), pnorm(mean(obs), norm_post[1], norm_post[2])
  )
  expect_equal(
    pnorm_post(1, obs, lower.tail = TRUE),
    pnorm(1, norm_post[1], norm_post[2], lower.tail = TRUE)
  )
})

test_that("qorm_post works", {
  obs <- rnorm(100, 4, 2)
  norm_post <- update_normal_parameters(
    mu_prior = 1, var_prior = 1 / sd(obs), obs = obs
  )
  expect_equal(
    qnorm_post(0.1, obs), qnorm(0.1, norm_post[1], norm_post[2])
  )
  expect_equal(
    qnorm_post(1, obs, lower.tail = TRUE),
    qnorm(1, norm_post[1], norm_post[2], lower.tail = TRUE)
  )
})

test_that("rorm_post works", {
  obs <- rnorm(100, 4, 2)
  norm_post <- update_normal_parameters(
    mu_prior = 1, var_prior = 1 / sd(obs), obs = obs
  )
  expect_equal(
    withr::with_seed(
      123,
      rnorm_post(mean(obs), obs)
    ),
    withr::with_seed(
      123,
      rnorm(mean(obs), norm_post[1], norm_post[2])
    )
  )
})
