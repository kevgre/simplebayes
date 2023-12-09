test_that("gamma parameters update", {
  alpha_prior <- 1
  beta_prior <- 1
  s_size <- 10
  OBS <- sample(1:5, s_size, TRUE)
  a_obs <- 3
  likelihoods <- c("pois", "exp", "gamma", "invgamma", "normal")
  res <- matrix(c(alpha_prior + sum(OBS), beta_prior + s_size,
                  alpha_prior + sum(OBS), beta_prior + s_size,
                  alpha_prior + s_size * a_obs, beta_prior + sum(OBS),
                  alpha_prior + s_size * a_obs, beta_prior + sum(1 / OBS),
                  alpha_prior + s_size/2, beta_prior + sum((OBS - mean(OBS))^2)/2),
                byrow = TRUE, nrow = 5)
  for (i in seq_along(likelihoods)) {
    expect_equal(
      update_gamma_parameters(
        alpha_prior, beta_prior, likelihoods[i], obs = OBS, s_size = s_size,
        a_obs = a_obs),
      res[i, ])
  }
})

test_that("dgamma_post works", {
  s_size <- 10
  OBS <- sample(1:5, s_size, TRUE)
  expect_equal(dgamma_post(2, OBS, s_size, "pois"),
               dgamma(2, 0 + sum(OBS), 0 + s_size))
  expect_length(dgamma_post(2:5, OBS, s_size, "pois"), length(2:5))
  expect_equal(dgamma_post(2, OBS, s_size, "pois", log = TRUE),
               dgamma(2, 0 + sum(OBS), 0 + s_size, log = TRUE))
})

test_that("pgamma_post works", {
  s_size <- 10
  OBS <- sample(1:5, s_size, TRUE)
  expect_equal(pgamma_post(2, OBS, s_size, "exp"),
               pgamma(2, 0 + sum(OBS), 0 + s_size))
  expect_length(pgamma_post(2:5, OBS, s_size, "exp"), length(2:5))
  expect_equal(pgamma_post(2, OBS, s_size, "exp", log = TRUE),
               pgamma(2, 0 + sum(OBS), 0 + s_size, log = TRUE))
})

test_that("qgamma_post works", {
  s_size <- 10
  OBS <- sample(1:5, s_size, TRUE)
  expect_equal(qgamma_post(0.5, OBS, s_size, "exp"),
               qgamma(0.5, 0 + sum(OBS), 0 + s_size))
  expect_length(
    qgamma_post(seq(0.2, 0.5, by = 0.1), OBS, s_size, "exp"), length(2:5)
    )
  expect_equal(qgamma_post(-2, OBS, s_size, "exp", log.p = TRUE),
               qgamma(-2, 0 + sum(OBS), 0 + s_size, log.p = TRUE))
})

test_that("rgamma_post works", {
  s_size <- 10
  OBS <- sample(1:5, s_size, TRUE)
  expect_equal(
    withr::with_seed(
      123, rgamma_post(2, OBS, s_size, "gamma", alpha_observed = 0.8)
    ),
    withr::with_seed(123, rgamma(2, 0 + s_size * 0.8, 0 + sum(OBS)))
  )
})
