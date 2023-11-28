test_that("gamma parameters update", {
  alpha_prior <- 1
  beta_prior <- 1
  s_size <- 10
  OBS <- sample(1:5, s_size, TRUE)
  a_obs <- 3
  likelihoods <- c("pois", "exp", "gamma", "invgamma")
  res <- matrix(c(alpha_prior + sum(OBS), beta_prior + s_size,
                  alpha_prior + sum(OBS), beta_prior + s_size,
                  alpha_prior + s_size * a_obs, beta_prior + sum(OBS),
                  alpha_prior + s_size * a_obs, beta_prior + sum(1 / OBS)),
                byrow = TRUE, nrow = 4)
  for (i in seq_along(likelihoods)) {
    expect_equal(
      update_gamma_parameters(
        alpha_prior, beta_prior, likelihoods[i], obs = OBS, sample_size = s_size,
        a_obs = a_obs),
      res[i, ])
  }
})
