test_that("posterior dbeta works", {
  expect_equal(dbeta_post(0.5, 10, 20), stats::dbeta(0.5, 10 + 1/2, 10 + 1/2))
})

test_that("posterior pbeta works", {
  expect_equal(pbeta_post(0.5, 10, 20), stats::pbeta(0.5, 10 + 1/2, 10 + 1/2))
})

test_that("posterior rbeta works", {
  withr::with_seed(123, {
    expect_equal(rbeta_post(1, 10, 20), stats::rbeta(1, 10 + 1/2, 10 + 1/2),
                 tolerance = 0.1)
  })
})

test_that("gamma functions work", {
  a_prior <- 1
  b_prior <- 1
  x <- sample(1:5, 10, TRUE)
  expect_equal(dgamma_post(2, x = x, 10, alpha_prior = a_prior, beta_prior = b_prior),
               dgamma(2, a_prior + sum(x), b_prior + 10))
  expect_equal(pgamma_post(2, x = x, 10, alpha_prior = a_prior, beta_prior = b_prior),
               pgamma(2, a_prior + sum(x), b_prior + 10))
  withr::with_seed(123, {
    expect_equal(rgamma_post(1, x = x, 10, alpha_prior = a_prior, beta_prior = b_prior),
                 rgamma(1, a_prior + sum(x), b_prior + 10), tolerance = 0.5)
  })
})
