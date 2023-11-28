test_that("beta posterior computed", {
  expect_equal(
    compute_beta_posterior_vals(0.5, 0.5, "binom", result = 2, s_size = 10),
               c(2.5, 8.5))
  expect_equal(
    compute_beta_posterior_vals(0.5, 0.5, "nbinom", result = 2, s_size = 10, f_rate = 0.5),
    c(0.5 + 10 * 0.5, 0.5 + 2)
  )
  expect_equal(
    compute_beta_posterior_vals(0.5, 0.5, "geometric", result = 2, s_size = 10),
    c(0.5 + 10, 0.5 + 2)
  )
})

test_that("dbeta_post works", {
  expect_equal(dbeta_post(2, likelihood = "binom", successes = 2, sample_size = 10),
              dbeta(2, 0.5 + 2, 10 - 2 + 0.5))
})
