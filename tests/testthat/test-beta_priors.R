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
  expect_error(dbeta_post(2, "poisson"))
  expect_equal(dbeta_post(2, likelihood = "binom", result = 2, s_size = 10),
              dbeta(2, 0.5 + 2, 10 - 2 + 0.5))
  expect_equal(dbeta_post(2, likelihood = "nbinom", result = 2, s_size = 10, f_rate = 0.5),
               dbeta(2, 0.5 + 10 * 0.5, 2 + 0.5))
  expect_equal(dbeta_post(2, likelihood = "geometric", result = 2, s_size = 10),
               dbeta(2, 0.5 + 10, 2 + 0.5))
  quants <- c(2, 3, 4, 5, 2, 3)
  expect_length(dbeta_post(quants, "binom", result = 3, s_size = 10), length(quants))
  expect_equal(dbeta_post(quants, "binom", result = 3, s_size = 10),
               c(dbeta_post(quants[1], "binom", result = 3, s_size = 10),
                 dbeta_post(quants[2], "binom", result = 3, s_size = 10),
                 dbeta_post(quants[3], "binom", result = 3, s_size = 10),
                 dbeta_post(quants[4], "binom", result = 3, s_size = 10),
                 dbeta_post(quants[5], "binom", result = 3, s_size = 10),
                 dbeta_post(quants[6], "binom", result = 3, s_size = 10)))
})
