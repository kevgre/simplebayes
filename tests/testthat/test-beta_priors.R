test_that("beta paramters updated", {
  expect_equal(
    update_beta_parameters(0.5, 0.5, "binom", res = 2, s_size = 10),
    c(2.5, 8.5)
  )
  expect_equal(
    update_beta_parameters(
      0.5, 0.5, "nbinom",
      res = 2, s_size = 10, f_rate = 0.5
    ),
    c(0.5 + 10 * 0.5, 0.5 + 2)
  )
  expect_equal(
    update_beta_parameters(0.5, 0.5, "geometric", res = 2, s_size = 10),
    c(0.5 + 10, 0.5 + 2)
  )
})

test_that("dbeta_post works", {
  expect_error(dbeta_post(1 / 2, "poisson"))
  expect_equal(
    dbeta_post(1 / 2, likelihood = "binom", result = 2, sample_size = 10),
    dbeta(1 / 2, 0.5 + 2, 10 - 2 + 0.5)
  )
  expect_equal(
    dbeta_post(1 / 2, likelihood = "nbinom", result = 2, sample_size = 10, failure_rate = 0.5),
    dbeta(1 / 2, 0.5 + 10 * 0.5, 2 + 0.5)
  )
  expect_equal(
    dbeta_post(1 / 2, likelihood = "geometric", result = 2, sample_size = 10),
    dbeta(1 / 2, 0.5 + 10, 2 + 0.5)
  )
  quants <- 1 / c(2, 3, 4, 5, 2, 3)
  expect_length(
    dbeta_post(quants, "binom", result = 3, sample_size = 10), length(quants)
  )
  expect_equal(
    dbeta_post(quants, "binom", result = 3, sample_size = 10),
    c(
      dbeta_post(quants[1], "binom", result = 3, sample_size = 10),
      dbeta_post(quants[2], "binom", result = 3, sample_size = 10),
      dbeta_post(quants[3], "binom", result = 3, sample_size = 10),
      dbeta_post(quants[4], "binom", result = 3, sample_size = 10),
      dbeta_post(quants[5], "binom", result = 3, sample_size = 10),
      dbeta_post(quants[6], "binom", result = 3, sample_size = 10)
    )
  )
  expect_equal(
    dbeta_post(0.5, likelihood = "binom", result = 2, sample_size = 10, log = TRUE),
    dbeta(0.5, 0.5 + 2, 10 - 2 + 0.5, log = TRUE)
  )
})

test_that("pbeta_post works", {
  expect_error(pbeta_post(1 / 2, "poisson"))
  expect_equal(
    pbeta_post(1 / 2, likelihood = "binom", result = 2, sample_size = 10),
    pbeta(1 / 2, 0.5 + 2, 10 - 2 + 0.5)
  )
  expect_equal(
    pbeta_post(1 / 2, likelihood = "nbinom", result = 2, sample_size = 10, failure_rate = 0.5),
    pbeta(1 / 2, 0.5 + 10 * 0.5, 2 + 0.5)
  )
  expect_equal(
    pbeta_post(1 / 2, likelihood = "geometric", result = 2, sample_size = 10),
    pbeta(1 / 2, 0.5 + 10, 2 + 0.5)
  )
  quants <- 1 / c(2, 3, 4, 5, 2, 3)
  expect_length(pbeta_post(quants, "binom", result = 3, sample_size = 10), length(quants))
  expect_equal(
    pbeta_post(quants, "binom", result = 3, sample_size = 10),
    c(
      pbeta_post(quants[1], "binom", result = 3, sample_size = 10),
      pbeta_post(quants[2], "binom", result = 3, sample_size = 10),
      pbeta_post(quants[3], "binom", result = 3, sample_size = 10),
      pbeta_post(quants[4], "binom", result = 3, sample_size = 10),
      pbeta_post(quants[5], "binom", result = 3, sample_size = 10),
      pbeta_post(quants[6], "binom", result = 3, sample_size = 10)
    )
  )
})

test_that("qbeta_post works", {
  expect_error(qbeta_post(1 / 2, "poisson"))
  expect_equal(
    qbeta_post(1 / 2, likelihood = "binom", result = 2, sample_size = 10),
    qbeta(1 / 2, 0.5 + 2, 10 - 2 + 0.5)
  )
  expect_equal(
    qbeta_post(1 / 2, likelihood = "nbinom", result = 2, sample_size = 10, failure_rate = 0.5),
    qbeta(1 / 2, 0.5 + 10 * 0.5, 2 + 0.5)
  )
  expect_equal(
    qbeta_post(1 / 2, likelihood = "geometric", result = 2, sample_size = 10),
    qbeta(1 / 2, 0.5 + 10, 2 + 0.5)
  )
  quants <- 1 / c(2, 3, 4, 5, 2, 3)
  expect_length(qbeta_post(quants, "binom", result = 3, sample_size = 10), length(quants))
  expect_equal(
    qbeta_post(quants, "binom", result = 3, sample_size = 10),
    c(
      qbeta_post(quants[1], "binom", result = 3, sample_size = 10),
      qbeta_post(quants[2], "binom", result = 3, sample_size = 10),
      qbeta_post(quants[3], "binom", result = 3, sample_size = 10),
      qbeta_post(quants[4], "binom", result = 3, sample_size = 10),
      qbeta_post(quants[5], "binom", result = 3, sample_size = 10),
      qbeta_post(quants[6], "binom", result = 3, sample_size = 10)
    )
  )
})

test_that("rbeta_post works", {
  expect_error(rbeta_post(2, "poisson"))
  expect_equal(
    withr::with_seed(
      123, rbeta_post(2, likelihood = "binom", result = 2, sample_size = 10)
    ),
    withr::with_seed(123, rbeta(2, 0.5 + 2, 10 - 2 + 0.5))
  )
  expect_equal(
    withr::with_seed(
      123,
      rbeta_post(
        2,
        likelihood = "nbinom", result = 2, sample_size = 10, failure_rate = 0.5
      )
    ),
    withr::with_seed(123, rbeta(2, 0.5 + 10 * 0.5, 2 + 0.5))
  )
  expect_equal(
    withr::with_seed(
      123, rbeta_post(2, likelihood = "geometric", result = 2, sample_size = 10)
    ),
    withr::with_seed(123, rbeta(2, 0.5 + 10, 2 + 0.5))
  )
})
