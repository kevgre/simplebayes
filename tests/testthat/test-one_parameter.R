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
