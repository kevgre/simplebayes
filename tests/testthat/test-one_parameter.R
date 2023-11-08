test_that("posterior dbeta works", {
  expect_equal(dbeta_post(1, 10, 100), stats::dbeta(1, 10 + 1/2, 100 + 1/2))
})
