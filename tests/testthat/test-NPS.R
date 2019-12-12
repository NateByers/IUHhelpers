context("NPS")

set.seed(10)
scores <- sample(0:10, 100, replace = TRUE)

test_that("the correct NPS is calculated", {
  expect_equal(calc_nps(scores), -29)

})

test_that("the correct standard error is calculated", {
  expect_equal(round(calc_nps_std_error(scores), 6), 8.519977)
})


