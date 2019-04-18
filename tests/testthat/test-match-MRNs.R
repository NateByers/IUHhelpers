context("matching MRNs")

data(x_EMR)
data(y_EMR)
 
matched <- match_MRNs(x_EMR, y_EMR)

test_that("right number of names are matched", {
  expect_equal(nrow(matched), 5)
})
