context("matching MRNs")

data(y_mrn)
data(x_mrn)
 
matched <- match_MRNs(x_mrn, y_mrn)

test_that("right number of names are matched", {
  expect_equal(nrow(matched), 5)
})
