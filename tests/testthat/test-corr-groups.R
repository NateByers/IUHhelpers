context("find correlated groups")

groups1 <- find_corr_groups(mtcars)
groups2 <- find_corr_groups(mtcars, cor_mat = TRUE)

test_that("one correlated group is found in mtcars", {
  expect_equal(length(groups1), 1)
  
  expect_identical(groups1[[1]], c("disp", "cyl"))
})

test_that("matrix is returned", {
  expect_equal(class(groups2[[1]]), "matrix")
})
