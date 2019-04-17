context("cleaning up headers")

column_names <- c("MRN ID", "First & Middle Name", "Name: Last") %>%
  cleanup_headers()

test_that("spaces and & are removed", {
  expect_equal(column_names[2], "First_and_Middle_Name")
})
