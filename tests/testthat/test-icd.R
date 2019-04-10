context("ICD codes properly added")

data("icd_lookup")



test_that("description is added", {
  x <- data.frame(icd = c("43852", "M93929", "W002XXA")) %>%
    add_icd_category(icd_column = "icd")
  
  x_description <- x %>%
    dplyr::filter(icd == "43852") %>%
    dplyr::pull(global_short_description)
  
  lookup_description <- icd_lookup %>%
    dplyr::filter(icd_code == "43852") %>%
    dplyr::pull(global_short_description)
  
  expect_equal(x_description, lookup_description)
})

test_that("different description is added", {
  x <- data.frame(icd = c("43852", "M93929", "W002XXA")) %>%
    add_icd_category(icd_column = "icd", icd_lookup_column = "global_long_description")
  
  x_description <- x %>%
    dplyr::filter(icd == "43852") %>%
    dplyr::pull(global_long_description)
  
  lookup_description <- icd_lookup %>%
    dplyr::filter(icd_code == "43852") %>%
    dplyr::pull(global_long_description)
  
  expect_equal(x_description, lookup_description)
})



test_that("parallel works", {
  x <- data.frame(icd = c("43852", "M93929", "W002XXA")) %>%
    add_icd_category(icd_column = "icd", parallel = TRUE)
  
  x_description <- x %>%
    dplyr::filter(icd == "43852") %>%
    dplyr::pull(global_short_description)
  
  lookup_description <- icd_lookup %>%
    dplyr::filter(icd_code == "43852") %>%
    dplyr::pull(global_short_description)
  
  expect_equal(x_description, lookup_description)
})
