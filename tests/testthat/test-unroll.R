library(dplyr)

set.seed(1)

dat <- dplyr::tibble(year = rep(2018:2019, each = 12),
                     month = rep(1:12, times = 2),
                     original = sample(200:300, 24, TRUE)) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(ytd = roll_mean_year_to_date(original),
                unrolled = unroll_year_to_date(ytd))


test_that("unroll_year_to_date works", {
  expect_equal(dat$original, dat$unrolled)
})

