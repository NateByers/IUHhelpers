library(dplyr)

set.seed(1)

test_data3 <- dplyr::tibble(x = sample(10, 10)) %>%
  dplyr::mutate(mean_right = zoo::rollmean(x, 3, fill = NA, align = "right"),
                mean_left = zoo::rollmean(x, 3, fill = NA, align = "left"),
                sum_right = zoo::rollsum(x, 3, fill = NA, align = "right"),
                sum_left = zoo::rollsum(x, 3, fill = NA, align = "left"))


original_mean_right3 <- unroll(moving_series = test_data3$mean_right[-(1:2)],
                               original_seed = test_data3$x[1:2],
                               window_length = 3,
                               align = "right",
                               window_function = "mean")

test_that("unroll mean works with window = 3, align right", {
  expect_equal(original_mean_right3, test_data3$x)
})

original_mean_left3 <- unroll(moving_series = test_data3$mean_left[-(9:10)],
                               original_seed = test_data3$x[9:10],
                               window_length = 3,
                               align = "left",
                               window_function = "mean")

test_that("unroll mean works with window = 3, align left", {
  expect_equal(original_mean_left3, test_data3$x)
})

original_sum_right3 <- unroll(moving_series = test_data3$sum_right[-(1:2)],
                              original_seed = test_data3$x[1:2],
                              window_length = 3,
                              align = "right",
                              window_function = "sum")

test_that("unroll sum works with window = 3, align right", {
  expect_equal(original_sum_right3, test_data3$x)
})

original_sum_left3 <- unroll(moving_series = test_data3$sum_left[-(9:10)],
                              original_seed = test_data3$x[9:10],
                              window_length = 3,
                              align = "left",
                              window_function = "sum")

test_that("unroll sum works with window = 3, align left", {
  expect_equal(original_sum_left3, test_data3$x)
})

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

