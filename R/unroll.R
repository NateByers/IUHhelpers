#' Recover original data series from moving average
#' @export
#' @importFrom zoo rollmax rollmean
#' @param moving_series a vector of moving averages
#' @param original_seed a vector of the first or last numbers of the original series--must be \code{window_length - 1} or longer
#' @param window_length integer length of the window
#' @param align character specifying whether the index of the original series was left- or right-aligned or centered (default) compared to the rolling window of series
#' @param window_function what function was used to roll up the original data
unroll <- function(moving_series, original_seed, window_length,
                   align = c("left", "right"),
                   window_function = c("mean", "sum")) {

  # test_data3 <- dplyr::tibble(x = sample(10, 10)) %>%
  #   dplyr::mutate(mean_right = zoo::rollmean(x, 3, fill = NA, align = "right"),
  #                 mean_left = zoo::rollmean(x, 3, fill = NA, align = "left"))
  #
  # moving_series = test_data3$mean_left[-(9:10)]; original_seed = test_data3$x[9:10]; window_length = 3; align = "left"; window_function = "mean"

  if(sum(is.na(moving_series)) > 0) {
    stop("can't have NAs in the moving_series vector")
  }

  if(window_function[1] == "mean") {
    unaverage(moving_series, original_seed, window_length, align)
  } else if(window_function[1] == "sum") {
    unsum(moving_series, original_seed, window_length, align)
  }

}

unaverage <- function(moving_series, original_seed, window_length, align) {
  if(align == "left") {
    original_series <- rev(original_seed)
    moving_series <- rev(moving_series)
  } else if(align == "right") {
    original_series <- original_seed
  }

  for(i in seq_along(moving_series)) {
    # i <- 1
    original_series[length(original_series) + 1] <- window_length*moving_series[i] - sum(tail(original_series, window_length - 1))
    }

  if(align == "left") {
    original_series <- rev(original_series)
  }

  original_series
}

unsum <- function(moving_series, original_seed, window_length, align) {
  if(align == "left") {
    original_series <- rev(original_seed)
    moving_series <- rev(moving_series)
  } else if(align == "right") {
    original_series <- original_seed
  }

  for(i in seq_along(moving_series)) {
    # i <- 1
    original_series[length(original_series) + 1] <- moving_series[i] - sum(tail(original_series, window_length - 1))
  }

  if(align == "left") {
    original_series <- rev(original_series)
  }

  original_series
}

#' @export
roll_mean_year_to_date <- function(x) {
  # x <- dat %>% dplyr::filter(cost_center == "A", year == 2018) %>% dplyr::pull(original)

  x_ <- x[1]

  for(i in seq_along(x)[-1]) {
    # i <- 2
    x_[i] <- mean(x[1:i], na.rm = TRUE)
  }

  x_
}

#' Recover original series from a vector of year-to-date averages
#' @export
#' @param x a vector of year-to-date averages
#' @examples
#' library(dplyr)
#'
#' set.seed(1)
#' dat <- dplyr::tibble(year = rep(2018:2019, each = 12),
#'                      month = rep(1:12, times = 2),
#'                      original = sample(200:300, 24, TRUE)) %>%
#'   dplyr::group_by(year) %>%
#'   dplyr::mutate(ytd = roll_mean_year_to_date(original),
#'                 unrolled = unroll_year_to_date(ytd))
unroll_year_to_date <- function(x) {
  # x <- dat %>% dplyr::filter(cost_center == "A", year == 2018) %>% dplyr::pull(ytd)

  x_ <- x[1]

  for(i in seq_along(x)[-1]) {
    # i <- 2
    x_[i] <- x[i]*i - sum(x_)
  }

  x_
}

