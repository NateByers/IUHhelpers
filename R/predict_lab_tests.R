#' @export
#' @import fitdistrplus purrr
predict_lab_tests <- function(lab_data, 
                              time_index_column = "time_index",
                              test_name_column = "test_name",
                              year_column = "year",
                              by_column = "month",
                              historical_column = "historical",
                              variable_column = "variable",
                              value_column = "value",
                              test_volume_variable = "test_volume",
                              confidence_interval = .90, 
                              random_n = 10000) {
  # lab_data <- read.csv("data-raw/simulated_lab_data.csv", stringsAsFactors = FALSE)
  # random_n <- 10000; confidence_interval = .90; time_index_column = "time_index"; test_name_column = "test_name"; year_column = "year"; by_column = "month"; historical_column = "historical"; variable_column = "variable"; value_column = "value"; test_volume_variable = "test_volume"
  if(class(lab_data[[by_column]]) != "integer") {
    stop("by_column must be an integer")
  }
  
  points_per_year <- c("day" = 365, "week" = 52, "month" = 12, "year" = 1)[by_column]
  
  columns <- c(time_index_column = time_index_column, 
               test_name_column = test_name_column,
               year_column = year_column, 
               by_column = by_column,
               historical_column = historical_column,
               variable_column = variable_column, 
               value_column = value_column)
  
  for(i in names(columns)) {
    current_name <- columns[[i]]
    names(lab_data)[names(lab_data) == current_name] <- i
  }
  
  characterization <- characterize_lab_data(lab_data, test_volume_variable,
                                            points_per_year)
  
  lab_data <- lab_data %>%
    dplyr::left_join(characterization, "test_name_column") %>%
    split(.$test_name_column) %>%
    purrr::map(predict_lab_test, test_volume_variable, points_per_year)
}

predict_lab_test <- function(dat, test_volume_variable, points_per_year) {
  # dat <- lab_data %>% dplyr::filter(test_name_column == "c")
  
  trend <- unique(dat[["trend"]])
  seasonal <- unique(dat[["seasonal"]])
  predictors <- unique(dat[["predictors"]])
  
  if(!trend & !seasonal & is.na(predictors)) {
    future_data <- predict_random(dat, test_volume_variable, points_per_year)
  }
  else if(trend & !seasonal & is.na(predictors)) {
    future_data <- predict_trend(dat, test_volume_variable, points_per_year)
  }
  else if(trend & seasonal & is.na(predictors)) {
    future_data <- predict_trend_seasonal(dat, test_volume_variable, points_per_year)
  }
  else if(trend & !seasonal & !is.na(predictors)) {
    future_data <- predict_trend_predictors(dat, test_volume_variable, points_per_year, predictors)
  }
  else if(trend & seasonal & !is.na(predictors)) {
    future_data <- predict_trend_seasonal_predictors(dat, test_volume_variable, points_per_year,
                                      predictors)
  }
  else if(!trend & seasonal & is.na(predictors)) {
    future_data <- predict_seasonal(dat, test_volume_variable, points_per_year)
  }
  else if(!trend & seasonal & !is.na(predictors)) {
    future_data <- predict_seasonal_predictors(dat, test_volume_variable, points_per_year, predictors)
  }
  else if(!trend & !seasonal & !is.na(predictors)) {
    future_data <- predict_predictors(dat, test_volume_variable, points_per_year, predictors)
  }
  
  dat <- dat %>%
    add_prediction(future_data) %>%
    dplyr::mutate(value_column = round(value_column))
  
  dat
}

predict_random <- function(dat, test_volume_variable, points_per_year) {
  test_volume <- dat %>%
    dplyr::filter(variable_column == test_volume_variable) %>%
    dplyr::pull(value_column)
  
  distributions <- c("normal", "gamma", "poisson")
  dist_tests <- lapply(distributions, 
                       function(x) MASS::fitdistr(test_volume, x)$loglik) %>%
    unlist() %>%
    abs()
  
  fit_distribution <- distributions[min(dist_tests) == dist_tests]
  
  center <- mean(test_volume)
  
  if(fit_distribution == "normal") {
    random_numbers <- rnorm(random_n, center, sd(test_volume))
  } else if(fit_distribution == "gamma") {
    # test_volume <- rgamma(36, 9, .5)
    fit_gamma <- fitdistrplus::fitdist(test_volume, distr = "gamma",
                                       method = "mle")$estimate
    random_numbers <- rgamma(random_n, fit_gamma[["shape"]], fit_gamma[["rate"]])
  } else if(fit_distribution == "poisson") {
    # test_volume <- rpois(36, 4)
    fit_poission <- fitdistrplus::fitdist(test_volume, distr = "pois",
                                          method = "mle")$estimate
    random_numbers <- rpois(random_n, fit_poission[["lambda"]])
  }
  
  left_prob <- (1 - confidence_interval)/2
  right_prob <- 1 - left_prob
  
  left_right <- quantile(random_numbers, c(left_prob, right_prob))
  
  if(left_right[1] < 0) {
    left_right[1] <- 0
  }
  
  ci_left <- rep(left_right[1]/points_per_year, points_per_year)
  ci_right <- rep(left_right[2]/points_per_year, points_per_year)
  center <- rep(center/points_per_year, points_per_year)
  
  future_year <- dat %>%
    dplyr::select(year_column) %>%
    dplyr::distinct() %>%
    dplyr::filter(year_column == max(year_column)) %>%
    dplyr::pull(year_column) + 1
  
  future_data <- data.frame(year_column = rep(future_year, points_per_year),
                            by_column = 1:points_per_year,
                            variable_column = test_volume_variable,
                            ci_left, center, ci_right, stringsAsFactors = FALSE)
  
  future_data
}

predict_trend <- function(dat, test_volume_variable, points_per_year) {
  
  test_volume_df <- dat %>%
    dplyr::filter(variable_column == test_volume_variable) 
  
  trend_line <- lm(value_column ~ time_index_column, 
                   data = test_volume_df)$fitted.values 
  
  trend_line <- trend_line - round(median(trend_line))
  
  # plot(test_volume_df$time_index_column, test_volume_df$value_column)
  # points(test_volume_df$time_index_column, trend_line$fitted.values,
  #        pch = 20)
  
  dat <- dat %>%
    dplyr::mutate(value_column = ifelse(variable_column == test_volume_variable, 
                                        value_column - trend_line, 
                                        value_column))
 
  future_data <- dat %>%
    predict_random(test_volume_variable, points_per_year) 
  
  steps <- diff(trend_line)[1]*1:nrow(future_data) + tail(trend_line, 1)
  
  future_data <- future_data %>%
    dplyr::mutate(ci_left = ci_left + steps,
                  center = center + steps,
                  ci_right = ci_right + steps)

  future_data
}

predict_trend_seasonal <- function(dat, test_volume_variable, points_per_year) {
  
}

predict_trend_predictors <- function(dat, test_volume_variable, points_per_year, 
                                     predictors) {
  
}

predict_trend_seasonal_predictors <- function(dat, test_volume_variable, 
                                              points_per_year, predictors) {
  
}

predict_seasonal <- function(dat, test_volume_variable, points_per_year) {
  
}

predict_seasonal_predictors <- function(dat, test_volume_variable, points_per_year,
                                        predictors) {
  
}

predict_predictors <- function(dat, test_volume_variable, points_per_year,
                               predictors) {
  
}

add_prediction <- function(dat, future_data) {
  
  dat[["location"]] <- "point"
  
  last_historical_index <- dat %>%
    dplyr::filter(historical_column) %>%
    dplyr::summarize(index = max(time_index_column)) %>%
    dplyr::pull(index)
  future_data[["time_index_column"]] <- (last_historical_index + 1):(last_historical_index + nrow(future_data))
  
  future_data[["test_name_column"]] <- rep(unique(dat$test_name_column), nrow(future_data))
  
  future_data[["historical_column"]] <- rep(FALSE, nrow(future_data))
  
  future_data[["trend"]] <- rep(unique(dat$trend), nrow(future_data))
  
  future_data[["seasonal"]] <- rep(unique(dat$seasonal), nrow(future_data))
  
  future_data[["predictors"]] <- rep(unique(dat$seasonal), nrow(future_data))
  
  future_data <- future_data %>%
    tidyr::gather(location, value_column, ci_left:ci_right) %>%
    dplyr::select_at(names(dat))
  
  rbind(dat, future_data)
}
