#' @export
#' @import fitdistrplus
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
    dplyr::group_by(test_name_column) %>%
    dplyr::summarize(prediction = predict_lab_test(time_index_column, 
                                                   year_column, 
                                                   by_column,
                                                   historical_column,
                                                   variable_column, 
                                                   value_column, 
                                                   trend,
                                                   seasonal,
                                                   predictors, 
                                                   test_volume_variable,
                                                   points_per_year,
                                                   confidence_interval,
                                                   random_n))
}

predict_lab_test <- function(time_index, 
                             year, 
                             by,
                             historical,
                             variable, 
                             value, 
                             trend,
                             seasonal,
                             predictors,
                             confidence_interval,
                             random_n) {
  # xyz <- lab_data %>%
  #   dplyr::filter(test_name_column == "a")
  #   
  # time_index <- xyz$time_index_column
  # year <- xyz$year_column
  # by <- xyz$by_column
  # historical <- xyz$historical_column
  # variable <- xyz$variable_column
  # value <- xyz$value_column 
  # trend <- xyz$trend
  # seasonal <- xyz$seasonal
  # predictors <- xyz$predictors
  # test_volume_variable <- "test_volume"
  # points_per_year <- 12
  # confidence_interval <- 90
  
  dat <- data.frame(time_index, year, by, historical, variable, value, 
                    stringsAsFactors = FALSE)
  
  trend <- unique(trend)
  seasonal <- unique(seasonal)
  predictors <- unique(predictors)
  
  if(!trend & !seasonal & is.na(predictors)) {
    predict_random(dat, test_volume_variable)
  }
  else if(trend & !seasonal & is.na(predictors)) {
    predict_trend(dat, test_volume_variable)
  }
  else if(trend & seasonal & is.na(predictors)) {
    predict_trend_seasonal(dat, test_volume_variable, points_per_year)
  }
  else if(trend & !seasonal & !is.na(predictors)) {
    predict_trend_predictors(dat, test_volume_variable, predictors)
  }
  else if(trend & seasonal & !is.na(predictors)) {
    predict_trend_seasonal_predictors(dat, test_volume_variable, points_per_year,
                                      predictors)
  }
  else if(!trend & seasonal & is.na(predictors)) {
    predict_seasonal(dat, test_volume_variable, points_per_year)
  }
  else if(!trend & seasonal & !is.na(predictors)) {
    predict_seasonal(dat, test_volume_variable, points_per_year, predictors)
  }
  else if(!trend & !seasonal & !is.na(predictors)) {
    predict_seasonal(dat, test_volume_variable, predictors)
  }
}

predict_random <- function(dat, test_volume_variable) {
  test_volume <- dat %>%
    dplyr::filter(variable == test_volume_variable) %>%
    dplyr::pull(value)
  
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
  
  paste(round(left_right[1]), round(center), round(left_right[2]), sep = "-")
}

predict_trend <- function(dat, test_volume_variable) {
  
}

predict_trend_seasonal <- function(dat, test_volume_variable, points_per_year) {
  
}

predict_trend_predictors <- function(dat, test_volume_variable, predictors) {
  
}

predict_trend_seasonal_predictors <- function(dat, test_volume_variable, 
                                              points_per_year, predictors) {
  
}

predict_seasonal <- function(dat, test_volume_variable, points_per_year) {
  
}

predict_seasonal <- function(dat, test_volume_variable, points_per_year,
                             predictors) {
  
}

predict_seasonal <- function(dat, test_volume_variable, predictors) {
  
}
