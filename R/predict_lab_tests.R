#' @export
predict_lab_tests <- function(lab_data,
                              time_index_column = "time_index",
                              test_name_column = "test_name",
                              year_column = "year",
                              by_column = "month",
                              historical_column = "historical",
                              variable_column = "variable",
                              value_column = "value",
                              test_volume_variable = "test_volume") {
  # lab_data <- read.csv("data-raw/simulated_lab_data.csv", stringsAsFactors = FALSE)
  # time_index_column = "time_index"; test_name_column = "test_name"; year_column = "year"; by_column = "month"; historical_column = "historical"; variable_column = "variable"; value_column = "value"; test_volume_variable = "test_volume"
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
                                                   points_per_year))
}

predict_lab_test <- function(time_index, 
                             year, 
                             by,
                             historical,
                             variable, 
                             value, 
                             trend,
                             seasonal,
                             predictors) {
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
