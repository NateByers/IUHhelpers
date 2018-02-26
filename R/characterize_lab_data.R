#' @import forecast
#' @export
characterize_lab_data <- function(lab_data, 
                                  time_index_column = "time_index",
                                  test_name_column = "test_name",
                                  year_column = "year",
                                  by_column = "month",
                                  variable_column = "variable",
                                  value_column = "value",
                                  test_volume_variable = "test_volume") {
  # lab_data <- read.csv("C:/Users/nbyers1/Documents/Repositories/Lab_Budget/simulate/simulated_lab_data.csv", stringsAsFactors = FALSE)
  # time_index_column = "time_index"; test_name_column = "test_name"; year_column = "year"; by_column = "month"; variable_column = "variable"; value_column = "value"; test_volume_variable = "test_volume"
  # columns <- c(time_index_column, test_name_column, year_column, by_column, variable_column, value_column)
  columns <- c(time_index_column = "time_index", test_name_column = "test_name", year_column = "year", by_column = "month", variable_column = "variable", value_column = "value")
  
  for(i in names(columns)) {
    names(lab_data)[names(lab_data) == current_name] <- i
  }
  
  volume_data <- lab_data %>%
    dplyr::filter(variable_column == test_volume_variable) %>%
    dplyr::group_by(test_name_column) %>%
    dplyr::summarize(trend = mk.test(value_column)$p.value < .05,
                     seasonality = check_seasonality())
    
}

check_seasonality <- function(x, by) {
  # x <- volume_data %>% filter(test_name_column == "a") %>% pull(value_column); by = by_column
  points_per_year <- c("day" = 365, "week" = 52, "month" = 12, "year" = 1)
  ts_object <- ts(data = x, end = length(x), frequency = points_per_year[by])
  
  fit1 <- forecast::ets(ts_object)
  fit2 <- forecast::ets(ts_object, model = "ANN")
}