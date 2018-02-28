#' @import forecast trend MASS broom
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
  columns <- c(time_index_column = time_index_column, 
               test_name_column = test_name_column,
               year_column = year_column, by_column = by_column, 
               variable_column = variable_column, value_column = value_column)
  
  for(i in names(columns)) {
    current_name <- columns[[i]]
    names(lab_data)[names(lab_data) == current_name] <- i
  }
  
  volume_data <- lab_data %>%
    dplyr::filter(variable_column == test_volume_variable) %>%
    dplyr::group_by(test_name_column) %>%
    dplyr::summarize(trend = check_trend(value_column, variable_column,
                                         test_volume_variable),
                     seasonal = check_seasonality(value_column, by_,
                                                  variable_column,
                                                  test_volume_variable),
                     predictors = check_predictors(value_column, variable_column,
                                                   test_volume_variable, 
                                                   time_index_column))
    
}

check_trend <- function(x, var_col, test_vol_var) {
  trend::mk.test(x[var_col == test_vol_var])$p.value < .05
}

check_seasonality <- function(x, by, var_col, test_vol_var) {
  # x <- lab_data %>% dplyr::filter(variable_column == "test_volume", test_name_column == "a") %>% dplyr::pull(value_column); by = by_column
  x <- x[var_col == test_vol_var]
  
  points_per_year <- c("day" = 365, "week" = 365/7, "month" = 12, "year" = 1)
  ts_object <- ts(data = x, end = length(x), frequency = points_per_year[by])
  
  fit <- forecast::tbats(ts_object)
  
  seasonal <- !is.null(fit$seasonal)
  
  seasonal
}

check_predictors <- function(values, variables, test_vol_var, time_index_col) {
  # values <- lab_data %>% dplyr::filter(test_name_column == "h") %>% dplyr::pull(value_column)
  # variables <- lab_data %>% dplyr::filter(test_name_column == "h") %>% dplyr::pull(variable_column)
  # test_vol_var <- test_volume_variable; 
  # time_index_col <- lab_data %>% dplyr::filter(test_name_column == "h") %>% dplyr::pull(time_index_column)
  
  model_data <- data.frame(index = time_index_col, variable = variables, value = values, stringsAsFactors = FALSE) %>%
    tidyr::spread(variable, value) %>%
    dplyr::select(-index)
  
  
  independent_vars <- names(model_data)[names(model_data) != test_vol_var]
  
  lm_formula <- paste(test_vol_var, "~.") %>%
    as.formula()
  
  fit <- lm(lm_formula, data = model_data)
  
  
  upper_formula <- paste(independent_vars, collapse = " + ")
  upper_formula <- paste("~", upper_formula) %>%
    as.formula()
  
  step_fit <- MASS::stepAIC(fit, scope = list(upper = upper_formula), 
                            trace = FALSE)
  broom::tidy(step_fit) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::pull(term) %>%
    paste(collapse = "|")
  
}
