#' @import forecast trend MASS broom
#' @export
characterize_lab_data <- function(lab_data, test_volume_variable, points_per_year) {
  
  
  volume_data <- lab_data %>%
    dplyr::filter(variable_column == test_volume_variable) %>%
    dplyr::group_by(test_name_column) %>%
    dplyr::summarize(trend = check_trend(value_column),
                     seasonal = check_seasonality(value_column, points_per_year))
  
  predictor_data <- lab_data %>%
    dplyr::filter(historical_column) %>%
    dplyr::group_by(test_name_column) %>%
    dplyr::summarise(predictors = check_predictors(value_column, 
                                                   variable_column,
                                                   test_volume_variable, 
                                                   time_index_column))
  
  dplyr::inner_join(volume_data, predictor_data, "test_name_column")
}

check_trend <- function(x) {
  trend::mk.test(x)$p.value < .1
}

check_seasonality <- function(x, frequency) {
  
  ts_object <- ts(data = x, end = length(x), frequency = frequency)
  
  fit <- forecast::tbats(ts_object)
  
  seasonal <- !is.null(fit$seasonal)
  
  seasonal
}

check_predictors <- function(values, variables, test_vol_var, time_index_col) {
  # z <- "e"
  # values <- lab_data %>% dplyr::filter(test_name_column == z, historical_column) %>% dplyr::pull(value_column)
  # variables <- lab_data %>% dplyr::filter(test_name_column == z, historical_column) %>% dplyr::pull(variable_column)
  # test_vol_var <- test_volume_variable;
  # time_index_col <- lab_data %>% dplyr::filter(test_name_column == z, historical_column) %>% dplyr::pull(time_index_column)
  
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

  predictors <- broom::tidy(step_fit) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::pull(term) %>%
    paste(collapse = "|") %>%
    trimws()

  
  if(!grepl("\\S", predictors)) {
    predictors <- NA_character_
  }

  
  predictors
}
