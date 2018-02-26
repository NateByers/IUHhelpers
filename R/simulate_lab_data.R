#' Generate data to test lab budget project
#' @import tidyr
#' @export
#' @import parallel
#' @param test_name Name for this test
#' @param years How many years of data
#' @param by What time duration do you want
#' @param mean_volume What is the mean volume for this lab test
#' @param sd_volume What is the standard deviation for these tests
#' @param trend Do you want a trend over time
#' @param seasonality Do you want seasonality during the year
#' @param predictor_variables How many predictor variables do you want
#' @param noisy_variables How many non-predictive variables do you want
#' @param shape "long" means the variables will be gathered
#' @examples 
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' 
#' x <- simulate_lab_data(test_name = "a", years = 3, by = "week", mean_volume = 100, 
#'                        sd_volume = 20, trend = TRUE, seasonality = TRUE, 
#'                        predictor_variables = 2, noisy_variables = 1) 
#' 
#' lm(test_volume ~ noisy1, data = x) %>% summary()
#' lm(test_volume ~ predictor1, data = x) %>% summary()
#' lm(test_volume ~ predictor2, data = x) %>% summary()
#' lm(test_volume ~ predictor1 + predictor2, data = x) %>% summary()
#' 
#' x <- tidyr::gather(x, key = "key", value = "value", test_volume:noisy1)
#' 
#' ggplot(x, aes(time_index, value, color = key)) + geom_point()
simulate_lab_data <- function(test_name, years, by = c("day", "week", "month", "year"), 
                              mean_volume = 100, sd_volume = 20, 
                              trend = FALSE, seasonality = FALSE, 
                              predictor_variables = 0, noisy_variables = 0,
                              shape = c("wide", "long")) {
  # test_name = "a"; years = 3; by = "month"; mean_volume = 100; sd_volume = 20; trend = TRUE; seasonality = TRUE;  predictor_variables = 2; noisy_variables = 1; shape = "long"
  
  points_per_year <- c("day" = 365, "week" = 52, "month" = 12, "year" = 1)
  points_per_year <- points_per_year[names(points_per_year) == by]
  
  n <- years*points_per_year
  
  lab_data <- rnorm(n = n, mean = mean_volume, sd = sd_volume) 
  
  if(trend) {
    trend_bump <- seq(from = 0, to = 3*sd_volume, length.out = n) 
    
    lab_data <- lab_data + trend_bump
  }
  
  if(seasonality) {
    if(by == "year") stop("can't have seasonality with yearly numbers")
    
    season_wave <- cos(seq(0, 2*pi, length.out = points_per_year)) * 2*sd_volume
    
    lab_data <- lab_data + season_wave 
  }
  
  lab_data <- round(lab_data)
  lab_data[lab_data < 0] <- 0
  
  lab_data_frame <- data.frame(time_index = 1:n, test_name = test_name, 
                               year = rep(1:years, each = points_per_year), 
                               by = rep(1:points_per_year, years),
                               test_volume = lab_data, 
                               stringsAsFactors = FALSE) 
  names(lab_data_frame)[names(lab_data_frame) == "by"] <- by
  
  if(predictor_variables > 0) {
    pred_var <- lab_data + rnorm(n = n, mean = 0, sd = sd_volume)
    pred_var[pred_var < 0] <- 0
    pred_var_data_frame <- data.frame(pred_var = pred_var)
    
    if(predictor_variables > 1) {
       split_predictor <- lapply(pred_var_data_frame$pred_var, function(x) {
         split_number <- rmultinom(n = 1, size = x, 
                                   prob = sample(1:100, predictor_variables))
         as.data.frame(t(split_number))
       })
       
       pred_var_data_frame <- Reduce(rbind, split_predictor)
    }
    
    for(i in 1:ncol(pred_var_data_frame)) {
      lab_data_frame[[paste0("predictor", i)]] <- round(pred_var_data_frame[[i]])
    }
  }
  
  if(noisy_variables > 0) {
    for(j in 1:noisy_variables) {
      lab_data_frame[[paste0("noisy", j)]] <- rnorm(n = n, mean = mean_volume,
                                                    sd = sd_volume)
    }
  }
  
  attr(lab_data_frame, "class") <- c("lab_data", "data.frame")
  attr(lab_data_frame, "years") <- unname(years)
  attr(lab_data_frame, "by") <- by
  attr(lab_data_frame, "points_per_year") <- unname(points_per_year)
  
  if(shape[1] == "long" & predictor_variables + noisy_variables > 0) {
    gather_columns <- "test_volume"
    if(predictor_variables > 0) {
      gather_columns <- c(gather_columns, paste0("predictor", 1:predictor_variables))
    }
    if(noisy_variables > 0) {
      gather_columns <- c(gather_columns, paste0("noisy", 1:noisy_variables))
    }
    lab_data_frame <- tidyr::gather(lab_data_frame, "variable", "value",
                                    !!gather_columns) %>%
      arrange(time_index, desc(variable))
  }
  
  lab_data_frame
}


 
