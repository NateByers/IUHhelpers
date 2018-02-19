
#' @export
#' @import parallel
generate_lab_data <- function(years, by = c("day", "week", "month", "year"), 
                              mean_volume = 100, sd_volume = 20, 
                              trend = FALSE, seasonality = FALSE, 
                              predictor_variables = 0, noisy_variables = 0) {
  # years = 3; by = "week"; mean_volume = 100; sd_volume = 20; trend = TRUE; seasonality = TRUE;  predictor_variables = 2; noisy_variables = 0
  
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
  
  lab_data_frame <- data.frame(index = 1:n, year = 1:years, 
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
  
  lab_data_frame
}


 
