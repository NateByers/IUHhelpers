library(IUHhelpers)
library(purrr)
library(dplyr)

parameters <- read.csv("data-raw/simulate_parameters.csv",
                       stringsAsFactors = FALSE) %>%
  mutate(noisy_variables = 2 - predictor_variables)

simulations <- pmap(parameters, simulate_lab_data) %>%
  bind_rows() 

write.csv(simulations, file = "data-raw/simulated_lab_data.csv",
          row.names = FALSE)
