library(IUHhelpers)
library(purrr)
library(dplyr)

parameters <- read.csv("simulate/simulate_parameters.csv",
                       stringsAsFactors = FALSE) %>%
  mutate(noisy_variables = 2 - predictor_variables)

simulations <- pmap(parameters, simulate_lab_data) %>%
  bind_rows() 

write.csv(simulations, file = "simulate/simulated_lab_data.csv",
          row.names = FALSE)
