# Compute Fitbit compliance

library(tidyverse)

#### settings ----

parent_file_path <- "balance-quantitative"
input_file_path <- "data/processed/"
input_file_name <- "balance_average_fitbit_features.csv"
output_file_path <- "data/processed/"
output_file_name <- "balance_fitbit_compliance.csv"

config <- yaml::read_yaml(here::here(parent_file_path, "config.yaml"))

data_yield_feature <- config$fitbit$data_yield$feature
data_yield_threshold <- eval(parse(text = config$fitbit$data_yield$threshold))


#### compute compliance ----

average_fitbit_features <- read_csv(here::here(parent_file_path, input_file_path, input_file_name))

fitbit_compliance <- average_fitbit_features %>%
  select(record_id, phase, n_days, n_days_with_sufficient_data) %>%
  mutate(
    prop_days_with_sufficient_data = n_days_with_sufficient_data/n_days,
    percent_days_with_sufficient_data = prop_days_with_sufficient_data*100,
    data_yield_feature = data_yield_feature,
    data_yield_threshold = data_yield_threshold
  ) 

#### save output ----

write_csv(fitbit_compliance, file = here::here(parent_file_path, output_file_path, output_file_name))
