# Average Fitbit features within and across study phases for each participant, 
# including only days with sufficient Fitbit wear time (i.e., data yield; based
# on intraday heart rate data)

library(tidyverse)

#### settings ----

parent_file_path <- "balance-quantitative"
input_file_path <- "data/interim/"
input_file_name <- "balance_fitbit_features.csv"
output_file_path <- "data/processed/"
output_file_name <- "balance_average_fitbit_features.csv"

config <- yaml::read_yaml(here::here(parent_file_path, "config.yaml"))

data_yield_feature <- config$fitbit$data_yield$feature
data_yield_threshold <- eval(parse(text = config$fitbit$data_yield$threshold))


#### compute feature averages ----

fitbit_features <- read_csv(here::here(parent_file_path, input_file_path, input_file_name))
data_yield_col <- grep(data_yield_feature, colnames(fitbit_features), value = TRUE)

average_fitbit_features <- fitbit_features %>%
  mutate(
    phase = as.character(phase),
    sufficient_fitbit_data = case_when(
      is.na(get(data_yield_col)) ~ 0,
      !is.na(get(data_yield_col)) & get(data_yield_col) < data_yield_threshold ~ 0,
      !is.na(get(data_yield_col)) & get(data_yield_col) >= data_yield_threshold ~ 1,
      TRUE ~ NA
    )
  ) %>%
  bind_rows((mutate(., phase = "overall"))) %>%
  summarize(
    .by = c(record_id, phase),
    n_days = n(),
    n_days_with_sufficient_data = sum(sufficient_fitbit_data),
    across(
      .cols = starts_with("fitbit"),
      .fns = list(
        n = function(x) sum(!is.na(x[sufficient_fitbit_data == 1])),
        mean = function(x) mean(x[sufficient_fitbit_data == 1], na.rm = TRUE),
        sd = function(x) sd(x[sufficient_fitbit_data == 1], na.rm = TRUE),
        min = function(x) min(x[sufficient_fitbit_data == 1], na.rm = TRUE),
        max = function(x) max(x[sufficient_fitbit_data == 1], na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  mutate(
    across(
      .cols = starts_with("fitbit"),
      .fns = function(x) ifelse(is.infinite(x), NaN, x)
    )
  ) 


#### save output -----

write_csv(average_fitbit_features, file = here::here(parent_file_path, output_file_path, output_file_name))
