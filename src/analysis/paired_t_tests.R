# Paired t-tests

library(tidyverse)
library(rstatix)

#### helper functions ----

paired_t_test <- function(data, x, y, id) {
  required_cols <- c(id, x, y)
  if (!all(required_cols %in% colnames(data))) {
    stop("One or more required columns is not present in the data")
  }
  
  # limit to complete cases (participants with data at all timepoints)
  data_for_test <- data %>%
    dplyr::select(all_of(required_cols)) %>%
    drop_na() %>%
    arrange(get(id))
  
  # paired t-test
  res <- t.test(x = data_for_test[[x]], y = data_for_test[[y]], paired = TRUE) 
  
  # effect size
  d_res <- data_for_test %>%
    pivot_longer(
      cols = -all_of(id),
      names_to = "x", 
      values_to = "y"
    ) %>%
    arrange(get(id), x) %>%
    mutate(x = forcats::fct_rev(factor(x))) %>%
    rstatix::cohens_d(
      data = .,
      formula = y ~ x,
      paired = TRUE
    )
  
  out <- do.call(cbind, res) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    pivot_wider(
      id_cols = everything(),
      values_from = conf.int,
      names_from = rowname,
      names_prefix = "conf.int"
    ) %>%
    dplyr::select(-data.name) %>%
    mutate(
      effect_size_cohensd = d_res$effsize,
      effect_size_magnitude = d_res$magnitude,
      data_x = x,
      data_y = y,
      data_x_mean = mean(data_for_test[[x]], na.rm = TRUE),
      data_x_sd = sd(data_for_test[[x]], na.rm = TRUE),
      data_x_min = min(data_for_test[[x]], na.rm = TRUE),
      data_x_max = max(data_for_test[[x]], na.rm = TRUE),
      data_y_mean = mean(data_for_test[[y]], na.rm = TRUE),
      data_y_sd = sd(data_for_test[[y]], na.rm = TRUE),
      data_y_min = min(data_for_test[[y]], na.rm = TRUE),
      data_y_max = max(data_for_test[[y]], na.rm = TRUE),
      n = length(unique(data_for_test[[id]])),
    ) %>%
    rename_all(function(x) gsub("\\.", "_", x)) %>%
    rename(
      t = statistic,
      df = parameter,
      mean_difference = estimate
    ) %>%
    dplyr::select(
      n, starts_with("data"), method, alternative, everything()
    )
  
  return(out)
}

read_averages <- function(input_file, input_file_path, parent_file_path) {
  data <- read_csv(here::here(parent_file_path, input_file_path, input_file))
  if ("phase" %in% colnames(data)) {
    data <- data %>%
      filter(phase != "overall") %>%
      mutate(phase = as.integer(phase)) %>%
      rename(study_phase = phase)
  }
  return(data)
}


#### settings ----

date_suffix <- gsub("-", "", today())

parent_file_path <- "balance-quantitative"
input_file_path <- "data/processed/"
input_file_names <- c("balance_average_goodness.csv", "balance_average_fitbit_features.csv")
output_file_path <- "output/results"
output_file_name <- glue::glue("paired_ttests_{date_suffix}.csv")

join_cols <- c("record_id", "study_phase")
outcome_cols <- c(
  "goodness_score_mean", 
  "goodness_score_n", 
  "fitbit_steps_intraday_rapids_sumsteps_mean",
  "fitbit_steps_intraday_rapids_sumsteps_n"
)


#### format data for analysis ----

outcomes <- input_file_names %>%
  purrr::map(
    function(x) read_averages(x, input_file_path, parent_file_path) 
  ) %>%
  purrr::reduce(full_join, by = join_cols)

outcomes_wide <- outcomes %>%
  pivot_wider(
    id_cols = record_id,
    names_from = study_phase,
    values_from = all_of(outcome_cols),
    names_glue = "{.value}_phase{study_phase}"
  ) %>%
  arrange(record_id) %>%
  mutate( 
    across(
      .cols = ends_with("phase2"),
      .fns = function(x) get(cur_column()) - get(gsub("2$", "1", cur_column())),
      .names = "{gsub('_phase.*', '', .col)}_difference"
    )
  ) 


#### paired sample t-tests ----

ttest_results <- data.frame()

for (outcome in outcome_cols) {
  x_col <- paste0(outcome, "_phase2")
  y_col <- paste0(outcome, "_phase1")
  
  result <- paired_t_test(
    data = outcomes_wide, 
    x = x_col, 
    y = y_col, 
    id = "record_id"
  )
  result <- result %>%
    mutate(outcome = outcome) %>%
    relocate(outcome, .before = everything())
  
  ttest_results <- bind_rows(ttest_results, result)
}

write_csv(ttest_results, here::here(parent_file_path, output_file_path, output_file_name))
