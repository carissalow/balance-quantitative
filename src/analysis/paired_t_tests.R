# Paired t-tests

library(tidyverse)

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
  
  res <- t.test(x = data_for_test[[x]], y = data_for_test[[y]], paired = TRUE)
  
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
      data_x = x,
      data_y = y,
      n = length(unique(data_for_test[[id]])),
    ) %>%
    rename_all(function(x) gsub("\\.", "_", x)) %>%
    rename(
      t = statistic,
      df = parameter,
      mean_difference = estimate,
    ) %>%
    dplyr::select(
      n, starts_with("data"), method, alternative, everything()
    )
  
  return(out)
}


#### settings ----

date_suffix <- gsub("-", "", today())

input_file_path <- "data/processed/"
input_file_names <- c("balance_average_goodness.csv")
#input_file_names <- c("balance_average_goodness.csv", "balance_average_fitbit.csv")
output_file_path <- "output/results"
output_file_name <- glue::glue("paired_ttests_{date_suffix}.csv")

join_cols <- c("record_id", "study_phase")
outcome_cols <- c("goodness_score_mean")
#outcome_cols <- c("goodness_score_mean", "fitbit_steps_intraday_rapids_sumsteps_mean")


#### format data for analysis ----

outcomes <- input_file_names %>%
  purrr::map(
    function(x) read_csv(here::here("balance-quantitative", input_file_path, x))
  ) %>%
  purrr::reduce(full_join, by = join_cols)

outcomes_wide <- outcomes %>%
  pivot_wider(
    id_cols = record_id,
    names_from = study_phase,
    values_from = any_of(outcome_cols),
    names_glue = "{.value}_phase{.name}"
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

write_csv(ttest_results, here::here("balance-quantitative", output_file_path, output_file_name))
