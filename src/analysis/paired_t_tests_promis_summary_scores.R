library(tidyverse)

#### functions ----

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
  
  cohensd <- data_for_test %>%
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
      cohensd_effect_size = cohensd$effsize,
      cohensd_magnitude = cohensd$magnitude,
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
      mean_difference = estimate,
    ) %>%
    dplyr::select(
      n, starts_with("data"), method, alternative, everything()
    )
  
  return(out)
}


#### settings ----

date_suffix <- gsub("-", "", today())

input_file_path <- "balance-quantitative/data/interim/"
output_file_path <- "balance-quantitative/output/results/"
input_file <- "balance_promis_physical_mental_health_summary_scores.csv" 
output_file <- glue::glue("paired_ttests_promis_summary_scores_{date_suffix}.csv")

outcome_cols <- c("physical_health_summary_t", "mental_health_summary_t")


#### load data ----

scores <- read_csv(here::here(input_file_path, input_file))

scores <- scores %>%
  filter(timepoint != "mp") %>%
  pivot_wider(
    id_cols = record_id,
    names_from = timepoint,
    values_from = all_of(outcome_cols)
  )

#### paired t-test ----

ttest_results <- data.frame()

for (outcome in outcome_cols) {
  x_col <- paste0(outcome, "_eos")
  y_col <- paste0(outcome, "_bl")
  
  result <- paired_t_test(
    data = scores, 
    x = x_col, 
    y = y_col, 
    id = "record_id"
  )
  result <- result %>%
    mutate(outcome = outcome) %>%
    relocate(outcome, .before = everything())
  
  ttest_results <- bind_rows(ttest_results, result)
}

write_csv(ttest_results, file=here::here(output_file_path, output_file))
