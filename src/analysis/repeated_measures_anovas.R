# Repeated measures ANOVAs

library(tidyverse)
library(rstatix)

#### helper functions ----

repeated_measures_anova <- function(data, outcome, time, id, p_adjust_method = "none") {
  required_cols <- c(id, time, outcome)
  if (!all(required_cols %in% colnames(data))) {
    stop("One or more columns is missing from the data")
  }
  
  # limit to complete cases (participants with data at all timepoints)
  # order by participant ID to ensure data are paired properly for paired t-test
  data_for_test <- data %>%
    dplyr::select(all_of(required_cols)) %>%
    mutate(
      .by = {{id}},
      n_na = sum(is.na(get(outcome)))
    ) %>%
    filter(n_na == 0) %>% 
    dplyr::select(-n_na) %>%
    arrange(get(id), get(time)) 
  
  # summary stats 
  summary_stats_by_timepoint <- data_for_test %>%
    summarize(
      .by = {{time}},
      across(
        .cols = all_of(outcome),
        .fns = list(mean = mean, sd = sd, min = min, max = max),
        .names = "{.fn}"
      )
    ) %>%
    pivot_wider(
      names_from = {{time}},
      values_from = -all_of(time)
    )

  # repeated measures ANOVA
  anova_formula <- glue::glue("{outcome} ~ {time} + Error({id}/{time})")
  anova_res <- anova_test(
    data = data_for_test,
    formula = as.formula(anova_formula)
  )
  
  # post-hoc pairwise paired t-tests
  ttest_formula <- gsub(" \\+ Error.*$", "", anova_formula)
  ttest_res <- pairwise_t_test(
    data = data_for_test,
    formula = as.formula(ttest_formula),
    paired = TRUE,
    pool.sd = FALSE,
    p.adjust.method = p_adjust_method,
    detailed = TRUE,
    comparisons = list(
      c(2,1),
      c(3,1),
      c(3,2)
    )
  )
  
  # collect results
  out <- data.frame(
    outcome = outcome,
    n = length(unique(data_for_test[[id]])),
    anova_formula = as.character(anova_formula),
    anova = list(anova_res$ANOVA),
    sphericity = list(anova_res$`Mauchly's Test for Sphericity`),
    correction = list(anova_res$`Sphericity Corrections`),
    ttest_formula = as.character(ttest_formula),
    ttest_paired = attributes(ttest_res)$args$paired,
    ttest_poolsd = attributes(ttest_res)$args$pool.sd,
    ttest = ttest_res,
    p_adj_method = p_adjust_method,
    row.names = NULL
  )
  
  out <- out %>% 
    dplyr::select(-ends_with("05"), -ends_with("signif")) %>%
    rename_all(
      function(x) x %>% 
        gsub("\\.$", "", .) %>%
        gsub("\\.\\.", "_", .) %>%
        gsub("\\.", "_", .)
    ) %>%
    bind_cols(summary_stats_by_timepoint)
  
  return(out)
}


#### settings ----

date_suffix <- gsub("-", "", today())

parent_file_path <- "balance-quantitative"
input_file_path <- "data/interim/"
input_file_name <- "balance_outcomes.csv"
output_file_path <- "output/results"
output_file_name <- glue::glue("repeated_measures_anovas_{date_suffix}.csv")

outcomes <- read_csv(here::here(parent_file_path, input_file_path, input_file_name))


#### format data ----

scale_score_cols <- grep(".*_score$", colnames(outcomes), value = TRUE)
promis_score_cols <- grep("^promis_.*_t_.*$", colnames(outcomes), value = TRUE)
score_cols <- c(scale_score_cols, promis_score_cols)

outcomes_for_analysis <- outcomes %>%
  mutate(
    timepoint = as.factor(timepoint_number)
  ) %>%
  dplyr::select(record_id, timepoint, all_of(score_cols)) %>%
  arrange(record_id, timepoint)

#### repeated measures ANOVAs with post-hoc pairwise paired t-tests ----

anova_results <- data.frame()

for (outcome in score_cols) {
  result <- repeated_measures_anova(
    data = outcomes_for_analysis,
    outcome = outcome,
    time = "timepoint",
    id = "record_id",
    p_adjust_method = "none"
  )
  anova_results <- bind_rows(anova_results, result)
}

write_csv(anova_results, here::here(parent_file_path, output_file_path, output_file_name))
