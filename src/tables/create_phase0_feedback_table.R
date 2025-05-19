# Descriptive statistics of Phase 0 participant feedback

library(tidyverse)
library(gtsummary)

#### setttings ----

stat_decimals <- 1
label_col_width_px <- 500
google_font_name <- "Roboto"

parent_file_path <- "balance-quantitative"
input_file_path <- "data/raw/"
output_file_path <- "output/tables/"
output_file_name <- "balance_phase0_feedback"

config <- yaml::read_yaml(here::here(parent_file_path, "config.yaml"))

input_file_name <- config$raw_data$phase0$feedback


#### create table ----

phase0_feedback <- read_csv(here::here(parent_file_path, input_file_path, input_file_name))

phase0_feedback_table <- phase0_feedback %>%
  tbl_summary(
    include = -c(record_id),
    type = list(
      starts_with("willingness_") ~ "continuous",
      starts_with("app_") ~ "continuous"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) [{min}, {max}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(stat_decimals, stat_decimals, 0, 0)
    ),
    label = list(
      app_difficulty_num = "How **easy** was it to use the study application?",
      app_pleasantness_num = "How **pleasant** was it to use the study application?",
      app_burden_num = "How **burdensome** was it to use the study application?",
      willingness_participate_num = "How likely would you be to **participate** in this study or use this app?",
      willingness_recommend_num = "How likely would you be to **recommend** this study or app to a friend with advanced cancer?"
    )
  ) %>%
  modify_header(
    label = "**On a scale of 0 to 10:**"
  ) %>%
  as_gt() %>%
  gt::fmt_markdown(columns = c(label))


#### save output ----

# Word doc for paper
phase0_feedback_table %>%
  gt::gtsave(
    filename = here::here(parent_file_path, output_file_path, glue::glue("{output_file_name}.docx"))
  )

# html for report
phase0_feedback_table %>%
  gt::opt_table_font(
    font = list(
      gt::google_font(name = google_font_name)
    )
  ) %>%
  gt::cols_width(
    starts_with("label") ~ px(label_col_width_px)
  ) %>%
  gt::gtsave(
    filename = here::here(parent_file_path, output_file_path, glue::glue("{output_file_name}.html"))
  )
