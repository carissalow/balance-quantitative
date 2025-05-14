# Descriptive statistics of Phase 1 participant feedback

library(tidyverse)
library(gtsummary)

#### setttings ----

stat_decimals <- 1
label_col_width_px <- 650
google_font_name <- "Roboto"

config <- yaml::read_yaml(here::here("balance-quantitative", "config.yaml"))

input_file_path <- "data/raw/"
input_file_name <- config$raw_data$phase1$feedback

output_file_path <- "output/tables/"
output_file_name <- "balance_phase1_feedback"


#### create table ----

phase1_feedback <- read_csv(here::here("balance-quantitative", input_file_path, input_file_name))

sus_items <- grep("^sus[1-9.*]", names(phase1_feedback), value = TRUE)

phase1_feedback_table <- phase1_feedback %>%
  mutate(
    data_sharing_others_score = ifelse(data_sharing_others_score == 1, "Yes", "No"),
    data_sharing_others_score = factor(
      data_sharing_others_score, levels = c("Yes", "No")
    )
  ) %>%
  relocate(data_sharing_others_score, .after = everything()) %>%
  select(-all_of(sus_items)) %>%
  tbl_summary(
    include = -c(record_id, sus_answered_eos),
    type = list(
      starts_with("sus_") ~ "continuous",
      starts_with("app_") ~ "continuous",
      starts_with("study_") ~ "continuous",
      starts_with("data_") & contains("providers") ~ "continuous",
      starts_with("data_") & contains("others") ~ "categorical"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) [{min}, {max}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(stat_decimals, stat_decimals, 0, 0)
    ),
    label = list(
      sus_score_eos = "System Usability Scale (SUS) Score",
      app_interest_self = "Keep using the BALANCE app on your own",
      app_interest_score_friend = "Recommend the BALANCE app to a friend with cancer",
      study_value_score_phase1evening = "Nightly surveys during Phase 1",
      study_value_score_calendar = "Calendar visualizations in the app",
      study_value_score_report = "Mid-study data report",
      study_value_score_review = "Mid-study data review meeting",
      study_value_score_phase2morning = "Selecting the activities you want to perform each morning of Phase 2",
      study_value_score_phase2evening = "Nightly check-ins during Phase 2",
      data_sharing_providers_score = "Sharing any of the data you collected with your care team",
      data_sharing_others_score = "Did you ever share any of the data you collected during this study with anyone else?"
    )
  )

# create sub-headings
  
interest_header <- "On a scale of 0 to 10, how likely would you be to:" 
value_header <- "On a scale of 0 to 10, how valuable did you find the following aspects of the study:"
sharing_header <- "On a scale of 0 to 10, how interested would you be in:"

insert_row_locations <- c(
  grep("app_interest_", phase1_feedback_table$table_body$variable)[1],
  grep("study_value_", phase1_feedback_table$table_body$variable)[1],
  grep("data_sharing_providers", phase1_feedback_table$table_body$variable)[1]
)

insert_rows <- tribble(
  ~variable, ~var_type, ~var_label, ~row_type, ~label, 
  "app_interest", "continuous", "app_interest_header", "label", interest_header,
  "study_value", "continuous", "study_value_header", "label", value_header,
  "data_sharing_providers", "continuous", "data_sharing_providers_header", "label", sharing_header
)

phase1_feedback_table$table_body <- phase1_feedback_table$table_body %>%
  mutate(
    label = ifelse(grepl(paste0(insert_rows$variable, collapse = "|"), variable), paste0("\t", label), label),
    row_type = ifelse(grepl(paste0(insert_rows$variable, collapse = "|"), variable), "level", row_type)
  )

phase1_feedback_table$table_body <- bind_rows(
  phase1_feedback_table$table_body[1:(insert_row_locations[1]-1), ],
  insert_rows[1, ],
  phase1_feedback_table$table_body[insert_row_locations[1]:(insert_row_locations[2]-1), ],
  insert_rows[2, ],
  phase1_feedback_table$table_body[insert_row_locations[2]:(insert_row_locations[3]-1), ],
  insert_rows[3, ],
  phase1_feedback_table$table_body[insert_row_locations[3]:nrow(phase1_feedback_table$table_body), ]
)
  
phase1_feedback_table <- phase1_feedback_table %>% 
  bold_labels() %>%
  modify_header(
    label = "**Characteristic**"
  ) %>%
  modify_column_indent(
    columns = label,
    rows = (grepl(paste0(insert_rows$variable, collapse = "|"), variable) & !grepl("header$", var_label)),
    double_indent = FALSE
  ) %>%
  as_gt()


#### save output ----

# Word doc for paper
phase1_feedback_table %>%
  gt::gtsave(
    filename = here::here("balance-quantitative", output_file_path, glue::glue("{output_file_name}.docx"))
  )

# html for report
phase1_feedback_table %>%
  gt::opt_table_font(
    font = list(
      gt::google_font(name = google_font_name)
    )
  ) %>%
  gt::cols_width(
    starts_with("label") ~ px(label_col_width_px)
  ) %>%
  gt::gtsave(
    filename = here::here("balance-quantitative", output_file_path, glue::glue("{output_file_name}.html"))
  )
