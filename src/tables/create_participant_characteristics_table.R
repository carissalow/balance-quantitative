# Descriptive statistics of CAB, Phase 0, and Phase 1 participant characteristics

library(tidyverse)
library(gtsummary)


#### settings ----

stat_decimals <- 1
label_col_width_px <- 500
google_font_name <- "Roboto"

input_file_path <- "data/interim/"
input_file_name <- "balance_demographics.csv"

output_file_path <- "output/tables/"
output_file_name <- "balance_participant_characteristics" 


#### format data for table ----

demos <- read_csv(here::here("balance-quantitative", input_file_path, input_file_name))

demos_to_table <- demos %>%
  mutate(
    cohort = case_when(
      cohort == "cab" ~ "CAB",
      cohort == "phase0" ~ "Phase 0",
      cohort == "phase1" ~ "Phase 1"
    ),
    cohort = factor(
      cohort,
      levels = c("CAB", "Phase 0", "Phase 1")
    ),
    gender = factor(
      gender, 
      levels = c("Female", "Male", "Non-binary")
    ),
    race = factor(
      race_combined, 
      levels = c("White/Caucasian", "Black/African American", "White/Caucasian & Black/African American")
    ),
    marital = factor(
      marital, 
      levels = c("Married", "Divorced/separated", "Widowed", "Never married")
    ),
    living = case_when(
      grepl("Partner|Children", living_combined) ~ paste0("With ", living_combined),
      TRUE ~ living_combined
    ),
    living = factor(
      str_to_sentence(living), 
      levels = c("Alone", "With children", "With partner", "With partner & children", "Other")
    ),
    education = factor(
      education,
      levels = c(
        "High school diploma or equivalent", 
        "Some college, no degree", 
        "Associate of arts of other 2-year degree",
        "Bachelor's degree", 
        "Graduate degree"
      )
    ),
    employment = factor(
      employment,
      levels = c("Working full time", "Working part time", "Not employed", "Retired", "Other")
    ),
    cancer_status = factor(
      cancer_status,
      levels = c("Stable", "Progressing", "In remission", "Not sure")
    ),
    cancer_category_participant = factor(
      cancer_category_participant,
      levels = c("Colon", "Lung", "Pancreatic", "Prostate", "Other")
    ),
    across(
      .cols = starts_with("cancer_treatment_") & !contains("combined"),
      .fns = function(x) factor(x, levels = c("Yes", "No"))
    )
  ) %>%
  select(
    cohort, record_id, age, gender, race, marital, living, employment, 
    education, cancer_category_participant, cancer_status, 
    (starts_with("cancer_treatment_") & !contains("combined")), 
    starts_with("months_"), starts_with("days_"), dhls_score
  )


#### create the table ----

demos_table <- demos_to_table %>%
  tbl_summary(
    include = -c(record_id),
    by = cohort,
    type = list(
      age ~ "continuous",
      dhls_score ~ "continuous",
      starts_with("months_") ~ "continuous",
      starts_with("days_") ~ "continuous",
      starts_with("cancer_treatment") ~ "categorical"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) [{min}, {max}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(stat_decimals, stat_decimals, 0, 0)
    ),
    label = list(
      age ~ "Age, years",
      gender ~ "Gender",
      race ~ "Race",
      marital ~ "Marital status",
      living ~ "Living situation",
      employment ~ "Employment situation",
      education ~ "Education",
      cancer_category_participant ~ "Cancer type",
      cancer_status ~ "Cancer status",
      months_since_diagnosed_at_demo_datetime ~ "Time since cancer diagnosis, months",
      months_since_diagnosed_stage4_at_demo_datetime ~ "Time since stage IV diagnosis, months",
      days_since_treatment_at_demo_datetime ~ "Time since most recent treatment, days",
      dhls_score ~ "Digital Health Literacy Scale (DHLS) score"
    )
  )

# hacky way to add "header" row for cancer treatment type items
insert_row_location <- grep("cancer_treatment_", demos_table$table_body$variable)[1]
insert_row <- tribble(
  ~variable, ~var_type, ~var_label, ~row_type, ~label, 
  "cancer_treatment", "categorical", "cancer_treatment", "label", "Cancer treatment"
)

demos_table$table_body <- demos_table$table_body %>%
  mutate(
    label = ifelse(grepl("cancer_treatment_", variable), paste0("\t", label), label),
    label = gsub("cancer_treatment_", "", label),
  )

demos_table$table_body <- bind_rows(
  demos_table$table_body[1:insert_row_location-1, ],
  insert_row,
  demos_table$table_body[insert_row_location:nrow(demos_table$table_body), ]
)

demos_table <- demos_table %>%
  bold_labels() %>%
  modify_column_indent(
    columns = label,
    rows = grepl("cancer_treatment_", variable),
    double_indent = TRUE
  ) %>%
  as_gt()


#### save output ----

# Word doc for paper
demos_table %>%
  gt::gtsave(
    filename = here::here("balance-quantitative", output_file_path, glue::glue("{output_file_name}.docx"))
  )

# html for report
demos_table %>%
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
