# Descriptive statistics of Phase 1 participant survey compliance

library(tidyverse)
library(gtsummary)

#### setttings ----

stat_decimals <- 2
range_decimals <- 2
label_col_width_px <- 650
google_font_name <- "Roboto"

input_file_path <- "data/processed/"
input_file_name <- "balance_survey_compliance.csv"

output_file_path <- "output/tables/"
output_file_name <- "balance_phase1_survey_compliance"


#### create table ----

phase1_compliance <- read_csv(here::here("balance-quantitative", input_file_path, input_file_name))

phase1_compliance_table <- phase1_compliance %>%
  select(record_id, starts_with("rate")) %>%
  mutate(
    across(
    .cols = starts_with("rate"),
    .fns = function(x) x*100
    ) 
  ) %>%
  tbl_summary(
    include = -c(record_id),
    type = list(
      starts_with("rate_") ~ "continuous"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) [{min}, {max}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(stat_decimals, stat_decimals, range_decimals, range_decimals)
    )
  )

# rename labels
phase1_compliance_table$table_body <- phase1_compliance_table$table_body %>%
  mutate(
    label = label %>%
      gsub(".*completed_", "", .) %>%
      gsub(".*surveys_", "", .) %>%
      gsub("_", " ", .) %>%
      gsub("phase", "phase ", .) %>%
      gsub("overall ", "overall, ", .) %>%
      gsub("1 ", "1, ", .) %>%
      gsub("2 ", "2, ", .) %>%
      str_to_sentence() 
  )

# create sub-headings
  
rate_expected_header <- "Percent of expected surveys completed" 
rate_days_header <- "Percent of days with completed survey(s)"

insert_row_locations <- c(
  grep("rate_expected", phase1_compliance_table$table_body$variable)[1],
  grep("rate_days", phase1_compliance_table$table_body$variable)[1]
)

insert_rows <- tribble(
  ~variable, ~var_type, ~var_label, ~row_type, ~label, 
  "rate_expected", "continuous", "rate_expected_header", "label", rate_expected_header,
  "rate_days", "continuous", "rate_days_header", "label", rate_days_header
)

phase1_compliance_table$table_body <- phase1_compliance_table$table_body %>%
  mutate(
    label = ifelse(grepl(paste0(insert_rows$variable, collapse = "|"), variable), paste0("\t", label), label),
    row_type = ifelse(grepl(paste0(insert_rows$variable, collapse = "|"), variable), "level", row_type)
  )

phase1_compliance_table$table_body <- bind_rows(
  insert_rows[1, ],
  phase1_compliance_table$table_body[insert_row_locations[1]:(insert_row_locations[2]-1), ],
  insert_rows[2, ],
  phase1_compliance_table$table_body[insert_row_locations[2]:nrow(phase1_compliance_table$table_body), ]
)
  
phase1_compliance_table <- phase1_compliance_table %>% 
  bold_labels() %>%
  modify_header(
    label = "**Compliance**"
  ) %>%
  modify_column_indent(
    columns = label,
    rows = (grepl(paste0(insert_rows$variable, collapse = "|"), variable) & !grepl("header$", var_label)),
    double_indent = FALSE
  ) %>%
  as_gt()


#### save output ----

# Word doc for paper
phase1_compliance_table %>%
  gt::gtsave(
    filename = here::here("balance-quantitative", output_file_path, glue::glue("{output_file_name}.docx"))
  )

# html for report
phase1_compliance_table %>%
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
