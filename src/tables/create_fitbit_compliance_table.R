library(tidyverse)
library(gtsummary)

#### settings ----

stat_decimals <- 2
range_decimals <- 2
label_col_width_px <- 400
google_font_name <- "Roboto"

parent_file_path <- "balance-quantitative"
input_file_path <- "data/processed/"
input_file_name <- "balance_fitbit_compliance.csv"
output_file_path <- "output/tables/"
output_file_name <- "balance_fitbit_compliance"


#### format data ----

fitbit_compliance <- read_csv(here::here(parent_file_path, input_file_path, input_file_name))

fitbit_compliance <- fitbit_compliance %>%
  select(record_id, phase, starts_with("percent")) %>%
  mutate(
    phase = ifelse(phase != "overall", paste0("Phase ", phase), "Overall"),
    phase = factor(phase, levels = c("Overall", "Phase 1", "Phase 2"))
  ) %>%
  arrange(phase) %>%
  pivot_wider(
    id_cols = record_id,
    names_from = phase,
    values_from = starts_with("percent"),
    names_glue = "{.value}_{phase}"
  ) 


#### create table ----

fitbit_compliance_table <- fitbit_compliance %>%
  tbl_summary(
    include = -c(record_id),
    type = list(
      starts_with("percent_") ~ "continuous"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) [{min}, {max}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(stat_decimals, stat_decimals, range_decimals, range_decimals)
    )
  ) 

insert_row_header <- "Percent of days with sufficient Fitbit data" 
insert_row_location <- min(grep("percent_days_with_sufficient_data", fitbit_compliance_table$table_body$variable))

insert_row <- tribble(
  ~variable, ~var_type, ~var_label, ~row_type, ~label, 
  "percent_days_with_sufficient_data", "continuous", "percent_days_with_sufficient_data_header", "label", insert_row_header
)

fitbit_compliance_table$table_body <- fitbit_compliance_table$table_body %>%
  mutate(
    label = gsub("^percent_days_with_sufficient_data_", "", label),
    label = ifelse(grepl(paste0(insert_row$variable, collapse = "|"), variable), paste0("\t", label), label),
    row_type = ifelse(grepl(paste0(insert_row$variable, collapse = "|"), variable), "level", row_type)
  )

fitbit_compliance_table$table_body <- bind_rows(
  insert_row,
  fitbit_compliance_table$table_body[insert_row_location:nrow(fitbit_compliance_table$table_body), ]
)

fitbit_compliance_table <- fitbit_compliance_table %>% 
  bold_labels() %>%
  modify_header(
    label = "**Compliance**"
  ) %>%
  modify_column_indent(
    columns = label,
    rows = (grepl(paste0(insert_row$variable, collapse = "|"), variable) & !grepl("header$", var_label)),
    double_indent = FALSE
  ) %>%
  as_gt()


#### save output ----

# Word doc for paper
fitbit_compliance_table %>%
  gt::gtsave(
    filename = here::here(parent_file_path, output_file_path, glue::glue("{output_file_name}.docx"))
  )

# html for report
fitbit_compliance_table %>%
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
