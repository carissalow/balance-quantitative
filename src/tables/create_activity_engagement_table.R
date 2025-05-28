# Table of summary stats characterizing participant engagement with
# activity tracking during the intervention: activity selection 
# (phase 1 and phase 2), endorsement and rating (phase 1), and 
# planning and completion (phase 2)  

library(tidyverse)
library(gtsummary)

#### settings ----

stat_decimals <- 1
range_decimals <- 1
range_decimals_freq <- 0
label_col_width_px <- 450
google_font_name <- "Roboto"

parent_file_path <- "balance-quantitative"
input_file_path <- "data/processed/"
input_file_name <- "balance_activity_engagement.csv"
output_file_path <- "output/tables/"
output_file_name <- "balance_activity_engagement"

#### format data ----

activity_engagement <- read_csv(here::here(parent_file_path, input_file_path, input_file_name)) 

activity_engagement_to_table <- activity_engagement %>%
  mutate(
    selected_phase1_custom_prop = selected_phase1_custom/selected_phase1_all,
    selected_phase2_custom_prop = selected_phase2_custom/selected_phase2_all,
    selected_phase2_not_phase1_all_prop = selected_phase2_not_phase1_all/selected_phase2_all,
    across(
      .cols = matches("_rate_|_prop$"),
      .fns = function(x) x*100
    )
  )

selected_stats <- list(
  "selected_phase1_all" = "Selected activities, n",
  "selected_phase1_custom" = "Custom selected activities, n",
  "selected_phase1_custom_prop" = "Custom selected activities, %",
  "phase1_total_endorsed" = "Total endorsed activities, n",
  "phase1_unique_endorsed" = "Unique endorsed activities, n",
  "phase1_total_rated" = "Total rated activities, n",
  "phase1_endorsed_activity_rating_rate_mean" = "Daily endorsed activity rating rate, mean %",
  "phase1_activity_rating_mean" = "Activity rating, mean",
  "selected_phase2_all" = "Selected activities, n",
  "selected_phase2_custom" = "Custom selected activities, n",
  "selected_phase2_custom_prop" = "Custom selected activities, %",
  "selected_phase2_not_phase1_all" = "Newly added activities, n",
  "selected_phase2_not_phase1_all_prop" = "Newly added activities, %", # all newly added activities were custom
  "phase2_total_planned" = "Total planned activities, n",
  "phase2_total_completed" = "Total completed activities, n",
  "phase2_total_planned_and_completed" = "Total planned and completed activities, n",
  "phase2_unique_planned_and_completed" = "Unique planned and completed activities, n",
  "phase2_planned_activity_completion_rate_mean" = "Daily planned activity completion rate, mean %"
)


#### create table ---

activity_table <- activity_engagement_to_table %>%
  select(record_id, names(selected_stats)) %>%
  tbl_summary(
    include = -c(record_id, ends_with("list"), ends_with("surveys")),
    label = selected_stats,
    type = list(
      starts_with("selected") ~ "continuous",
      starts_with("phase") ~ "continuous"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) [{min}, {max}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() & !contains("mean") ~ c(stat_decimals, stat_decimals, range_decimals_freq, range_decimals_freq),
      all_continuous() & contains("mean") ~ c(stat_decimals, stat_decimals, range_decimals, range_decimals)
    ),
    missing = "no"
  )
 
insert_row_locations <- c(
  min(grep("selected_phase1", activity_table$table_body$variable)),
  min(grep("^phase1", activity_table$table_body$variable)),
  min(grep("selected_phase2", activity_table$table_body$variable)),
  min(grep("^phase2", activity_table$table_body$variable))
)

insert_rows <- tribble(
  ~variable, ~var_type, ~var_label, ~row_type, ~label, 
  "phase1", "categorical", "phase1", "label", "Phase 1",
  "selected_phase1", "categorical", "phase1", "label", "\tActivity selection",
  "endorsed_phase1", "categorical", "phase1", "label", "\tActivity endorsement and rating",
  "phase2", "categorical", "phase2", "label", "Phase 2",
  "selected_phase2", "categorical", "phase2", "label", "\tActivity selection",
  "planned_phase2", "categorical", "phase2", "label", "\tActivity planning and completion"
)

activity_table$table_body <- activity_table$table_body %>%
  mutate(
    row_type = "level",
    label = ifelse(grepl("phase", variable), paste0("\t\t", label), label)
  )

activity_table$table_body <- bind_rows(
  insert_rows[1:2, ],
  activity_table$table_body[1:(insert_row_locations[2]-1), ],
  insert_rows[3, ],
  activity_table$table_body[insert_row_locations[2]:(insert_row_locations[3]-1), ],
  insert_rows[4:5, ],
  activity_table$table_body[insert_row_locations[3]:(insert_row_locations[4]-1), ],
  insert_rows[6, ],
  activity_table$table_body[insert_row_locations[4]:nrow(activity_table$table_body), ]
)

activity_table <- activity_table %>%
  bold_labels() %>%
  modify_column_indent(
    columns = label,
    rows = grepl("^\t", label) & !grepl("^\t\t", label),
    double_indent = FALSE
  ) %>%
  modify_column_indent(
    columns = label,
    rows = grepl("^\t\t", label),
    double_indent = TRUE
  ) %>%
  as_gt()


#### save output ----

# Word doc for paper
activity_table %>%
  gt::gtsave(
    filename = here::here(parent_file_path, output_file_path, glue::glue("{output_file_name}.docx"))
  )

# html for report
activity_table %>%
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
