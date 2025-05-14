# Compute Phase 1 participant study survey compliance data 

library(tidyverse)

#### settings ----

input_file_path <- "data/interim/"
input_file_name <- "balance_survey_compliance_data.csv"
output_file_path <- "data/processed/"
output_file_name <- "balance_survey_compliance.csv"

compliance_data <- read_csv(here::here("balance-quantitative", input_file_path, input_file_name))


#### compute compliance ----

compliance <- compliance_data %>%
  select(
    record_id, date, phase, phase_duration, 
    number_surveys_completed, number_surveys_expected, 
    starts_with("has_survey")
  ) %>%
  summarize(
    .by = c(record_id, phase, phase_duration),
    across(
      .cols = contains("survey"),
      .fns = function(x) sum(x, na.rm = TRUE),
      .names = "total_{.col}"
    )
  ) %>%
  pivot_wider(
    id_cols = record_id, 
    names_from = phase, 
    values_from = -c(record_id, phase), 
    names_prefix = "phase"
  ) %>%
  select(
    -matches("daily.*phase2|morning.*phase1|evening.*phase1")
  ) %>%
  mutate(
    total_number_surveys_completed_overall = total_number_surveys_completed_phase1 + total_number_surveys_completed_phase2,
    total_number_surveys_expected_overall = total_number_surveys_expected_phase1 + total_number_surveys_expected_phase2,
    total_days_with_any_surveys_overall = total_has_survey_daily_phase1 + total_has_survey_morning_or_evening_phase2,
    total_days_with_all_surveys_overall = total_has_survey_daily_phase1 + total_has_survey_morning_and_evening_phase2,
    total_duration_overall = phase_duration_phase1 + phase_duration_phase2
  ) %>%
  mutate(
    rate_expected_surveys_completed_overall = total_number_surveys_completed_overall / total_number_surveys_expected_overall,
    rate_expected_surveys_completed_phase1 = total_number_surveys_completed_phase1 / total_number_surveys_expected_phase1,
    rate_expected_surveys_completed_phase2 = total_number_surveys_completed_phase2 / total_number_surveys_expected_phase2,
    rate_days_with_surveys_overall_any = total_days_with_any_surveys_overall / total_duration_overall,
    rate_days_with_surveys_overall_all = total_days_with_all_surveys_overall / total_duration_overall,
    rate_days_with_surveys_phase1_daily = total_has_survey_daily_phase1 / phase_duration_phase1,
    rate_days_with_surveys_phase2_morning = total_has_survey_morning_phase2 / phase_duration_phase2,
    rate_days_with_surveys_phase2_evening = total_has_survey_evening_phase2 / phase_duration_phase2,
    rate_days_with_surveys_phase2_morning_or_evening = total_has_survey_morning_or_evening_phase2 / phase_duration_phase2,
    rate_days_with_surveys_phase2_morning_and_evening = total_has_survey_morning_and_evening_phase2 / phase_duration_phase2
  ) 

#### save output ----

write_csv(compliance, file = here::here("balance-quantitative", output_file_path, output_file_name))