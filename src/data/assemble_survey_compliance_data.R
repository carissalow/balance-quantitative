# Assemble Phase 1 participant study phase 1 daily, phase 2 morning, and phase 2 
# evening survey compliance data 

library(tidyverse)

#### settings ----

parent_file_path <- "balance-quantitative"
input_file_path <- "data/interim/"
input_file_name_dates <- "balance_study_dates.csv"
input_file_name_surveys <- "balance_surveys.csv"
output_file_path <- "data/interim/"
output_file_name <- "balance_survey_compliance_data.csv"

study_dates <- read_csv(here::here(parent_file_path, input_file_path, input_file_name_dates))
surveys <- read_csv(here::here(parent_file_path, input_file_path, input_file_name_surveys))


#### format data ----

surveys_clean <- surveys %>%
  select(
    record_id, date, phase = study_phase, 
    survey_type, survey_duration, goodness_score, added_note
  ) %>%
  mutate(
    survey_type = tolower(survey_type),
    has_survey = 1
  ) %>%
  pivot_wider(
    id_cols = c("record_id", "date", "phase"),
    names_from = "survey_type",
    values_from = c("has_survey", "survey_duration", "goodness_score", "added_note")
  ) %>%
  select(
    -c("goodness_score_morning")
  )

compliance_data <- study_dates %>%
  left_join(surveys_clean, by = c("record_id", "date", "phase")) %>%
  mutate(
    across(
      .cols = c(has_survey_daily),
      .fns = function(x) case_when(
        phase == 1 & is.na(x) ~ 0,
        phase != 1 & !is.na(x) ~ NA,
        TRUE ~ x
      )
    ),
    across(
      .cols = c(has_survey_morning, has_survey_evening),
      .fns = function(x) case_when(
        phase == 2 & is.na(x) ~ 0,
        phase != 2 & !is.na(x) ~ NA,
        TRUE ~ x
      )
    ),
    number_surveys_completed = rowSums(
      across(starts_with("has_survey_")),
      na.rm = TRUE
    ),
    number_surveys_expected = case_when(
      phase == 1 ~ 1,
      phase == 2 ~ 2,
      TRUE ~ NA_real_
    ),
    has_survey_morning_and_evening = case_when(
      phase == 2 & has_survey_morning == 1 & has_survey_evening == 1 ~ 1,
      phase == 2 & (has_survey_morning == 0 | has_survey_evening == 0) ~ 0,
      TRUE ~ NA_real_
    ),
    has_survey_morning_or_evening = case_when(
      phase == 2 & (has_survey_morning == 1 | has_survey_evening == 1) ~ 1,
      phase == 2 & has_survey_morning == 0 & has_survey_evening == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    phase_duration = as.numeric(phase_end_date - phase_start_date, units="days") + 1
  ) 

#### save output ----

write_csv(compliance_data, file = here::here(parent_file_path, output_file_path, output_file_name))
