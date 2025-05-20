# Aggregate daily activity endorsements (phase 1 and 2) and scheduling (phase 2)

library(tidyverse)

#### settings ----

parent_file_path <- "balance-quantitative"
input_file_path <- "data/interim"
input_file_name <- "balance_activity_endorsements.csv"
output_file_path <- "data/processed"


#### aggregate data ---- 

endorsements <- read_csv(here::here(parent_file_path, input_file_path, input_file_name))
endorsements <- distinct(endorsements)

overall_distinct_activities <- bind_rows(
  summarize(
    endorsements %>%
      mutate(
        study_phase = paste0("phase", study_phase)
      ),
    .by = c(record_id, study_phase, survey_type),
    n_distinct_activities = length(unique(activity_id))
  ),
  summarize(
    endorsements %>%
      filter(study_phase == 2) %>%
      mutate(
        study_phase = paste0("phase", study_phase),
        survey_type = "OVERALL"
      ),
    .by = c(record_id, study_phase, survey_type),
    n_distinct_activities = length(unique(activity_id))
  ),
  summarize(
    endorsements %>%
      mutate(
        study_phase = "overall",
        survey_type = "OVERALL"
      ),
    .by = c(record_id, study_phase, survey_type),
    n_distinct_activities = length(unique(activity_id))
  )
) %>%
arrange(record_id, study_phase, survey_type)

phase1_daily <- endorsements %>%
  filter(survey_type == "DAILY") %>%
  summarize(
    .by = c(record_id, date, study_phase, survey_type),
    n_activities_endorsed = n(),
    n_custom_activities_endorsed = sum(custom_activity == 1),
    n_activities_rated = sum(!is.na(activity_score) & activity_score > -1),
    n_custom_activities_rated = sum(custom_activity == 1 & !is.na(activity_score) & activity_score > -1),
    prop_endorsed_activities_rated = n_activities_rated/n_activities_endorsed,
    prop_custom_endorsed_activities_rated = n_custom_activities_rated/n_custom_activities_endorsed,
    mean_activity_rating = mean(ifelse(activity_score < 0, NA_real_, activity_score), na.rm = TRUE),
    sd_activity_rating = sd(ifelse(activity_score < 0, NA_real_, activity_score), na.rm = TRUE),
    min_activity_rating = min(ifelse(activity_score < 0, NA_real_, activity_score), na.rm = TRUE),
    max_activity_rating = max(ifelse(activity_score < 0, NA_real_, activity_score), na.rm = TRUE),
    mean_custom_activity_rating = mean(ifelse(activity_score < 0 | custom_activity == 0, NA_real_, activity_score), na.rm = TRUE),
    sd_custom_activity_rating = sd(ifelse(activity_score < 0 | custom_activity == 0, NA_real_, activity_score), na.rm = TRUE),
    min_custom_activity_rating = min(ifelse(activity_score < 0 | custom_activity == 0, NA_real_, activity_score), na.rm = TRUE),
    max_custom_activity_rating = mean(ifelse(activity_score < 0 | custom_activity == 0, NA_real_, activity_score), na.rm = TRUE)
  ) %>%
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = function(x) ifelse(is.infinite(x), NA_real_, x)
    )
  )

phase2_morning <- endorsements %>%
  filter(survey_type == "MORNING") %>%
  summarize(
    .by = c(record_id, date, study_phase, survey_type),
    n_activities_scheduled = n(),
    n_custom_activities_scheduled = sum(custom_activity == 1),
  )

phase2_evening <- endorsements %>%
  filter(survey_type == "EVENING") %>%
  summarize(
    .by = c(record_id, date, study_phase, survey_type),
    n_activities_endorsed = n(),
    n_custom_activities_endorsed = sum(custom_activity == 1),
  ) 
  
phase2 <- endorsements %>%
  filter(study_phase == 2) %>%
  mutate(
    survey_type = str_to_lower(survey_type),
    scheduled_or_endorsed = 1
  ) %>%
  pivot_wider(
    id_cols = c(record_id, date, study_phase, activity_id, custom_activity),
    names_from = survey_type,
    values_from = scheduled_or_endorsed
  ) %>%
  mutate(
    .by = c(record_id, date),
    date_has_morning_survey = ifelse(sum(!is.na(morning)) == 0, 0, 1),
    date_has_evening_survey = ifelse(sum(!is.na(evening)) == 0, 0, 1)
  ) %>%
  mutate(
    morning = ifelse(date_has_morning_survey == 1 & is.na(morning), 0, morning),
    evening = ifelse(date_has_evening_survey == 1 & is.na(evening), 0, evening)
  ) %>%
  select(
    record_id, date, study_phase, date_has_morning_survey, date_has_evening_survey, 
    activity_id, custom_activity, activity_scheduled_in_morning = morning, 
    activity_completed_in_evening = evening
  ) %>%
  summarize(
    .by = c(record_id, date, study_phase, date_has_morning_survey, date_has_evening_survey),
    # all activities
    n_activities_scheduled_in_morning = sum(activity_scheduled_in_morning, na.rm = TRUE),
    n_activities_completed_in_evening = sum(activity_completed_in_evening, na.rm = TRUE),
    n_scheduled_activities_completed_in_evening = sum(activity_scheduled_in_morning == 1 & activity_completed_in_evening == 1),
    prop_scheduled_activities_completed_in_evening = n_scheduled_activities_completed_in_evening/n_activities_scheduled_in_morning,
    # custom_activities
    n_custom_activities_scheduled_in_morning = sum(custom_activity == 1 & activity_scheduled_in_morning == 1),
    n_custom_activities_completed_in_evening = sum(custom_activity == 1 & activity_completed_in_evening == 1),
    n_custom_scheduled_activities_completed_in_evening = sum(custom_activity == 1 & activity_scheduled_in_morning == 1 & activity_completed_in_evening == 1),
    prop_custom_scheduled_activities_completed_in_evening = n_custom_scheduled_activities_completed_in_evening/n_custom_activities_scheduled_in_morning
  ) 


#### combine output ----

distinct_activities <- overall_distinct_activities %>%
  mutate(survey_type = str_to_lower(survey_type)) %>%
  pivot_wider(
    id_cols = record_id, 
    names_from = c(study_phase, survey_type),
    values_from = n_distinct_activities,
    names_glue = "{.value}_{study_phase}_{survey_type}"
  )

daily_activities <- phase1_daily %>%
  mutate(date_has_daily_survey = 1) %>%
  bind_rows(phase2) %>%
  select(
    record_id, date, study_phase,
    date_has_daily_survey, date_has_morning_survey, date_has_evening_survey,
    everything(),
    -c(survey_type)
  ) %>%
  arrange(record_id, date) 

daily_activities_averaged <- daily_activities %>%
  pivot_wider(
    id_cols = c(record_id, date),
    names_from = study_phase,
    values_from = -c(record_id, date, study_phase),
    names_glue = "phase{study_phase}_{.value}"
  ) %>%
  select_if(~ !all(is.na(.))) %>%
  summarize(
    .by = c(record_id),
    across(
      .cols = where(is.numeric) & !contains("_date_"),
      .fns = list(
        mean = function(x) mean(x, na.rm = TRUE),
        sd = function(x) sd(x, na.rm = TRUE),
        min = function(x) min(x, na.rm = TRUE),
        max = function(x) max(x, na.rm = TRUE)
      )
    ),
    across(
      .cols =  where(is.numeric) & contains("_date_"),
      .fns = list(
        count = function(x) sum(x, na.rm = TRUE)
      )
    )
  ) %>%
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = function(x) ifelse(is.infinite(x), NA_real_, x)
    )
  ) %>%
  select_if(~ !all(is.na(.))) 


#### save results ----

write_csv(distinct_activities, here::here(parent_file_path, output_file_path, "balance_aggregated_distinct_activity_endorsements.csv"))
write_csv(daily_activities, here::here(parent_file_path, output_file_path, "balance_aggregated_daily_activity_endorsements.csv"))
write_csv(daily_activities_averaged, here::here(parent_file_path, output_file_path, "balance_average_aggregated_daily_activity_endorsements.csv"))
