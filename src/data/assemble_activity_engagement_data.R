# Assemble activity engagement (selection, phase 1 endorsement, and phase 2
# planning and completion) data

library(tidyverse)


#### helper functions ----

format_activity_selections <- function(df, phase) {
  df <- df %>%
    dplyr::select(-ends_with("date")) %>%
    rowwise() %>%
    mutate(
      activity_type = ifelse(
        grepl(glue::glue("^{pId}"), activityId),
        "Custom",
        "Default"
      ),
      !!glue::glue("selected_phase{phase}") := 1
    ) %>%
    ungroup() %>%
    rename(
      record_id = pId,
      activity_id = activityId
    ) 
  return(df)
}


#### settings ----

parent_file_path <- "balance-quantitative"
input_file_path <- "data/interim"
output_file_path <- "data/processed"
output_file_name <- "balance_activity_engagement.csv"


#### activity selections ----

selected_phase1 <- read_csv(here::here(parent_file_path, input_file_path, "balance_phase1_selected_activities.csv"))
selected_phase2 <- read_csv(here::here(parent_file_path, input_file_path, "balance_phase2_selected_activities.csv"))

selected <- format_activity_selections(selected_phase1, 1) %>%
  full_join(
    format_activity_selections(selected_phase2, 2),
    by = c("record_id", "activity_id", "activity_name", "activity_type")
  ) %>%
  replace_na(
    list(
      selected_phase1 = 0,
      selected_phase2 = 0
    )
  )

activity_selection <- selected %>%
  summarize(
    .by = c(record_id, activity_type),
    phase1 = sum(selected_phase1),
    phase2 = sum(selected_phase2),
    phase1_and_phase2 = sum(selected_phase1 == 1 & selected_phase2 == 1),
    phase1_not_phase2 = sum(selected_phase1 == 1 & selected_phase2 == 0),
    phase2_not_phase1 = sum(selected_phase1 == 0 & selected_phase2 == 1)
  ) %>%
  bind_rows(
    selected %>%
      summarize(
        .by = record_id,
        phase1 = sum(selected_phase1),
        phase2 = sum(selected_phase2),
        phase1_and_phase2 = sum(selected_phase1 == 1 & selected_phase2 == 1),
        phase1_not_phase2 = sum(selected_phase1 == 1 & selected_phase2 == 0),
        phase2_not_phase1 = sum(selected_phase1 == 0 & selected_phase2 == 1)
      ) %>%
      mutate(activity_type = "All")
  ) %>%
  arrange(record_id, activity_type) %>%
  pivot_wider(
    names_from = activity_type, 
    values_from = starts_with("phase"),
    names_glue = "selected_{.value}_{tolower(activity_type)}"
  )


#### phase 1 activity endorsements ----

endorsed <- read_csv(here::here(parent_file_path, input_file_path, "balance_phase1_endorsed_activities.csv"))

endorsed <- endorsed %>%
  rename(
    record_id = pId,
    activity_id = activityId
  )

activity_endorsement_participant_level <- endorsed %>%
  summarize(
    .by = record_id,
    daily_surveys = length(unique(survey_id_daily)),
    total_endorsed = sum(endorsed),
    total_rated = sum(!is.na(activity_rating)),
    unique_endorsed = length(unique(activity_id)),
    unique_rated = length(unique(activity_id[!is.na(activity_rating)])),
    across(
      .cols = c("activity_rating"),
      .fns = list(
        "mean" = function(x) mean(x, na.rm = TRUE),
        "sd" = function(x) sd(x, na.rm = TRUE),
        "min" = function(x) min(x, na.rm = TRUE),
        "max" = function(x) max(x, na.rm = TRUE)
      )
    )
  ) 

activity_endorsement_day_level <- endorsed %>%
  summarize(
    .by = c(record_id, date),
    total_endorsed = sum(endorsed),
    total_rated = sum(!is.na(activity_rating)),
    across(
      .cols = c("activity_rating"),
      .fns = list(
        "mean" = function(x) mean(x, na.rm = TRUE),
        "min" = function(x) min(x, na.rm = TRUE),
        "max" = function(x) max(x, na.rm = TRUE)
      )
    )
  ) %>%
  mutate(
    endorsed_activity_rating_rate = total_rated/total_endorsed,
  ) %>%
  summarize(
    .by = record_id,
    across(
      .cols = where(is.numeric),
      .fns = list(
        "mean" = function(x) mean(x, na.rm = TRUE),
        "sd" = function(x) sd(x, na.rm = TRUE),
        "min" = function(x) min(x, na.rm = TRUE),
        "max" = function(x) max(x, na.rm = TRUE)
      )
    )
  )

activity_endorsement <- activity_endorsement_participant_level %>%
  full_join(activity_endorsement_day_level, by = c("record_id")) %>%
  rename_all(
    function(x) ifelse(!grepl("record_id", x), paste0("phase1_", x), x) 
  )


#### phase 2 activity planning and completion ----

planned_morning <- read_csv(here::here(parent_file_path, input_file_path, "balance_phase2_planned_activities.csv"))
completed_evening <- read_csv(here::here(parent_file_path, input_file_path, "balance_phase2_completed_activities.csv"))

planned_and_completed <- planned_morning %>%
  full_join(
    completed_evening, 
    by = c("pId", "date", "activityId")
  ) %>%
  full_join(
    planned_morning %>%
      select(pId, date) %>%
      distinct() %>%
      mutate(has_morning = 1),
    by = c("pId", "date")
  ) %>%
  full_join(
    completed_evening %>%
      select(pId, date) %>%
      distinct() %>%
      mutate(has_evening = 1),
    by = c("pId", "date")
  ) %>%
  replace_na(
    list(
      has_morning = 0,
      has_evening = 0
    )
  ) %>%
  select(
    record_id = pId, 
    date,
    has_morning,
    has_evening,
    activity_id = activityId,
    planned,
    completed 
  ) %>%
  arrange(record_id, date, activity_id) %>%
  mutate(
    planned = case_when(
      has_morning == 1 & is.na(planned) ~ 0,
      TRUE ~ planned
    ),
    completed = case_when(
      has_evening == 1 & is.na(completed) ~ 0,
      TRUE ~ completed
    )
  )

activity_completion_participant_level <- planned_and_completed %>%
  summarize(
    .by = record_id,
    # data quantity
    morning_surveys = length(unique(date[has_morning == 1])),
    evening_surveys = length(unique(date[has_evening == 1])),
    morning_and_evening_surveys = length(unique(date[has_morning == 1 & has_evening == 1])),
    # total
    total_planned = sum(planned == 1, na.rm = TRUE),
    total_completed = sum(completed == 1, na.rm = TRUE),
    total_planned_and_completed = sum(planned == 1 & completed == 1, na.rm = TRUE),
    total_planned_not_completed = sum(planned == 1 & completed == 0, na.rm = TRUE),
    total_completed_not_planned = sum(planned == 0 & completed == 1, na.rm = TRUE),
    total_not_planned_not_completed = sum(planned == 0 & completed == 0, na.rm = TRUE),
    # unique
    unique_planned = length(na.omit(unique(activity_id[planned == 1]))),
    unique_completed = length(na.omit(unique(activity_id[completed == 1]))),
    unique_planned_and_completed = length(na.omit(unique(activity_id[planned == 1 & completed == 1]))),
    unique_planned_not_completed = length(na.omit(unique(activity_id[planned == 1 & completed == 0]))),
    unique_completed_not_planned = length(na.omit(unique(activity_id[planned == 0 & completed == 1]))),
    # list of unique
    unique_planned_list = paste0(sort(na.omit(unique(activity_id[planned == 1]))), collapse=", "),
    unique_completed_list = paste0(sort(na.omit(unique(activity_id[completed == 1]))), collapse=", "),
    unique_planned_and_completed_list = paste0(sort(na.omit(unique(activity_id[planned == 1 & completed == 1]))), collapse=", "),
    unique_planned_not_completed_list = paste0(sort(na.omit(unique(activity_id[planned == 1 & completed == 0]))), collapse=", "),
    unique_completed_not_planned_list = paste0(sort(na.omit(unique(activity_id[planned == 0 & completed == 1]))), collapse=", ")
  ) 

activity_completion_day_level <- planned_and_completed %>%
  summarize(
    .by = c(record_id, date, has_morning, has_evening),
    total_planned = sum(planned == 1, na.rm = TRUE),
    total_completed = sum(completed == 1, na.rm = TRUE),
    total_planned_and_completed = sum(planned == 1 & completed == 1, na.rm = TRUE)
  ) %>%
  mutate(
    planned_activity_completion_rate = case_when(
      has_morning == 1 & has_evening == 1 ~ total_planned_and_completed / total_planned,
      TRUE ~ NA
    )
  ) %>%
  summarize(
    .by = record_id,
    across(
      .cols = ends_with("rate"),
      .fns = list(
        "n" = function(x) sum(!is.na(x)),
        "mean" = function(x) mean(x, na.rm = TRUE),
        "sd" = function(x) sd(x, na.rm = TRUE),
        "min" = function(x) min(x, na.rm = TRUE),
        "max" = function(x) max(x, na.rm = TRUE)
      )
    )
  )

activity_completion <- activity_completion_participant_level %>%
  full_join(
    activity_completion_day_level,
    by = c("record_id")
  ) %>%
  rename_all(
    function(x) ifelse(!grepl("record_id", x), paste0("phase2_", x), x) 
  )


#### combine and save output ----

activity_engagement <- activity_selection %>%
  full_join(activity_endorsement, by = c("record_id")) %>%
  full_join(activity_completion, by = c("record_id"))

write_csv(activity_engagement, here::here(parent_file_path, output_file_path, output_file_name))
