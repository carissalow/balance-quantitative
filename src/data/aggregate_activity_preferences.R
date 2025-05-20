# Aggregate participants phase 1 and 2 activity preferences

library(tidyverse)

#### settings ----

parent_file_path <- "balance-quantitative"
input_file_path <- "data/interim"
input_file_name <- "balance_activity_preferences.csv"
output_file_path <- "data/processed"
output_file_name <- "balance_aggregated_activity_preferences.csv"


#### aggregate data ---- 

preferences <- read_csv(here::here(parent_file_path, input_file_path, input_file_name))

preferences_aggregated <- preferences %>%
  mutate(selected = 1) %>%
  pivot_wider(
    id_cols = c(record_id, activity_id, custom_activity),
    names_from = study_phase,
    values_from = selected,
    names_glue = "{.value}_in_phase{study_phase}"
  ) %>%
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = function(x) ifelse(is.na(x), 0, x)
    )
  ) %>%
  summarize(
    .by = c(record_id),

    # count of activities overall and by phase
    n_activities = n(),
    n_custom_activities = sum(custom_activity == 1),
    n_default_activities = sum(custom_activity == 0),
    
    n_activities_phase1 = sum(selected_in_phase1 == 1),
    n_custom_activities_phase1 = sum(selected_in_phase1 == 1 & custom_activity == 1),
    n_default_activities_phase1 = sum(selected_in_phase1 == 1 & custom_activity == 0),
    
    n_activities_phase2 = sum(selected_in_phase2 == 1),
    n_custom_activities_phase2 = sum(selected_in_phase2 == 1 & custom_activity == 1),
    n_default_activities_phase2 = sum(selected_in_phase2 == 1 & custom_activity == 0),

    # count of intersect/diff phase 1 and 2 (custom) activities
    n_activities_in_phase1_and_phase2 = sum(selected_in_phase1 == 1 & selected_in_phase2 == 1),
    n_activities_in_phase1_not_phase2 = sum(selected_in_phase1 == 1 & selected_in_phase2 == 0),
    n_activities_in_phase2_not_phase1 = sum(selected_in_phase1 == 0 & selected_in_phase2 == 1),
    
    n_custom_activities_in_phase1_and_phase2 = sum(custom_activity == 1 & selected_in_phase1 == 1 & selected_in_phase2 == 1),
    n_custom_activities_in_phase1_not_phase2 = sum(custom_activity == 1 & selected_in_phase1 == 1 & selected_in_phase2 == 0),
    n_custom_activities_in_phase2_not_phase1 = sum(custom_activity == 1 & selected_in_phase1 == 0 & selected_in_phase2 == 1),
    
    # proportion of intersect/diff (custom) activities overall and per phase
    prop_activities_in_phase1_and_phase2 = n_activities_in_phase1_and_phase2/n_activities,
    prop_activities_in_phase1_not_phase2 = n_activities_in_phase1_not_phase2/n_activities,
    prop_activities_in_phase2_not_phase1 = n_activities_in_phase2_not_phase1/n_activities,
    
    prop_custom_activities_in_phase1_and_phase2 = n_custom_activities_in_phase1_and_phase2/n_custom_activities,
    prop_custom_activities_in_phase1_not_phase2 = n_custom_activities_in_phase1_not_phase2/n_custom_activities,
    prop_custom_activities_in_phase2_not_phase1 = n_custom_activities_in_phase2_not_phase1/n_custom_activities,
    
    prop_phase1_activities_in_phase2 = n_activities_in_phase1_and_phase2/n_activities_phase1,
    prop_phase1_activities_not_in_phase2 = n_activities_in_phase1_not_phase2/n_activities_phase1,
    prop_custom_phase1_activities_in_phase2 = n_custom_activities_in_phase1_and_phase2/n_custom_activities_phase1,
    prop_custom_phase1_activities_not_in_phase2 = n_custom_activities_in_phase1_not_phase2/n_custom_activities_phase1,
    
    prop_phase2_activities_in_phase1 = n_activities_in_phase1_and_phase2/n_activities_phase2,
    prop_phase2_activities_not_in_phase1 = n_activities_in_phase2_not_phase1/n_activities_phase2,
    prop_custom_phase2_activities_in_phase1 = n_custom_activities_in_phase1_and_phase2/n_custom_activities_phase2,
    prop_custom_phase2_activities_not_in_phase1 = n_custom_activities_in_phase2_not_phase1/n_custom_activities_phase2
  ) 


#### save output ---- 

write_csv(preferences_aggregated, file = here::here(parent_file_path, output_file_path, output_file_name))

