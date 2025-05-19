# Compute Phase 1 participant average goodness during study phases 1 & 2

library(tidyverse)

#### settings ----

parent_file_path <- "balance-quantitative"
input_file_path <- "data/interim/"
input_file_name <- "balance_surveys.csv"
output_file_path <- "data/processed/"
output_file_name <- "balance_average_goodness.csv"

surveys <- read_csv(here::here(parent_file_path, input_file_path, input_file_name))


#### compute average goodness per participant and study phase ----

average_goodness <- surveys %>%
  select(record_id, date, study_phase, survey_type, goodness_score) %>%
  filter(survey_type != "MORNING") %>%
  summarize(
    .by = c(record_id, study_phase),
    across(
      .cols = starts_with("goodness"),
      .fns = list(
        n = function(x) sum(!is.na(x)),
        mean = function(x) mean(x, na.rm = TRUE),
        sd = function(x) sd(x, na.rm = TRUE),
        min = function(x) min(x, na.rm = TRUE),
        max = function(x) max(x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )


#### save output ----

write_csv(average_goodness, file = here::here(parent_file_path, output_file_path, output_file_name))
