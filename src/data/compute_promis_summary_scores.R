# Compute PROMIS physical and mental health summary scores

# References: 
# https://pmc.ncbi.nlm.nih.gov/articles/PMC5999556/ 
# https://www.healthmeasures.net/images/PROMIS/manuals/Scoring_Manual_Only/PROMIS_Adult_Profile_Scoring_Manual_16Sept2024.pdf (p. 6)

# Steps: 
# 1. Obtain t-scores for all domains except 0-10 pain intensity item  
# 2. Transform t-scores to z-scores: z = (t-50)/10
# 3. Transform 0-10 pain intensity item to z-score: z = (score-2.31)/2.34 
# 4. Compute pain composite: average of pain intensity z and pain interference domain z
# 5. Compute emotional distress composite: average of depression z and anxiety z  
# 6. Multiply z-scores by scoring weights and sum to obtain physical and mental health summary z-scores
# 7. Transform summary z-scores into t-scores: t = (summary * 10) + 50


library(tidyverse)


#### set-up ----

pain_mean <- 2.31 
pain_sd <- 2.34
t_mean <- 50
t_sd <- 10

scoring_coefficients <- tribble(
  ~domain,           ~weight_physical,  ~weight_mental,
  "pf",              0.872,             -0.015,
  "paincomposite",   -0.094,            -0.154,
  "social",          0.113,             0.252,
  "fatigue",         -0.009,            -0.351,
  "emotcomposite",   0.003,             -0.257,
  "sleep",           0.002,             -0.139
)

parent_file_path <- "balance-quantitative"
input_file_path <- "data/raw/"
output_file_path <- "data/interim/"
  
#### pain intensity item ---- 

pain_file <- "BALANCEPhase1StudyDa-PROMISPainIntensity_DATA_2025-06-10_1756.csv"
pain <- read_csv(here::here(parent_file_path, input_file_path, pain_file))

pain_z_scores <- pain %>%
  pivot_longer(
    cols = -c("record_id"),
    values_to = "score"
  ) %>%
  separate(
    name,
    into = c("domain", "timepoint"),
    sep = "_"
  ) %>%
  mutate(z = (score-pain_mean)/pain_sd) %>%
  select(record_id, timepoint, domain, z)


#### all other domain scores ----

promis_file <- "BALANCEPhase1StudyDa-PROMIS_DATA_2025-05-08_1218.csv"
promis <- read_csv(here::here(parent_file_path, input_file_path, promis_file))

promis_z_scores <- promis %>%
  pivot_longer(cols = -c("record_id")) %>%
  separate(
    name, 
    into = c("instrument", "domain", "score_type", "calibration_type", "timepoint"), 
    sep = "_"
  ) %>%
  filter(score_type == "t" & domain != "cf" & calibration_type == "default") %>%
  rename(t = value) %>%
  mutate(z = (t-t_mean)/t_sd) %>%
  select(record_id, timepoint, domain, z)


#### compute summary scores ----

z_scores <- promis_z_scores %>%
  bind_rows(pain_z_scores) %>%
  pivot_wider(
    names_from = domain,
    values_from = z,
    names_glue = "{.name}_z"
  ) %>%
  rowwise() %>%
  mutate(
    paincomposite_z = mean(c(pain_z, global07_z), na.rm = FALSE),
    emotcomposite_z = mean(c(dep_z, anxiety_z), na.rm = FALSE)
  ) %>%
  select(-c(pain_z, global07_z, dep_z, anxiety_z)) %>%
  pivot_longer(
    cols = -c(record_id, timepoint),
    names_to = "domain",
    values_to = "z",
    names_transform = function(x) gsub("_z$", "", x)
  ) %>%
  left_join(
    scoring_coefficients, 
    by = "domain"
  ) 

summary_scores <- z_scores %>%
  summarize(
    .by = c(record_id, timepoint),
    n_component_zscores_na = sum(is.na(z)),
    physical_health_summary_z = sum(z*weight_physical, na.rm = FALSE),
    mental_health_summary_z = sum(z*weight_mental, na.rm = FALSE),
  ) %>%
  mutate(
    across(
      .cols = ends_with("z"),
      .fns = function(x) (x*t_sd)+t_mean,
      .names = "{.col}_t"
    )
  ) %>%
  rename_all(function(x) gsub("_z_t$", "_t", x)) %>%
  rename(timepoint_name = timepoint) %>%
  mutate(
    timepoint_number = case_when(
      timepoint_name == "bl" ~ 1,
      timepoint_name == "mp" ~ 2,
      timepoint_name == "eos" ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(timepoint_number, .after = timepoint_name)


#### save output ----

write_csv(summary_scores, here::here(parent_file_path, output_file_path, "balance_promis_summary_scores.csv"))
