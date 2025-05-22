# Table results of paired t-tests

library(tidyverse)
library(gtsummary)
library(gt)

#### settings ----

google_font_name <- "Roboto"

results_date <- "20250522"
parent_file_path <- "balance-quantitative"
input_file_path <- "output/results"
input_file_name <- glue::glue("paired_ttests_{results_date}.csv")

output_file_path <- "output/tables/"
output_file_name <- "balance_ttest_results"

#### format data ----

results <- read_csv(here::here(parent_file_path, input_file_path, input_file_name))

ttest_results_to_table <- results %>%
  select(
    outcome, n, starts_with("data"), mean_difference, t, df, p_value
  ) %>%
  rowwise() %>%
  mutate(
    across(
      .cols = c(data_x, data_y),
      .fns = function(s) s %>% 
        gsub(paste0("^", outcome, "_"), "", .) %>% 
        gsub("phase", "Phase ", .)
    )
  ) %>%
  mutate(
    outcome_name = case_when(
      grepl("goodness", outcome) ~ "Goodness score",
      grepl("sumsteps", outcome) ~ "Step count"
    ),
    outcome_type = case_when(
      grepl("mean$", outcome) ~ "mean",
      grepl("n$", outcome) ~ "n"
    ),
    outcome_str = paste0(outcome_name, ", ", outcome_type),
    t = round(t, 2),
    p_str = case_when(
      round(p_value, 3) < 0.001 ~ "<0.001",
      round(p_value, 3) > 0.999 ~ ">0.999",
      TRUE ~ as.character(round(p_value, 3))
    ),
    across(
      .cols = matches("mean_|_mean|_sd|_min|_max"),
      .fns = function(x) round(x, 2)
    ),
    data_x_range = glue::glue("{data_x_min}, {data_x_max}"),
    data_y_range = glue::glue("{data_y_min}, {data_y_max}")
  )

#### create the table ----

data_y_label <- unique(ttest_results_to_table$data_y)[[1]]
data_x_label <- unique(ttest_results_to_table$data_x)[[1]]

ttest_results_table <- ttest_results_to_table %>%
  select(
    outcome_str, n, data_y_mean, data_y_sd, data_y_range, data_x_mean, data_x_sd,
    data_x_range, mean_difference, t, df, p_str
  ) %>%
  gt(
    rowname_col = "outcome_str"
  ) %>%
  cols_label(
    n ~ "n",
    mean_difference ~ "MD",
    p_str ~ "p-value",
    ends_with("_mean") ~ "Mean",
    ends_with("_sd") ~ "SD",
    ends_with("_range") ~ "Range"
  ) %>%
  cols_align(
    align = "right",
    columns = ends_with("range")
  ) %>%
  tab_spanner(
    label = data_y_label, 
    columns = starts_with("data_y")
  ) %>%
  tab_spanner(
    label = data_x_label, 
    columns = starts_with("data_x")
  ) %>%
  tab_stubhead(
    label = md("**Outcome measure**")
  ) %>%
  tab_style(
    style = cell_text(
      weight = "bold",
    ),
    locations = list(
      cells_column_labels(),
      cells_column_spanners(),
      cells_stub(
        rows = grepl("mean", outcome_str)
      )
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = NULL
    ),
    locations = cells_stub()
  ) %>%
  tab_style_body(
    style = cell_text(
      weight = "bold",
      style = "italic"
    ),
    columns = c("p_str"),
    fn = function(x) as.numeric(x) < 0.05
  ) %>%
  tab_footnote(
    #n = 2 had 0 valid days during both phases; n = 1 had 0 valid days during Phase 2"
    footnote = "N = 3 participants with 0 days of sufficient Fitbit data during Phase 1 and/or Phase 2 were excluded from analysis", 
    locations = cells_stub(
      rows = grepl("Step count, mean", outcome_str)
    )
  )


#### save output -----

# Word doc for paper
ttest_results_table %>%
  gtsave(
    filename = here::here(parent_file_path, output_file_path, glue::glue("{output_file_name}.docx"))
  )

# html for report
ttest_results_table %>%
  opt_table_font(
    font = list(
      gt::google_font(name = google_font_name)
    )
  ) %>%
  cols_width(
    1 ~ px(200),
    ends_with("range") ~ 150,
    everything() ~ 75
  ) %>%
  gtsave(
    filename = here::here(parent_file_path, output_file_path, glue::glue("{output_file_name}.html"))
  )