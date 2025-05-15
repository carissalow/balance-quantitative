library(tidyverse)
library(gtsummary)
library(gt)

#### setttings ----

google_font_name <- "Roboto"

input_file_date <- "20250515"
input_file_path <- "output/results"
input_file_name <- glue::glue("repeated_measures_anovas_{input_file_date}.csv")

output_file_path <- "output/tables/"
output_file_name <- "balance_anova_results"


#### format data ----

results <- read_csv(here::here("balance-quantitative", input_file_path, input_file_name))

results <- results %>%
  mutate(
    outcome_name = case_when(
      grepl("^cesd", outcome) ~ "CESD Score",
      grepl("^peat", outcome) ~ "PEAT Score",
      grepl("^sip", outcome) ~ "SIP Score",
      grepl("promis_anx", outcome) ~ "Anxiety",
      grepl("promis_cf", outcome) ~ "Cognitive function",
      grepl("promis_dep", outcome) ~ "Depression",
      grepl("promis_fat", outcome) ~ "Fatigue",
      grepl("promis_pain", outcome) ~ "Pain interference",
      grepl("promis_pf", outcome) ~ "Physical function",
      grepl("promis_sleep", outcome) ~ "Sleep disturbance",
      grepl("promis_social", outcome) ~ "Social roles and activities"
    ),
    outcome_name = ifelse(grepl("^promis", outcome), paste0("PROMIS ", outcome_name), outcome_name)
  ) 
  
anova_results_to_table <- results %>% 
  dplyr::select(
    outcome_name, n, anova_DFn, anova_DFd, anova_F, anova_p, anova_ges, 
    sphericity_W, sphericity_p, matches("correction_.*GG.*")
  ) %>%
  distinct() %>%
  mutate(
    correction_applied = ifelse(sphericity_p < 0.05, 1, 0),
    df = ifelse(correction_applied == 1, correction_DF_GG, glue::glue("{anova_DFn}, {anova_DFd}")),
    p = ifelse(correction_applied == 1, correction_p_GG, anova_p),
    f = round(anova_F, 2),
    p_str = case_when(
      round(p, 3) < 0.001 ~ "<0.001",
      round(p, 3) > 0.999 ~ ">0.999",
      TRUE ~ as.character(round(p, 3))
    ),
    ges = case_when(
      round(anova_ges, 3) < 0.001 ~ "<0.001",
      TRUE ~ as.character(round(anova_ges, 3))
    ),
    f_df = glue::glue("F({df})={round(anova_F, 2)}"),
    f_df_p = glue::glue("{f_df}, p{ifelse(grepl('<|>', p_str), '', '=')}{p_str}"),
  ) %>%
  dplyr::select(outcome_name, correction_applied, df, f, p_str, ges) %>%
  rename_all(function(x) ifelse(x != "outcome_name", paste0("anova_", x), x))

ttest_results_to_table <- results %>%
  mutate(
    # reverse comparison groups 
    across(
      .cols = c(ttest_estimate, ttest_statistic),
      .fns = function(x) -1 * x
    ),
    comparison = glue::glue("t{ttest_group1} to t{ttest_group2}"),
    across(
      .cols = c(ttest_estimate, ttest_statistic),
      .fns = function(x) round(x, 2)
    ),
    ttest_p_str = case_when(
      round(ttest_p, 3) < 0.001 ~ "<0.001",
      round(ttest_p, 3) > 0.999 ~ ">0.999",
      TRUE ~ as.character(round(ttest_p, 3))
    )
  ) %>%
  dplyr::select(outcome_name, comparison, ttest_estimate, ttest_df, ttest_statistic, ttest_p_str) %>%
  pivot_wider(
    id_cols = outcome_name,
    names_from = comparison,
    values_from = starts_with("ttest"),
    names_sort = FALSE
  ) %>%
  dplyr::select(
    outcome_name, ends_with("t1 to t2"), ends_with("t1 to t3"), ends_with("t2 to t3")
  )

results_to_table <- anova_results_to_table %>%
  full_join(ttest_results_to_table, by = "outcome_name")

insert_row_location <- min(grep("PROMIS", results_to_table$outcome_name))
insert_row <- tribble(~outcome_name, "PROMIS Subscale t-score")

results_to_table <- bind_rows(
  results_to_table[1:(insert_row_location-1), ],
  insert_row,
  results_to_table[insert_row_location:nrow(results_to_table), ]
)

results_to_table <- results_to_table %>%
  mutate(
    outcome_name = case_when(
      grepl("PROMIS", outcome_name) & !grepl("t-score", outcome_name) ~ gsub("PROMIS ", "\t", outcome_name),
      TRUE ~ outcome_name
    ),
    across(
      .cols = -c(outcome_name, anova_correction_applied),
      .fns = function(x) ifelse(is.na(x), "", as.character(x))
    )
  )


#### create the table ----

indent_rows <- grep("^\t", results_to_table$outcome_name)
bolded_rows <- which(!grepl("^\t", results_to_table$outcome_name))
f_df_footnote_rows <- which(results_to_table$anova_correction_applied == 1)

f_df <- unique(results_to_table[which(results_to_table$anova_correction_applied == 0), ]$anova_df)
f_df_label <- paste0("{{F_", gsub(" .*$", "", f_df), " _", gsub("^.*, ", "", f_df), "}}")

t_df <- unique(results_to_table[which(results_to_table$`ttest_df_t1 to t2` != ""), ]$`ttest_df_t1 to t2`)
t_df_label <- paste0("{{t_", t_df, "}}")

anova_results_table <- results_to_table %>%
  dplyr::select(
    -contains("df"), 
    -contains("correction")
  ) %>%
  gt(
    rowname_col = "outcome_name"
  ) %>%
  cols_label(
    outcome_name ~ "Outcome measure",
    anova_f ~ f_df_label,
    anova_p_str ~ "p-value",
    anova_ges ~ "{{:eta:[_g^2]}}",
    contains("ttest_estimate") ~ "MD",
    contains("ttest_statistic") ~ t_df_label,
    contains("ttest_p") ~ "p-value"
  ) %>%
  tab_spanner(
    label = "{{Baseline to Midpoint}}", 
    columns = ends_with("t1 to t2")
  ) %>%
  tab_spanner(
    label = "{{Baseline to End of study}}", 
    columns = ends_with("t1 to t3")
  ) %>%
  tab_spanner(
    label = "{{Midpoint to End of study}}", 
    columns = ends_with("t2 to t3")
  ) %>%
  tab_stubhead(
    label = md(paste0("**Outcome measure**, N = ", max(results$n)))
  ) %>%
  tab_stub_indent(
    rows = indent_rows,
    indent = 4
  ) %>%
  tab_style(
    style = cell_text(
      weight = "bold",
    ),
    locations = list(
      cells_column_labels(),
      cells_column_spanners(),
      cells_stub(
        rows = bolded_rows
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
    columns = contains("_p_"),
    fn = function(x) as.numeric(x) < 0.05
  ) %>%
  tab_footnote(
    footnote = "N = 1 participant missing baseline was excluded from analysis",
    locations = cells_stubhead()
  ) %>%
  tab_footnote(
    footnote = "Greenhouse-Geisser correction applied",
    locations = cells_stub(
      rows = f_df_footnote_rows
    )
  )


#### save output -----

# Word doc for paper
anova_results_table %>%
  gtsave(
    filename = here::here("balance-quantitative", output_file_path, glue::glue("{output_file_name}.docx"))
  )

# html for report
anova_results_table %>%
  opt_table_font(
    font = list(
      gt::google_font(name = google_font_name)
    )
  ) %>%
  cols_width(
    1 ~ px(250),
    everything() ~ 70
  ) %>%
  gtsave(
    filename = here::here("balance-quantitative", output_file_path, glue::glue("{output_file_name}.html"))
  )