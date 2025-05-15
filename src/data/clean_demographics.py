import pandas as pd
import numpy as np
import datetime
import yaml
import re

from utils import *

def harmonize_col_names(df):
    old_to_new = {
        "diganosed": "diagnosed",
        "type_participant": "category_participant",
        "^dhl_": "dhls_",
        "^dhls": "dhls_",
        "^dhls__": "dhls_",
        "_bl$": ""
    }
    for old, new in old_to_new.items():
        df = df.rename(lambda x: re.sub(old, new, x), axis=1)
    return df

def recode_categorical_cols(df, mappings):
    for col, mapping in mappings.items():
        if col in df.columns:
            df[col] = df[col].replace(mapping)
    return df

def rename_multiple_choice_cols(df, mappings):
    for col in mappings.keys():
        name_mapper = {f"{col}___{k}": f"{col}_{v}" for k, v in mappings[col].items()}
        df = df.rename(name_mapper, axis=1)
    return df

def recode_multiple_choice_cols(df, mappings):
    for prefix in mappings.keys():
        cols_to_recode = get_matching_cols(df, prefix=prefix, excluded_string="combined")
        for col in cols_to_recode:
            df[col] = df[col].replace({0: "No", 1: "Yes"})
    return df

def combine_multiple_choice_cols(df, mappings, sort=False, append_other=True):
    for prefix in mappings.keys():
        cols_to_combine = get_matching_cols(df, prefix=prefix, excluded_string="combined")
        if len(cols_to_combine) > 0:
            df[f"{prefix}_combined"] = df[cols_to_combine].apply(
                lambda row: pretty_join(
                    df[cols_to_combine].columns[(row == "Yes") | (row == 1)].tolist(),
                    sort=sort, prefix=prefix, append_other=append_other
                ),
                axis=1
            )
    
    return df

def sum_multiple_choice_cols(df, mappings):
    for prefix in mappings.keys():
        cols_to_combine = get_matching_cols(df, prefix=prefix, excluded_string="combined")
        if (df[cols_to_combine] == "Yes").any().any():
            df[f"{prefix}_combined_n_choices"] = df[cols_to_combine].apply(lambda x: np.where(x == "Yes", 1, 0)).sum(axis=1)
        else:
            df[f"{prefix}_combined_n_choices"] = df[cols_to_combine].sum(axis=1)
    return df

def construct_date(df, suffixes):
    for suffix in suffixes:
        year_col = "year_" + suffix
        month_col = "month_" + suffix
        day_col = "day_" + suffix

        component_cols = [year_col, month_col, day_col]

        date_str = df[component_cols].apply(lambda x: "-".join(x.astype("Int64").astype(str)), axis=1)
        date_str = date_str.apply(lambda x: np.where("NA" in x, None, x))
        df["date_" + suffix] = pd.to_datetime(date_str, errors="coerce")
    
    return df

def compute_time_interval(df, start_col, end_col, units):
    supported_units = ["days", "months", "years"]
    if not units in supported_units:
        raise ValueError(f"`units` must be one of: {', '.join(supported_units)}")

    time_since_col = f"{units}_since_{start_col.replace('date_', '')}_at_{end_col.replace('date_', '')}"
    time_since = (
        df[[start_col, end_col]]
        .apply(lambda x: x.dt.to_period(units[0].capitalize()), axis=1)
        .apply(lambda x: (x[end_col] - x[start_col]), axis=1)
    )

    # can only convert non-NaT values from periods to integer
    not_NaT_mask = time_since.notnull()
    time_since.loc[not_NaT_mask] = time_since.loc[not_NaT_mask].apply(lambda x: x.n)
    with pd.option_context("future.no_silent_downcasting", True):
        time_since = time_since.fillna(np.nan).infer_objects(copy=False)

    # treat negative values (indicating event is in the future as missing)
    time_since = np.where(time_since < 0, np.nan, time_since)
    
    df[time_since_col] = time_since
    return df

def main():
    config = load_config()

    categorical_mappings = config["demographics"]["categorical_mappings"]
    multiple_choice_mappings = config["demographics"]["multiple_choice_mappings"]
    dates_to_construct = config["demographics"]["dates_to_construct"]
    intervals_to_compute = config["demographics"]["intervals_to_compute"]

    input_file_path = "../../data/raw/"
    output_file_path = "../../data/interim/"
    output_file_name = "balance_demographics.csv"

    demos = pd.DataFrame()

    cohorts = list(config["raw_data"].keys())
    for cohort in cohorts:
        input_file_name = config["raw_data"][cohort]["demographics"]
        cohort_demos = pd.read_csv(input_file_path + input_file_name, dtype={"record_id":"str"})
        cohort_demos = harmonize_col_names(cohort_demos)
    
        original_cols = cohort_demos.columns.tolist()
    
        cohort_demos["cohort"] = cohort
        cohort_demos = cohort_demos[["cohort"] + original_cols]

        demos = pd.concat([demos, cohort_demos], axis=0)

    demos_clean = (
        demos
        .pipe(parse_dates)
        .pipe(recode_categorical_cols, categorical_mappings)
        .pipe(rename_multiple_choice_cols, multiple_choice_mappings)
        .pipe(combine_multiple_choice_cols, multiple_choice_mappings)
        .pipe(sum_multiple_choice_cols, multiple_choice_mappings)
        .pipe(recode_multiple_choice_cols, multiple_choice_mappings)
        .pipe(construct_date, dates_to_construct)
    )

    for interval in intervals_to_compute.keys():
        params = intervals_to_compute[interval]
        demos_clean = demos_clean.pipe(
            compute_time_interval,
            start_col = params["start"],
            end_col = params["end"],
            units = params["units"]
        )

    demos_clean.to_csv(output_file_path + output_file_name, index=False)

if __name__ == "__main__":
    main()