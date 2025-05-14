import pandas as pd
import numpy as np

from utils import load_config

def main():
    config = load_config()

    input_file_path = "../../data/raw/"
    input_file_name_promis = config["raw_data"]["phase1"]["promis"]
    input_file_name_scores = config["raw_data"]["phase1"]["scores"]

    output_file_path = "../../data/interim/"
    output_file_name = "balance_outcomes.csv"

    promis = pd.read_csv(input_file_path + input_file_name_promis)
    scores = pd.read_csv(input_file_path + input_file_name_scores)

    outcomes = (
        scores
        .merge(promis, on="record_id", how="outer")
        .melt(id_vars=["record_id"])
        .assign(
            timepoint_name = lambda x: 
                x["variable"].str.split("_").str[-1]
        )
        .assign(
            timepoint_number = lambda x: 
                x["timepoint_name"]
                .replace(
                    {"bl":"1", "mp":"2", "eos":"3"}
                )
                .astype(int)
        )
        .assign(
            variable = lambda x:
                x["variable"].str.replace("_bl$|_mp$|_eos$", "", regex=True)
        )
        .pivot(
            index=["record_id", "timepoint_name", "timepoint_number"],
            columns="variable",
            values="value"
        )
        .reset_index(drop=False)
        .rename_axis(None, axis=1)
        .sort_values(["record_id", "timepoint_number"])
    )

    outcomes.to_csv(output_file_path + output_file_name, index=False)

if __name__ == "__main__":
    main()