import pandas as pd
import numpy as np

from utils import load_config, flatten_columns

def main():
    input_file_path_dates = "../../data/interim/"
    input_file_path_features = "../../rapids/data/processed/features/all_participants/"
    output_file_path = "../../data/interim/"

    id_cols = ["pid", "date", "local_segment_label", "local_segment_start_datetime", "local_segment_end_datetime"]

    config = load_config()
    params = config["fitbit"]

    feature_cols = params["features"]
    drop_epochs = params["segments"]["drop_epochs"]
    drop_low_data_yield = params["segments"]["drop_low_data_yield"]
    data_yield_feature = params["data_yield"]["feature"]
    data_yield_threshold = params["data_yield"]["threshold"]
    if isinstance(data_yield_threshold, str):
        data_yield_threshold = eval(data_yield_threshold)

    study_dates = pd.read_csv(input_file_path_dates + "balance_study_dates.csv", parse_dates=["date"])
    fitbit_features = pd.read_csv(input_file_path_features + "all_sensor_features.csv", parse_dates = ["local_segment_start_datetime"]) 

    study_dates["date"] = study_dates["date"].dt.date
    study_dates["pid"] = "p" +  study_dates["record_id"].astype(str)

    fitbit_features["date"] = fitbit_features["local_segment_start_datetime"].dt.date
    fitbit_features = fitbit_features.filter(id_cols + feature_cols, axis=1)

    if drop_low_data_yield:
        data_yield_feature_col = f"fitbit_data_yield_rapids_{data_yield_feature}"
        fitbit_features = fitbit_features.query("@data_yield_feature_col >= @low_data_yield_threshold")

    if drop_epochs:
        fitbit_features = fitbit_features.query("local_segment_label == 'daily'")
        fitbit_features = fitbit_features.drop("local_segment_label", axis=1)
    else:
        fitbit_features = (
            fitbit_features
            .pivot(index = ["pid", "date"], columns = "local_segment_label")
            .reset_index(drop=False)
            .pipe(flatten_columns)
        )

    fitbit_features_clean = study_dates.merge(fitbit_features, how="left", on=["pid", "date"])
    fitbit_features_clean.to_csv(output_file_path + "balance_fitbit_features.csv", index=False)

if __name__ == "__main__":
    main()