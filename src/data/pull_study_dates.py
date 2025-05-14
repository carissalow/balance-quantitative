import pandas as pd
import numpy as np

from utils import load_credentials, connect_to_database

def main():
    output_file_path = "../../data/interim/"
    output_file_name = "balance_study_dates.csv"

    credentials = load_credentials("balance")
    con = connect_to_database(credentials)

    study_dates_query = f""" 
    select 
        pId as record_id,
        replace(phaseId, 'PHASE_', '') as phase,
        startDate as phase_start_date,
        date_sub(endDate, interval 1 day) as phase_end_date
    from user_study_phases
    where 
        pId like "1%" and
        phaseId != 'NOT_STARTED' and
        pId != '121'
    ;
    """

    study_dates = pd.read_sql(sql=study_dates_query, con=con)
    con.close()

    study_dates["date_range"] = study_dates.apply(lambda row: pd.date_range(row["phase_start_date"], row["phase_end_date"]), axis=1)

    study_dates = (
        study_dates
        .explode("date_range")
        .reset_index(drop=True)
        .rename(columns={"date_range":"date"})
    )

    study_dates.to_csv(output_file_path + output_file_name, index=False)

if __name__ == "__main__":
    main()