import pandas as pd
import numpy as np

from utils import load_credentials, connect_to_database

def main():
    output_file_path = "../../data/interim/"
    output_file_name = "balance_devices.csv"

    device_query = f"""
    select 
        fitbits.id as device_id,
        fitbits.id as fitbit_id,
        '' as empatica_id,
        concat('p', participants.label) as pid,
        '' as platform,
        participants.label,
        participants.start_date,
        participants.end_date
    from fitbits
    join participants on fitbits.participant_id = participants.id
    where label like '1%'
    order by label;
    """

    credentials = load_credentials(group="pittbit")
    con = connect_to_database(credentials=credentials)

    devices = pd.read_sql(sql=device_query, con=con)
    devices.to_csv(output_file_path + output_file_name, index=False)

    con.close()

if __name__ == "__main__":
    main()