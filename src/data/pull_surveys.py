import pandas as pd
import numpy as np

from utils import load_credentials, connect_to_database

def main():
    output_file_path = "../../data/interim/"
    output_file_name = "balance_surveys.csv"

    survey_query = f""" 
    with 
        phase1_dates as (
            select 
                pId, 
                startDate as phase1_start_date, 
                date_sub(endDate, interval 1 day) as phase1_end_date
            from user_study_phases
            where 
                phaseId = 'PHASE_1' and
                pId like '1%' and
                pId != '121'
        ),
        phase2_dates as (
            select 
                pId, 
                startDate as phase2_start_date, 
                date_sub(endDate, interval 1 day) as phase2_end_date
            from user_study_phases
            where 
                phaseId = 'PHASE_2' and
                pId like '1%' and
                pId != '121'
        ),
        surveys as (
            select 
                pId,
                case 
                    when sId = 'DAILY' then 1
                    when sId in ('MORNING', 'EVENING') then 2
                    else NULL
                end as study_phase,
                sId,
                surveyId,
                date, 
                startTime,
                timeTaken,
                case
                    when sId = 'MORNING' then NULL
                    else goodnessScore
                end as goodness_score,
                case 
                    when note = '' then 0
                    else 1
                end as added_note
            from survey_responses
            where pId like '1%' and pId != '121'
        )
    select 
        surveys.pId as record_id,
        surveyId as survey_id,
        study_phase,
        sId as survey_type,
        date,
        startTime as survey_start_datetime,
        timeTaken as survey_duration,
        goodness_score,
        added_note
    from surveys
    join phase1_dates on surveys.pId = phase1_dates.pId
    join phase2_dates on surveys.pId = phase2_dates.pId
    where 
        (sId = 'DAILY' and date >= phase1_start_date and date <= phase1_end_date) or
        (sId in ('MORNING', 'EVENING') and date >= phase2_start_date and date <= phase2_end_date)
    order by record_id, survey_type, date;
    """

    credentials = load_credentials(group="balance")
    con = connect_to_database(credentials=credentials)

    surveys = pd.read_sql(sql=survey_query, con=con)
    surveys.to_csv(output_file_path + output_file_name, index=False)

    con.close()

if __name__ == "__main__":
    main()