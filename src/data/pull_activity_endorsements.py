import pandas as pd
import numpy as np

from utils import load_credentials, connect_to_database

def main():
    derived_tables_prefix = f""" 
    with 
        phase1_dates as (
            select pId, startDate as phase1_start_date, date_sub(endDate, interval 1 day) as phase1_end_date
            from user_study_phases
            where phaseId = 'PHASE_1' and pId like '1%' and pId != '121'
        ),
        phase2_dates as (
            select pId, startDate as phase2_start_date, date_sub(endDate, interval 1 day) as phase2_end_date
            from user_study_phases
            where phaseId = 'PHASE_2' and pId like '1%' and pId != '121'
        ),
        activity_id_to_name as (
            select activityId, name as activity_name, 1 as custom_activity
            from user_activities
            union all
            select activityId, name as activity_name, 0 as custom_activity
            from activities
        ),
        pid_activity_preferences as (
            select pId, substring(participantPhaseId, -1) as study_phase, activityId
            from user_activity_preferences
        ),
        pid_activity_endorsements as (
            select 
                pId, 
                date, 
                sId as survey_type, 
                survey_response_details.surveyId, 
                goodnessScore as goodness_score, 
                activityId, 
                score as activity_score
            from survey_response_details
            join survey_responses on survey_response_details.surveyId = survey_responses.surveyId
        )
    """

    activity_preference_suffix = f""" 
    select distinct
        pId as record_id,
        study_phase,
        pid_activity_preferences.activityId as activity_id,
        activity_name,
        custom_activity
    from pid_activity_preferences  
    join activity_id_to_name on pid_activity_preferences.activityId = activity_id_to_name.activityId
    where pId like '1%' and pId != '121'
    order by record_id, study_phase, activity_id;    
    """

    activity_endorsement_suffix = f""" 
    select 
        pid_activity_endorsements.pId as record_id,
        date,
        case when survey_type = 'DAILY' then 1 else 2 end as study_phase,
        survey_type,
        surveyId as survey_id,
        pid_activity_endorsements.activityId as activity_id,
        activity_name,
        custom_activity,
        activity_score
    from pid_activity_endorsements
    join phase1_dates on pid_activity_endorsements.pId = phase1_dates.pId
    join phase2_dates on pid_activity_endorsements.pId = phase2_dates.pId
    join activity_id_to_name on pid_activity_endorsements.activityId = activity_id_to_name.activityId
    where 
        (survey_type = 'DAILY' and date >= phase1_start_date and date <= phase1_end_date) or 
        (survey_type in ('MORNING', 'EVENING') and date >= phase2_start_date and date <= phase2_end_date)
    order by record_id, date, survey_type desc;
    """

    output_file_path = "../../data/interim/"

    credentials = load_credentials(group="balance")
    con = connect_to_database(credentials)

    activity_preference_query = derived_tables_prefix + activity_preference_suffix
    activity_endorsement_query = derived_tables_prefix + activity_endorsement_suffix

    activity_preference_data = pd.read_sql(sql=activity_preference_query, con=con)
    activity_endorsement_data = pd.read_sql(sql=activity_endorsement_query, con=con)

    activity_preference_data.to_csv(output_file_path + "balance_activity_preferences.csv", index=False)
    activity_endorsement_data.to_csv(output_file_path + "balance_activity_endorsements.csv", index=False)

    con.close()

if __name__ == "__main__":
    main()