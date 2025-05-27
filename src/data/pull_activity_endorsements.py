import pandas as pd
import numpy as np

from utils import load_credentials, connect_to_database

def main():
    derived_tables = """
    with 
        activity_id_to_name as (
            select distinct activityId, name as activity_name
            from activities
            union all 
            select distinct activityId, name as activity_name
            from user_activities
            order by activityId
        ),
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
            order by pId
        ),
        phase1_selected_activities as (
            select distinct
                user_activity_preferences.pId,
                phase1_start_date,
                phase1_end_date,
                user_activity_preferences.activityId,
                activity_name
            from user_activity_preferences
            left join activity_id_to_name on user_activity_preferences.activityId = activity_id_to_name.activityId
            left join phase1_dates on user_activity_preferences.pId = phase1_dates.pId
            where 
                participantPhaseId like "%_PHASE_1" and
                user_activity_preferences.pId like '1%' and 
                user_activity_preferences.pId != '121'
            order by pId, activityId
        ),
        phase1_endorsed_activities as (
            select distinct
                survey_response_details.surveyId as survey_id_daily,
                survey_responses.pId,
                date,
                activityId,
                1 as endorsed,
                case  
                    when score = -1 then null
                    else score 
                end as activity_rating
            from survey_response_details 
            left join survey_responses on survey_response_details.surveyId = survey_responses.surveyId
            left join phase1_dates on survey_responses.pId = phase1_dates.pId
            where 
                date >= phase1_start_date and
                date <= phase1_end_date and
                survey_response_details.surveyId like '%_DAILY_%' and 
                survey_responses.pId like '1%' and 
                survey_responses.pId != '121'
            order by pId, date, activityId
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
            order by pId
        ),
        phase2_selected_activities as (
            select distinct
                user_activity_preferences.pId,
                phase2_start_date,
                phase2_end_date,
                user_activity_preferences.activityId,
                activity_name
            from user_activity_preferences
            left join activity_id_to_name on user_activity_preferences.activityId = activity_id_to_name.activityId
            left join phase2_dates on user_activity_preferences.pId = phase2_dates.pId
            where 
                participantPhaseId like "%_PHASE_2" and
                user_activity_preferences.pId like '1%' and 
                user_activity_preferences.pId != '121'
            order by pId, activityId
        ),
        phase2_planned_activities as (
            select distinct
                survey_response_details.surveyId as survey_id_morning,
                survey_responses.pId,
                date,
                activityId,
                1 as planned
            from survey_response_details 
            left join survey_responses on survey_response_details.surveyId = survey_responses.surveyId
            left join phase2_dates on survey_responses.pId = phase2_dates.pId
            where 
                date >= phase2_start_date and
                date <= phase2_end_date and
                survey_response_details.surveyId like '%_MORNING_%' and 
                survey_responses.pId like '1%' and 
                survey_responses.pId != '121'
            order by pId, date, activityId
        ),
        phase2_completed_activities as (
            select distinct
                survey_response_details.surveyId as survey_id_evening,
                survey_responses.pId,
                date,
                activityId,
                1 as completed
            from survey_response_details 
            left join survey_responses on survey_response_details.surveyId = survey_responses.surveyId
            left join phase2_dates on survey_responses.pId = phase2_dates.pId
            where 
                date >= phase2_start_date and
                date <= phase2_end_date and
                survey_response_details.surveyId like '%_EVENING_%' and 
                survey_responses.pId like '1%' and 
                survey_responses.pId != '121'
            order by pId, date, activityId
        )
    """

    output_file_path = "../../data/interim/"

    tables_to_query = [
        "phase1_selected_activities", 
        "phase2_selected_activities", 
        "phase1_endorsed_activities", 
        "phase2_planned_activities", 
        "phase2_completed_activities"
    ]

    credentials = load_credentials(group="balance")
    con = connect_to_database(credentials)

    for table in tables_to_query:
        output_file = f"{output_file_path}/balance_{table}.csv"
        table_query = derived_tables + f"\nselect * from {table};"
        table_data = pd.read_sql(table_query, con)
        table_data.to_csv(output_file, index=False)

    con.close()

if __name__ == "__main__":
    main()