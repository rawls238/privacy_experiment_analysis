import psycopg2
import pandas as pd
import random
import numpy as np
from scipy import stats
import os # Added for os.path.join
from datetime import datetime # Added for update_database timestamp


# Define working directory (Updated)
#WD = '/Users/marklee/Dropbox/' ###### MK: changed it to read the new survey
WD = "/Users/guyaridor/Dropbox/Privacy-Experiment/spring2025experiment/onboarding_sequence_scripts/"
DATA_WD = "/Users/guyaridor/Dropbox/Privacy-Experiment/spring2025experiment/"


# Database configuration (Updated)
DB_CONFIG = {
    'host': 'ec2-54-211-174-125.compute-1.amazonaws.com',
    'dbname': 'd56bkd3ac3tfpl',
    'user': 'ulv2hiihjeu22',
    'password': 'p26eb36e67e9fd5f79a771937bc626f40faaa904d80eb1e58132120dc349782b6',
    'port': 5432
}


###### Time filters - MK: updated
START_TSTAMP_WAVE_ONE = 1749873600
END_TSTAMP_WAVE_ONE = 1751083200

START_TSTAMP_WAVE_TWO =  1751083200 #1751428800# #1751428800
END_TSTAMP_WAVE_TWO = 1752282000


def get_people():
    '''
    Retrieves experiment_id and email from database, then enriches with time data
    '''
    conn = None
    db_cursor = None
    df = None
    try:
        # Connect using DB_CONFIG dictionary (Updated)
        conn = psycopg2.connect(**DB_CONFIG)
        db_cursor = conn.cursor()

        wave_one_sql = """
        SELECT experiment_id, email, experiment_condition
        FROM experiment_conditions_pilot_july_2024
        WHERE wave_id = 1
          AND experiment_condition != ''
        """
        db_cursor.execute(wave_one_sql)
        rows = db_cursor.fetchall()
        cols = [desc[0] for desc in db_cursor.description]
        wave_one_df = pd.DataFrame(rows, columns=cols)
        wave_one_df['experiment_id'] = wave_one_df['experiment_id'].astype(str).str.zfill(7)
        wave_one_df['wave_id'] = 1

        # --- Wave 2 (all other active participants) ---
        wave_two_sql = """
            SELECT experiment_id, email
            FROM experiment_conditions_pilot_july_2024
            WHERE in_experiment = 'true'
              AND wave_id != 1
        """
        db_cursor.execute(wave_two_sql)
        rows = db_cursor.fetchall()
        if not rows:
            print("No active participants found for wave 2; exiting.")
            return None
        cols = [desc[0] for desc in db_cursor.description]
        wave_two_df = pd.DataFrame(rows, columns=cols)
        wave_two_df['experiment_id'] = wave_two_df['experiment_id'].astype(str).str.zfill(7)
        wave_two_df['wave_id'] = 2
        wave_two_df['experiment_condition'] = None

        # Combine into one lookup table
        participants = pd.concat(
            [wave_one_df[['experiment_id', 'email', 'experiment_condition', 'wave_id']],
             wave_two_df[['experiment_id', 'email', 'experiment_condition', 'wave_id']]],
            ignore_index=True
        )

        # Enrich with time data (logic from original script)
        ###### MK: read the time_data_2_w_privacy.csv - data/extension_data_06_24.
        enriched_time_data_all = pd.read_csv(
            os.path.join(DATA_WD, 'data', 'processed_data', 'enriched_time_data_2_wave_2.csv')
        )
        enriched_time_data_all['experiment_id'] = (
            enriched_time_data_all['experiment_id']
            .astype(str)
            .str.zfill(7)
        )
        enriched_time_data_all['date'] = pd.to_datetime(enriched_time_data_all['date'])
        enriched_time_data_all = enriched_time_data_all.sort_values(by=['experiment_id', 'date'])

        min_dates = enriched_time_data_all.groupby('experiment_id')['date'].transform('min')

        enriched_time_data_all = (
            enriched_time_data_all[enriched_time_data_all['date'] != min_dates]
            .reset_index(drop=True)
)        # --- START: New code to filter out survey websites ---
        print("\n--- Filtering survey websites from time data ---")
        
        SURVEY_WEBSITES = [
            "qualtrics", "vercel", "respondent", "cmix", "raterproject", "questionpro", 
            "usertesting", "netlify", "primeopinion", "paidviewpoint", "surveymonkey", 
            "surveymeasure", "alchemer", "yougov", "prolific", "mturk", "cloudresearch", "guyaridor", "chromewebstore"
        ]


        # Helper function to check if a website string corresponds to a survey platform
        def is_survey_site(website, survey_list):
            # Return False for non-string or empty inputs, mimicking R's is.na check
            if pd.isna(website) or website == "":
                return False
            # Split the domain into parts (e.g., 'sub.domain.com' -> ['sub', 'domain', 'com'])
            website_parts = str(website).lower().split('.')
            # Check if any part of the domain is in our list of survey sites
            return any(part in survey_list for part in website_parts)


        # Report original size
        num_rows_before = len(enriched_time_data_all)
        print(f"Original number of rows in time data: {num_rows_before}")
        
        # Create a boolean series: True for rows to KEEP (i.e., not a survey site)
        # The `~` inverts the result of .apply, so we keep rows where is_survey_site is False.
        rows_to_keep = ~enriched_time_data_all['website'].apply(is_survey_site, survey_list=SURVEY_WEBSITES)
        
        # Filter the DataFrame
        enriched_time_data_all = enriched_time_data_all[rows_to_keep]
        num_rows_after = len(enriched_time_data_all)
        
        # Report the number of removed rows and the new total
        print(f"Number of rows about survey websites removed: {num_rows_before - num_rows_after}")
        print(f"New number of rows in time data: {num_rows_after}")
        print("--- End of filtering ---\n")
        # --- END: New code to filter out survey websites ---


        enriched_time_data_all['tstamp'] = pd.to_numeric(enriched_time_data_all['tstamp'], errors='coerce').fillna(0).astype(int)
        enriched_time_data_all['experiment_id'] = enriched_time_data_all['experiment_id'].astype(str)
        
        # Filter by experiment IDs that are in our df
        enriched_time_data_all = enriched_time_data_all.merge(participants, on='experiment_id', how='inner')
        print(f"After merging participant waves: {len(enriched_time_data_all)} rows.")

        # --- Apply wave-specific timestamp filters ---
        mask_wave1 = (
            (enriched_time_data_all['wave_id'] == 1) &
            (enriched_time_data_all['tstamp'] > START_TSTAMP_WAVE_ONE) &
            (enriched_time_data_all['tstamp'] < END_TSTAMP_WAVE_ONE)
        )
        mask_wave2 = (
            (enriched_time_data_all['wave_id'] == 2) &
            (enriched_time_data_all['tstamp'] > START_TSTAMP_WAVE_TWO) &
            (enriched_time_data_all['tstamp'] < END_TSTAMP_WAVE_TWO)
        )

        enriched_time_data_all = enriched_time_data_all[mask_wave1 | mask_wave2].copy()
        # Apply time filters
       # enriched_time_data_all = enriched_time_data_all[enriched_time_data_all['tstamp'] > START_TSTAMP_WAVE_ONE]
       # enriched_time_data_all = enriched_time_data_all[enriched_time_data_all['tstamp'] < END_TSTAMP_WAVE_ONE]


        if enriched_time_data_all.empty:
            print("No time data found for the participants within the specified time range.")
            participants['total_time_spent_overall'] = 0.0
            participants['total_time_spent'] = 0.0
        else:
            # Calculate total_time_spent_overall (all domains)
            total_time_overall_calc = enriched_time_data_all.groupby('experiment_id')['time_spent'].sum().reset_index()
            total_time_overall_calc['time_spent'] = total_time_overall_calc['time_spent'] / 3600
            total_time_overall_calc.rename(columns={'time_spent': 'total_time_spent_overall'}, inplace=True)
            #participants = pd.merge(participants, total_time_overall_calc, on='experiment_id', how='left')


            # Calculate total_time_spent (privacy-specific)
            if 'privacy_exist' in enriched_time_data_all.columns:
                privacy_time_df = enriched_time_data_all[enriched_time_data_all['privacy_exist'] == True] # Explicitly check for True
                if not privacy_time_df.empty:
                    today = pd.to_datetime('today').normalize()

                    total_time_privacy_calc = (
                        privacy_time_df
                        .groupby('experiment_id')
                        .agg(
                            total_time_spent = ('time_spent', lambda s: s.sum()/3600),
                            first_active_date = ('date', 'min')
                        )
                        .reset_index()
                        .assign(
                            avg_daily_time=lambda df: df['total_time_spent'] / 
                                ((today - df['first_active_date']).dt.days + 1)
                        )
                        .loc[:, ['experiment_id', 'total_time_spent', 'avg_daily_time']]
)
                    #participants = pd.merge(participants, total_time_privacy_calc, on='experiment_id', how='left')
                else:
                    participants['total_time_spent'] = 0.0
                    participants['avg_daily_time'] = 0.0
            else:
                print("Warning: 'privacy_exist' column not found in time data. 'total_time_spent' will be NaN.")
                participants['total_time_spent'] = 0.0
                participants['avg_daily_time'] = 0.0
        
        participants = participants.merge(
            total_time_overall_calc,
            on='experiment_id',
            how='left'
        )
        participants['total_time_spent_overall'] = participants[
            'total_time_spent_overall'
        ].fillna(0)

        # Merge privacy‐specific
        participants = participants.merge(
            total_time_privacy_calc,
            on='experiment_id',
            how='left'
        )
        participants['total_time_spent'] = participants['total_time_spent'].fillna(0)
        participants['avg_daily_time']   = participants['avg_daily_time'].fillna(0)
        # Filter based on total_time_spent (privacy-specific)
        #participants.dropna(subset=['total_time_spent'], inplace=True) # Remove rows where total_time_spent is NaN
        #participants = participants[participants['total_time_spent'] > 0]                 # Keep only rows where total_time_spent is positive
        if participants.empty:
            print("No participants remaining after time data enrichment and filtering.")
            return None


        #participants['ltotal_time_spent'] = np.log(participants['total_time_spent'])
        participants = (
            participants
            .sort_values(
                by=['wave_id', 'total_time_spent'],
                ascending=[True, False]    # wave_id ↑, then total_time_spent ↓
            )
            .reset_index(drop=True)
        )


    except (Exception, psycopg2.DatabaseError) as error:
        print(f"An error occurred in get_people: {error}")
        return None
    finally:
        if db_cursor:
            db_cursor.close()
        if conn:
            conn.close()
    return participants


def read_csv_full(csv_file):
    """
    This function reads a CSV file and returns a DataFrame.
    (Original function from your script)
    """
    return pd.read_csv(csv_file)
    #df_only_answers = df_with_questions.iloc[2:]
    #return df_only_answers


def assign_groups(df_ppl):
    conditions = ['info', 'control', 'saliency']
    # These filters are now applied in get_people, but defensive checks are okay.
    wave_one_df = df_ppl[df_ppl['wave_id'] == 1]
    wave_two_df = df_ppl[df_ppl['wave_id'] == 2]
    #wave_two_df  = wave_two_df[wave_two_df['total_time_spent'] > 0] 
    #wave_two_df  = wave_two_df [~wave_two_df['total_time_spent'].isna()]
    wave_two_df  = wave_two_df .sort_values(by='total_time_spent', ascending=False).reset_index(drop=True) # Ensure sorted and index reset
    wave_two_df['experiment_condition'] = None
    
    N_rows = len(wave_two_df)
    if N_rows == 0:
        return wave_two_df # Return empty if no rows


    # Stratified assignment (assigning by trios based on sorted time)
    # This is a form of stratified randomization where strata are implicit by sorting.
    assigned_indices = []
    block_idx = 0
    for i in range(0, N_rows - (N_rows % 3), 3):
        block_idx = i // 3
        block_indices = df_ppl.index[i:i+3]
        shuffled_conditions = random.sample(conditions, 3)
        for j, cond_idx in enumerate(block_indices):
            wave_two_df.loc[cond_idx, 'experiment_condition'] = shuffled_conditions[j]
            wave_two_df .loc[cond_idx, 'block_idx'] = block_idx
        assigned_indices.extend(block_indices)
    block_idx += 1

    # Handle remaining rows if N_rows is not a multiple of 3
    remainder_indices = wave_two_df.index[N_rows - (N_rows % 3):]
    if len(remainder_indices) > 0:
        shuffled_conditions = random.sample(conditions, len(remainder_indices))
        for i, cond_idx in enumerate(remainder_indices):
            wave_two_df.loc[cond_idx, 'block_idx'] = block_idx
            wave_two_df.loc[cond_idx, 'experiment_condition'] = shuffled_conditions[i]
    combined_df = pd.concat([wave_one_df, wave_two_df], ignore_index=True)
    return combined_df




def compare_means_and_generate_latex(df_control, df_saliency, df_information, columns, alpha=0.15):
    '''
    (Original function from your script)
    '''
    all_p_values_above_threshold = True
    
    latex_table = r"""
\begin{table}[h!]
\centering
\begin{tabular}{|l|c|c|c|c|c|c|c|c|c|c|c|c|c|}
\hline
 & \multicolumn{3}{c|}{Control} & \multicolumn{3}{c|}{Saliency} & \multicolumn{3}{c|}{Information} & \multirow{2}{*}{Diff. in Means (S-C)} & \multirow{2}{*}{Diff. in Means (I-C)} & \multirow{2}{*}{p (S vs C)} & \multirow{2}{*}{p (I vs C)} \\
 & N & Mean & Std. Dev. & N & Mean & Std. Dev. & N & Mean & Std. Dev. &  &  &  &  \\
\hline
"""
    for column in columns:
        control = df_control[column].dropna().astype(float) # Ensure numeric for stats
        saliency = df_saliency[column].dropna().astype(float)
        information = df_information[column].dropna().astype(float)
        
        control_mean = np.mean(control) if len(control) > 0 else np.nan
        control_std = np.std(control, ddof=1) if len(control) > 1 else np.nan
        control_n = len(control)
        
        saliency_mean = np.mean(saliency) if len(saliency) > 0 else np.nan
        saliency_std = np.std(saliency, ddof=1) if len(saliency) > 1 else np.nan
        saliency_n = len(saliency)
        
        information_mean = np.mean(information) if len(information) > 0 else np.nan
        information_std = np.std(information, ddof=1) if len(information) > 1 else np.nan
        information_n = len(information)
        
        diff_in_means_s_c = saliency_mean - control_mean if control_n > 0 and saliency_n > 0 else np.nan
        diff_in_means_i_c = information_mean - control_mean if control_n > 0 and information_n > 0 else np.nan
        
        p_value1, p_value2 = np.nan, np.nan
        # Perform ANOVA tests only if there are enough data points in both groups being compared
        if control_n >= 2 and saliency_n >= 2:
            _, p_value1 = stats.f_oneway(control, saliency)
        if control_n >= 2 and information_n >= 2:
            _, p_value2 = stats.f_oneway(control, information)
        
        # Check p-values (only if they could be calculated)
        if not np.isnan(p_value1) and p_value1 <= alpha:
            all_p_values_above_threshold = False
        if not np.isnan(p_value2) and p_value2 <= alpha:
            all_p_values_above_threshold = False
        
        latex_table += r"{} & {} & {:.3f} & {:.3f} & {} & {:.3f} & {:.3f} & {} & {:.3f} & {:.3f} & {:.3f} & {:.3f} & {:.3f} & {:.3f} \\".format(
            column.replace('_', '\\_'), control_n, control_mean, control_std, saliency_n, saliency_mean, saliency_std, information_n, information_mean, information_std, diff_in_means_s_c, diff_in_means_i_c, p_value1, p_value2
        )
        latex_table += "\n\\hline\n"


    latex_table += r"""
\end{tabular}
\caption{Comparison of Means, Standard Deviations, Sample Sizes, and P-values for Multiple Fields}
\label{table:comparison}
\end{table}
"""
    return all_p_values_above_threshold, latex_table


def check_balance(cur_df):
    '''
    (Original function from your script, with WD updated path)
    '''
    survey_file_path = os.path.join(WD, "qualtrics_csvs", "survey_full_eligible.csv")
    try:
        baseline_survey = read_csv_full(survey_file_path)
        baseline_survey["email"] = baseline_survey["email"].str.lower()
        cur_df["email"] = cur_df["email"].str.lower()
    except FileNotFoundError:
        print(f"ERROR: Survey file not found at {survey_file_path}")
        return False, "Survey file not found." # Cannot perform balance check
    ###### MK: change 'contact' to 'emailid'
    merged_df = pd.merge(cur_df, baseline_survey, left_on='email', right_on='email', how='left')

    merged_df = merged_df.drop_duplicates(subset=['email'], keep='first')
    merged_df.dropna(subset=['total_time_spent'], inplace=True)
    
    # Ensure experiment_condition is not NaN for proper splitting
    merged_df.dropna(subset=['experiment_condition'], inplace=True)
    
    control_df = merged_df[merged_df['experiment_condition'] == 'control']
    saliency_df = merged_df[merged_df['experiment_condition'] == 'saliency']
    info_df = merged_df[merged_df['experiment_condition'] == 'info']
    extension_cols_to_compare = ['total_time_spent', 'avg_daily_time', 'total_time_spent_overall']
    survey_cols_to_compare = ['is_male', 'college_graduate', 'cleaned_age', 'cleaned_income', 'not_disclosing_income', 'is_white', 'privacy_attitude_index', 
    'privacy_knowledge', 'cleaned_WTA', 'beliefs_use', 'beliefs_collection', 'beliefs_control', 'beliefs_quality',
     'news_conjoint_cat', 'entertainment_conjoint_cat', 'ecom_conjoint_cat', 'social_conjoint_cat', 'privacy_extensive_margin']
    cols_to_compare = extension_cols_to_compare + survey_cols_to_compare
    # Check if essential columns for comparison exist, if not, return error
    missing_cols = [col for col in cols_to_compare if col not in merged_df.columns]
    if any(col not in ['total_time_spent', 'total_time_spent_overall'] for col in missing_cols):
         print(f"Warning: Essential balance check columns are missing from merged_df: {missing_cols}")
         # Decide how to handle this - for now, we let compare_means_and_generate_latex handle missing cols.


    return compare_means_and_generate_latex(control_df, saliency_df, info_df, cols_to_compare)




if __name__ == "__main__":
    print(">>> 1. Get participants' data") # Changed message slightly
    df_ppl = get_people()


    if df_ppl is not None and not df_ppl.empty:
        df_ppl.to_csv(os.path.join(WD,'df_before_assign.csv'), index=False) # Use WD for path
        print(f"Saved data before assignment to {os.path.join(WD,'df_before_assign.csv')}")
        print("--------------------------------------")
    else:
        print("No people found or processed from database and time data.")
        print("--------------------------------------")
        exit() # Exit if no participants


    balanced = False
    balance_table_latex = None # Renamed for clarity
    df_assigned_final = None # To store the successfully balanced assignment


    # Loop until balanced assignment is found (original script logic)
    # Added a max attempts to prevent infinite loop
    max_attempts = 100
    attempt_count = 0
    
    while not balanced and attempt_count < max_attempts:
        attempt_count += 1
        print(f">>> Attempt {attempt_count}: Assigning groups and checking balance...")
        # df_ppl.copy() is important if assign_groups modifies it in a way that shouldn't persist across attempts
        df_assigned_attempt = assign_groups(df_ppl.copy()) 
        
        if df_assigned_attempt is None or df_assigned_attempt.empty:
            print("Assignment resulted in an empty DataFrame. Retrying...")
            continue


        try:
            balanced, balance_table_latex = check_balance(df_assigned_attempt.copy()) # Pass a copy
            if balanced:
                df_assigned_final = df_assigned_attempt
                print(">>> Assignment achieved balance!")
            else:
                print(">>> Assignment did not achieve balance. Retrying...")
        except Exception as e:
            print(f"Error during balance check: {e}. Retrying assignment...")
            balanced = False # Ensure loop continues
            
    if df_assigned_final is not None and balanced:
        print("\nFinal Balance Table (LaTeX):")
        print(balance_table_latex)
        mask = (
            df_assigned_final['block_idx'].notna()               # not null/NaN
            & df_assigned_final['block_idx'].astype(str)         # cast non-strings safely
                  .str.strip()                                   # trim whitespace
                  .ne('')                                       # not equal to empty
        )

        df_assigned_final = df_assigned_final[mask].reset_index(drop=True)
        df_assigned_final.to_csv(os.path.join(WD,'df_after_assign.csv'), index=False) # Use WD
        print(f"Saved balanced assignment to {os.path.join(WD, 'df_after_assign.csv')}")
        print("--------------------------------------")
        
        #print(">>> 3. Update the database with the balanced assignment")
        #if push_to_db(df_assigned_final):
        #    print("Database update successful.")
        #else:
        #    print("Database update failed.")
        #print("--------------------------------------")
    elif attempt_count >= max_attempts:
        print(f"Could not achieve balance after {max_attempts} attempts. No data pushed to DB.")
        if df_assigned_attempt is not None: # Save the last attempt if loop finished due to attempts
             df_assigned_attempt.to_csv(os.path.join(WD,'df_after_assign_unbalanced_last_attempt.csv'), index=False)
             print(f"Saved last unbalanced assignment attempt to {os.path.join(WD,'df_after_assign_unbalanced_last_attempt.csv')}")
    else:
        print("No valid assignment could be generated or an error occurred. No data pushed to DB.")
        print("--------------------------------------")


    # The check part of original main block - may not be necessary if push_to_db works
    # print(">>> Checking database after update (optional)")
    # df_ppl_2 = get_people() # This would fetch again, potentially before cache clear or if in_experiment changes
    # if df_ppl_2 is not None:
    #     df_ppl_2.to_csv(os.path.join(WD,'df_check_after_update.csv'), index=False)
    #     print(f"Saved data from DB after script run to {os.path.join(WD,'df_check_after_update.csv')}")
    # else:
    #     print("No people founded in get_people after script run.")
    # print("--------------------------------------")
