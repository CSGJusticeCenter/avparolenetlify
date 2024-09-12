#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: July 15, 2024 (MAR)
# Description:
#    This script generates parole eligibility visualizations and related summaries
#    for the "Parole Eligibility" tab in state reports.
#
#    Key Components:
#    - **Prison Population by Parole Eligibility Status**: Filters the NCRP prison population data by specific criteria,
#      including new court commitments and sentence lengths of 1-25 years, to analyze people in prison past their parole eligibility date.
#      It then visualizes the proportion of individuals in different parole eligibility statuses.
#
#    - **Demographic Breakdown**: Analyzes and visualizes parole eligibility status by demographic factors such as race, sex, and age for
#      people in prison with new court commitments and sentence lengths between 1 and 25 years.
#
#    - **Offense Type Analysis**: Breaks down the parole eligibility population by offense types (e.g., violent, non-violent) to see what
#      percentage of people are in prison past their eligibility date based on the crimes committed.
#
#    - **Sentence Length Distribution**: Examines parole eligibility status by sentence length for individuals in prison past their parole eligibility year,
#      with a focus on people sentenced to 1-24.9 years.
#
#    For each of these components, the script generates both **visualizations** (e.g., stacked bar charts, column charts) and **descriptive sentences**
#    to summarize the findings for each state.
#
#    Finally, the output data and visualizations are saved as `.rds` files for later use in the interactive tool.
#######################################

# pes = parole eligibility status
# pop = population
# ncrp = NCRP data



