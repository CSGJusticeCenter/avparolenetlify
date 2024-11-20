# AV Parole Project

The AV Parole Project is dedicated to analyzing and visualizing data
related to individuals in prison who are past their parole eligibility.
The project leverages data from the National Corrections Reporting
Program (NCRP) to generate insights into the criminal justice system,
focusing on trends in incarceration, parole eligibility, prison releases, and 
disparities across race, ethnicity, and sex.

National Snapshot Page:  
[https://avparoleproject.netlify.app/national_trends](https://avparoleproject.netlify.app/national_trends)

Example of State Report (Georgia):  
[https://avparoleproject.netlify.app/state_report_georgia](https://avparoleproject.netlify.app/state_report_georgia)

# Background

For this project, we primarily used NCRP data, including the year-end population and release files. While the most recent data available is from 2020, 2019 was found to be more reliable for analysis, and most state-specific reports rely on 2019 data. An exception is Hawaii, where 2018 data was used to ensure more reliable estimates.  

To estimate the number of individuals in prison past parole eligibility, Sebastián Guzmán (CSG Research) imputed values by integrating information from NCRP's year-end population, release, and terms records, as well as publicly available data regarding prison populations and parole eligibility criteria by state. These are called "consolidated files". 

For analyses involving age at year-end, the unconsolidated files are used, as this information is not available in the consolidated files. The unconsolidated files allow us to calculate the proportion of people in prison and past parole eligibility based on their age at year-end. 

# Objectives

-   Data Processing: Load and clean BJS and NCRP records to
    create standardized datasets for analysis.
-   Disparity Analysis: Identify and quantify disparities in
    incarceration and parole eligibility based on race, ethnicity, and
    sex.
-   Data Visualization: Develop visualizations that communicate key
    findings, such as trends over time and comparisons between different
    demographic groups.
-   State-Specific Insights: Generate state-level reports and
    visualizations.
    
# Features

National Snapshot:

- Projected parole eligibility populations by state.
- Display interactive hex map for state comparisons.

Parole Eligibility Tab:

- State-by-state trends in people in prison past parole eligibility
- Break down demographic data by race, sex, age, sentence length, and offense type.

Population Tab:

- Examine state-by-state trends in prison populations.
- Break down demographic data by race, sex, age, sentence length, and offense type.

Prison Releases Tab:

- Examine state-by-state trends in prison releases.
- Analyze release types (conditional vs. unconditional).
- Break down demographic data by race, sex, age, and offense type.

Disparities Tab:

- Calculate Relative Rate Index (RRI) for incarceration past parole eligibility.
- Visualize disparities by race, ethnicity, and sex in time served and 
incarceration past parole eligibility.

# Repository

avparolenetlify/  
│  
├── R/  
│   ├── config.R  
│   ├── pull_state_findings.R  
│   ├── generate_rmds.R  
│   ├── prep/  
│   │   ├── import/  
│   │   │   ├── import_format.R  
│   │   │   ├── historical_ncrp_term_records.R  
│   │   │   └── helper_functions_import.R  
│   │   ├── analysis/  
│   │   │   ├── tab_releases.R  
│   │   │   ├── tab_population.R  
│   │   │   ├── tab_parole_eligibility.R  
│   │   │   ├── tab_disparities.R  
│   │   │   ├── tab_disparities_rris.R  
│   │   │   ├── page_national_trends.R  
│   │   │   └── helper_functions.R  

# File Descriptions

**R/:** 
- config.R: Centralized configuration file where you define file
paths, global settings, and parameters that are used throughout the
project. This makes it easy to change settings without modifying
individual scripts.  
- pull_state_findings.R: Extracts and summarizes state-specific data
from NCRP term records. It generates findings for each state, which can
be used in the state reports.  
- generate_rmds.R: Automates the creation of Quarto documents for
rendering.  

**R/prep/:**

This folder contains scripts for data import and analysis, divided into
import and analysis subfolders.  

**R/prep/import:**

-   **import_format.R:** Importing, formatting, and standardizing raw data.  
-   **historical_ncrp_term_records.R:** Processes NCRP term records data
    from 2014 to 2020. The script loads raw data, cleans it by trimming
    characters and whitespace, and organizes the data for analysis. This
    cleaned data serves as a base for subsequent imputation by Seba Guzman 
    in Stata.  
-   **helper_functions_import.R:** Contains utility functions to assist
    with data import tasks, such as custom cleaning routines, data
    transformation functions, and helpers to manage file loading.  

**R/prep/analysis/:**

-   **tab_releases.R:** Analyzes data related to prison releases. This
    script focuses on identifying patterns and trends in release data,
    including how these trends vary by demographic factors like age,
    race, and sex.  
-   **tab_population.R:** Analyzes data related prison populations,
    including changes over time and differences across demographic
    groups.  
-   **tab_parole_eligibility.R:** Analyzes data related to parole
    eligibility, including who becomes eligible for parole and the
    characteristics of people in prison past parole eligibility.    
-   **tab_disparities.R:** Focuses on identifying disparities in parole
    eligibility by race, ethnicity, and sex.  
-   **tab_disparities_rris.R:** Computes and analyzes Relative Rate
    Indexes (RRIs) to quantify disparities in parole release.
-   **page_national_trends.R:** Uses 2023 projections to visualize 
    national trends of people in prison past parole eligibility.  
-   **helper_functions.R:** Contains reusable functions that support
    various analysis tasks, such as data transformations, statistical
    calculations, and custom visualizations.  
    
# Data Security

This data does not contain personally identifiable information and 
does not need to be destroyed by a specific deadline.

# Notes and Acknowledgments

Public Data Sources:

- National Corrections Reporting Program (NCRP)
- Bureau of Justice Statistics (BJS)

Caution for Hispanic Data:

- Interpret RRI findings for Hispanic populations with caution due to 
inconsistent data collection and reporting across states.

Authors:

- Mari Roberts, Sebastián Guzman
