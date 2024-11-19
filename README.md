# AV Parole Project

The AV Parole Project is dedicated to analyzing and visualizing data
related to individuals in prison who are past their parole eligibility.
The project leverages data from the National Corrections Reporting
Program (NCRP) to generate insights into the criminal justice system,
focusing on trends in incarceration, parole eligibility, and disparities
across race, ethnicity, and sex.

# Objectives

-   Data Processing: Load and clean NCRP records from 2014 to 2020 to
    create standardized datasets for analysis.  
-   Disparity Analysis: Identify and quantify disparities in
    incarceration and parole eligibility based on race, ethnicity, and
    sex.  
-   Data Visualization: Develop visualizations that communicate key
    findings, such as trends over time and comparisons between different
    demographic groups.  
-   State-Specific Insights: Generate state-level reports and
    visualizations.

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
- generate_rmds.R: Automates the creation of R Markdown documents for
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
-   **page_national_trends.R:** Uses 2023 projections to visualize national trends.
-   **helper_functions.R:** Contains reusable functions that support
    various analysis tasks, such as data transformations, statistical
    calculations, and custom visualizations.  
