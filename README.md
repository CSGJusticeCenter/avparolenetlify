---
editor: 
  markdown: 
    wrap: 72
---

# AV Parole Project

The AV Parole Project is dedicated to analyzing and visualizing data
related to individuals in prison who are past their parole eligibility.
The project leverages data from the National Corrections Reporting
Program (NCRP) to generate insights into the criminal justice system,
focusing on trends in incarceration, parole eligibility, and disparities
across race, ethnicity, and sex.

# Objectives

-   Data Processing: Load and clean NCRP records from 2014 to 2020 to
    create standardized datasets for analysis.\
-   Disparity Analysis: Identify and quantify disparities in
    incarceration and parole eligibility based on race, ethnicity, and
    sex.\
-   Data Visualization: Develop visualizations that communicate key
    findings, such as trends over time and comparisons between different
    demographic groups.\
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

-   **import_format.R:** Script used for formatting and standardizing
    raw data as it is imported. It ensures that the data has a uniform
    structure and format, which is essential for accurate analysis.  
-   **historical_ncrp_term_records.R:** Processes NCRP term records data
    from 2014 to 2020. The script loads raw data, cleans it by trimming
    characters and whitespace, and organizes the data for analysis. This
    cleaned data serves as a base for subsequent imputation and
    analysis.  
-   **helper_functions_import.R:** Contains utility functions to assist
    with data import tasks, such as custom cleaning routines, data
    transformation functions, and helpers to manage file loading. These
    functions streamline the data preparation process.  

**R/prep/analysis/:**

-   **tab_releases.R:** Analyzes data related to prison releases. This
    script focuses on identifying patterns and trends in release data,
    including how these trends vary by demographic factors like age,
    race, and sex.  
-   **tab_population.R:** Examines trends in prison population data,
    including changes over time and differences across demographic
    groups. It provides insights into the composition and fluctuations
    of the incarcerated population.  
-   **tab_parole_eligibility.R:** Analyzes data related to parole
    eligibility, including who becomes eligible for parole and the
    factors that influence eligibility. It generates insights into
    patterns and disparities related to parole opportunities.  
-   **tab_disparities.R:** Focuses on identifying disparities in parole
    and incarceration outcomes by race, ethnicity, and sex. This script
    helps highlight systemic inequalities and areas where policy
    interventions may be needed.  
-   **tab_disparities_rris.R:** Computes and analyzes Relative Rate
    Indexes (RRIs) to quantify disparities in parole and incarceration
    outcomes. This script is essential for understanding the magnitude
    of disparities between different groups.  
-   **page_national_trends.R:** Compiles national-level trends in the
    data, providing a broad overview of how parole and incarceration
    patterns have changed over time across the United States. It creates
    visualizations that are useful for presentations and reports.  
-   **helper_functions.R:** Contains reusable functions that support
    various analysis tasks, such as data transformations, statistical
    calculations, and custom visualizations. These functions keep the
    codebase organized and efficient.  
