# AV Parole Project

The Council of State Governments (CSG) Justice Center, funded by Arnold Ventures, intends to illuminate parole release policies and practices in the United States and how they impact prison population sizes and lengths of stay, using data from the National Corrections Reporting Program (NCRP) from up to 45 states to examine trends in their parole-eligible populations and conduct original empirical research on racial, ethnic, and gender disparities on the portion of sentences that are served in carceral settings past parole eligibility.

<br>

## Sources

Alper, Mariel E., et al. "Profiles in Parole Release and Revocation: Examining the Legal Framework in the United States." Robina Institute of Criminal Law and Criminal Justice, 13 May 2022, robinainstitute.umn.edu/publications/profiles-parole-release-and-revocation-examining-legal-framework-united-states.

Carson, Ann E. "Prisoners in 2020 -- Statistical Tables." Bureau of Justice Statistics, 1 Dec. 2020, bjs.ojp.gov/library/publications/prisoners-2020-statistical-tables.

United States. Bureau of Justice Statistics. Annual Parole Survey, 2018. Inter-university Consortium for Political and Social Research \[distributoR], 2021-10-28. https://doi.org/10.3886/ICPSR38058.v1.

United States. Bureau of Justice Statistics. National Corrections Reporting Program, 1991-2020: Selected Variables. Inter-university Consortium for Political and Social Research \[distributoR], 2022-11-28. https://doi.org/10.3886/ICPSR38492.v1.

<br>

## Deliverables

Netlify Site: https://avparoleproject.netlify.app/\
Password: csgjcavparole

<br>

## Netlify File types

An extremely brief (and mostly accurate) overview of the types of files needed to build the site:

<dl>

<dt>\_quarto.yml</dt>

<dd>Sets the structure and theme of the site, and the structure of the <i>\_site</i> folder. The layout of the YAML file should mirror the folder stucture of the repo and vice-versa (i.e., repo folders align with the menus/submenus of your site). New pages or sections for the site need to be added here along with their corresponding html files.</dd>

<dt>\_site folder</dt>

<dd>The actual contents of your site, created by the rendering process. This folder's contents should mirror the main repo, but file extentions will be ".html" instead of ".qmd".</dd>

<dt>img folder</dt>

<dd>Any images (logos, gifs, etc.) for the site. Images for individual pages should be saved in an img subfolder in the state folder. The <i>img</i> folders will be copied into the <i>\_site</i> folder during the render process - you need <b>both</b> sets for the site to function.</dd>

<dt>styles.css</dt>

<dd>The css settings for the site. This file can be empty, but it must exist in the repo for everything to render. This file will also get copied into <i>\_site</i> during the render process, and you need <b>both</b> copies.</dd>

<dt>\<file name\>.qmd</dt>

<dd>The Quarto files for each page of the site. These get re-created as html files during the render process.</dd>

</dl>

## Repository Structure

```         
  |-- avparolenetlify 
    |-- index.qmd                  # Landing page
    |-- national_trends.qmd        # National trends page
    |-- state_report_links.qmd     # State report pages
    |-- missing_data.qmd           # Missing data page
    |-- styles.css                 # CSS code for website design
    |-- _state_report_template.qmd # Template for autogeneration of the state pages 
    |--
    |-- state_report_Georgia.qmd   # State report for the state of Georgia
    |--      
    |-- prep 
      |-- 00_library.R                    # Packages
      |-- 01_function.R                   # Custom functions
      |-- 02_import.R                     # Imports data
      |-- 03_tab_eligibility.R            # Prepares visualizations and data for eligibility tab
      |-- 04_tab_releases_from_prison.R   # Prepares visualizations and data for releases tab
      |-- 05_tab_offenses.R               # Prepares visualizations and data for offenses tab
      |-- 06_tab_prison_population.R      # Prepares visualizations and data for population tab
      |-- 07_tab_disparities.R            # Prepares visualizations and data for disparities tab
      |-- generate_state_reports.R        # Generates each state page based on the _state_report_template.qmd
      |-- dataframes.R                    # Loads dataframes needed to run each page's QMD.
  
```

## Processes

To clean NCRP data, prepare visualizations, and generate website pages, run the following code in this order:

-   00_library.R
-   01_function.R
-   02_import.R
-   03_tab_eligibility.R
-   04_tab_releases_from_prison.R
-   05_tab_offenses.R
-   06_tab_prison_population.R
-   07_tab_disparities.R
-   generate_state_reports.R

## Data Information

-   This data is stored in [Sharepoint](https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EjOiusd2IBpEtWhY0xufTs0BcfvztTih-w-VsEtq3171JQ?e=c7E0GP).\
-   This data is not from partners and does not need to be logged in the data inventory.\
-   This data does not have PII.\
-   This data does not require data destruction.
