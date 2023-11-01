# AV Parole Project

The Council of State Governments (CSG) Justice Center, funded by Arnold Ventures, intends to illuminate parole release policies and practices in the United States and how they impact prison population sizes and lengths of stay, using data from the National Corrections Reporting Program (NCRP) from up to 45 states to examine trends in their parole-eligible populations and conduct original empirical research on racial, ethnic, and gender disparities on the portion of sentences that are served in carceral settings past parole eligibility.

<br>

## Deliverables

Netlify Site: https://avparoleproject.netlify.app/\
Password: csgjcavparole

<br>

## Repository Structure

```         
  |-- avparolenetlify 
    |-- index.qmd                  # Landing page
    |-- state_report_links.qmd     # State report pages
    |-- national_trends.qmd        # National trends page
    |-- missing_data.qmd           # Missing data page
    |-- styles.css                 # CSS code for website design
    |-- logs.txt                   # Log file  
    |-- _state_report_template.qmd # Template for autogeneration of the state reports 
    |--
    |-- state_report_STATE.qmd     # State reports for all 50 states  
    |--      
    |-- prep 
      |-- library.R                    # Action required/packages
      |-- function.R                   # Custom functions
      |-- import.R                     # Imports data
      |-- page_missing_data.R          # Prepares tables exploring missing data for missing data page
      |-- page_national_trends.R       # Prepares visualizations and data for national trends page
      |-- tab_eligibility.R            # Prepares visualizations and data for eligibility tab
      |-- tab_releases.R               # Prepares visualizations and data for releases tab
      |-- tab_prison_population.R      # Prepares visualizations and data for population tab
      |-- tab_disparities.R            # Prepares visualizations and data for disparities tab
      |-- dataframes.R                 # Loads dataframes needed to run each page's QMD.
      |-- 
      |-- generate_netlfy_site.R       # The only file needed to generate the app (action required in library.R)
  
```

## How to Run this Code

To clean NCRP data, prepare visualizations, and generate website pages, open the library.R file and make sure you have all packages downloaded and that your Sharepoint path is set. Then run generate_netlify_site.R. In the generate_netlify_site.R file, you have the option to re-run all code to create the Netlify site or load saved data to create the Netlify site.  

## Data Limitations

The CSG Justice Center staff have encountered challenges with the integrity and completeness of open-source data, particularly from the NCRP. Several states have exhibited inconsistencies in their reporting, and some have not participated in the most recent year of publicly available data. Specifically, in 2020, four states—New Jersey, New Mexico, Michigan, and Arizona—did not contribute any data to the NCRP, even if they had in prior years. Furthermore, 17 states, including Alabama, Alaska, Arkansas, Connecticut, Delaware, Florida, Hawaii, Idaho, Illinois, Iowa, Maine, Minnesota, Ohio, Oregon, Utah, Vermont, and Virginia, provided data but omitted key information about parole eligibility. Beyond these reporting gaps, there are deeper intricacies related to demographics: Alabama, for instance, does not offer granular details on race and ethnicity. This is compounded by the broader dataset challenge concerning the vague "Other, non-Hispanic" category, which obscures a more detailed understanding of this group.  

## Data Information

-   This data is stored in [Sharepoint](https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EjOiusd2IBpEtWhY0xufTs0BcfvztTih-w-VsEtq3171JQ?e=c7E0GP).\
-   This data is not from partners and does not need to be logged in the data inventory.\
-   This data does not have PII.\
-   This data does not require data destruction.

## Sources

Alper, Mariel E., et al. "Profiles in Parole Release and Revocation: Examining the Legal Framework in the United States." Robina Institute of Criminal Law and Criminal Justice, 13 May 2022, robinainstitute.umn.edu/publications/profiles-parole-release-and-revocation-examining-legal-framework-united-states.

Carson, Ann E. "Prisoners in 2020 -- Statistical Tables." Bureau of Justice Statistics, 1 Dec. 2020, bjs.ojp.gov/library/publications/prisoners-2020-statistical-tables.

United States. Bureau of Justice Statistics. Annual Parole Survey, 2018. Inter-university Consortium for Political and Social Research \[distributoR\], 2021-10-28. https://doi.org/10.3886/ICPSR38058.v1.

United States. Bureau of Justice Statistics. National Corrections Reporting Program, 1991-2020: Selected Variables. Inter-university Consortium for Political and Social Research \[distributoR\], 2022-11-28. https://doi.org/10.3886/ICPSR38492.v1.

<br>

## Netlify File types

Here's a concise overview of the file types required to build the site:

quarto.yml  

Defines the website's structure, theme, and the architecture of the _site folder. Introducing new pages or sections to the site requires entries here, accompanied by their respective html files.

_site folder  

Comprises the actual content of the site, produced during the rendering process. This folder mirrors the main repo, but file extensions are ".html" instead of ".qmd".

img folder  

Stores images (logos, gifs, etc.) used on the site. Images specific to particular pages should be placed in a corresponding img subfolder inside the state folder. During the render process, the img folders are replicated into the _site folder -- maintaining both sets is crucial for the site's operation.

styles.css  

Contains the site's CSS settings. While it can remain empty, its presence in the repo is essential for successful rendering. This file is also duplicated into _site during rendering, and both instances are necessary.

<file name\>.qmd  

These Quarto files represent individual pages on the site. They transform into HTML files during the rendering phase.
