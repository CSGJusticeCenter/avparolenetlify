# https://dhhr.wv.gov/office-of-drug-control-policy/datadashboard/Pages/default.aspx

# Load required packages
library(rvest)
library(dplyr)

# Define the URL of the dashboard
url <- "https://dhhr.wv.gov/office-of-drug-control-policy/datadashboard/Pages/default.aspx"

# Read the HTML content of the page
webpage <- read_html(url)

# Function to extract data based on dropdown selections
extract_data <- function(year, county) {
  # Simulate the selection of dropdown options (adjust selectors accordingly)
  year_dropdown <- html_node(webpage, css = "#YearMonth") %>% html_form() %>% set_values(YearMonth = year)
  county_dropdown <- html_node(webpage, css = "#County") %>% html_form() %>% set_values(County = county)

  # Submit the form with the selected options and read the updated page
  updated_page <- submit_form(webpage, year_dropdown) %>%
    submit_form(county_dropdown)

  # Extract the relevant data from the updated page (adjust selectors accordingly)
  data <- updated_page %>%
    html_nodes(css = ".data-class") %>%
    html_text()

  return(data)
}

# List of years and counties to loop over (replace with actual options)
years <- c("2024", "2023", "2022") # Example years
counties <- c("All", "Barbour", "Boone") # Example counties

# Initialize an empty list to store the data
all_data <- list()

# Loop over each combination of year and county and extract data
for (year in years) {
  for (county in counties) {
    data <- extract_data(year, county)
    all_data[[paste(year, county, sep = "_")]] <- data
  }
}

# Combine all extracted data into a single data frame
combined_data <- bind_rows(all_data, .id = "Year_County")

# Print the combined data
print(combined_data)
