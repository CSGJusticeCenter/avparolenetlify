# Adjust the years to include all desired ones
all_years <- seq(min(pe_proj_pop$year, na.rm = TRUE), max(pe_proj_pop$year, na.rm = TRUE))

# Generate Line Charts for Past Parole Eligibility Projections
all_line_pop_pe_by_year <- map(states, function(x) {
  # Filter data for the current state and prepare for charting
  df1 <- pe_proj_pop |>
    filter(state == x) |>
    complete(year = all_years, fill = list(pct_past_pe = NA, proj_pct_past_pe = NA)) |> # Ensure all years are included
    mutate(
      # Get the last observed value for percentage past parole eligibility
      last_value_past_pe = last(na.omit(pct_past_pe)),

      # Identify the first year needing projection filling (if any)
      year_to_fill = if (any(!is.na(proj_pct_past_pe))) {
        min(year[!is.na(proj_pct_past_pe)], na.rm = TRUE) - 1 # Fill one year before the first projected year
      } else {
        NA_real_
      },

      # Fill projected values with the last observed value for identified years
      proj_pct_past_pe = if_else(
        is.na(proj_pct_past_pe) & year == year_to_fill,
        last_value_past_pe,
        proj_pct_past_pe
      )
    ) |>
    select(-last_value_past_pe, -year_to_fill) # Remove helper columns after processing

  # Define chart properties
  title <- "People in Prison Past Parole Eligibility by Year"
  hc_accessibility_text <- "This chart shows the percentage of people in prison who
  are past their parole eligibility year, with projections highlighted in red."

  # Create Highcharts object
  highchart() |>
    hc_chart(type = "line") |>
    hc_title(text = title) |>
    hc_xAxis(categories = all_years, lineWidth = 1) |>
    hc_yAxis(
      title = list(text = "Percent Past Parole Eligibility"),
      min = 0, max = 100, # Define Y-axis range (0–100%)
      labels = list(format = "{value}%") # Add percentage format to Y-axis labels
    ) |>
    hc_add_series(
      name = "Past Parole Eligibility",
      data = round(df1$pct_past_pe, 0), # Add observed data
      color = teal, # Set line color
      marker = list(enabled = TRUE), # Enable markers on data points
      connectNulls = TRUE, # Connect lines even if there are missing values
      tooltip = list(valueSuffix = "%") # Tooltip showing percentage
    ) |>
    hc_add_series(
      name = "Projected Past Parole Eligibility",
      data = round(df1$proj_pct_past_pe, 0), # Add projected data
      color = red, # Set line color for projections
      marker = list(enabled = TRUE),
      connectNulls = TRUE,
      tooltip = list(valueSuffix = "%")
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = TRUE) |>
    hc_exporting(
      enabled = TRUE,
      filename = paste0(gsub(" ", "_", tolower(title)), "_") # Set export file name
    ) |>
    hc_caption(text = ncrp_csg_source) |> # Add source caption
    fnc_add_hc_accessibility(hc_accessibility_text) # Add accessibility text
})

# Assign state names to the generated charts
all_line_pop_pe_by_year <- setNames(all_line_pop_pe_by_year, states)
all_line_pop_pe_by_year$Georgia
all_line_pop_pe_by_year$Colorado
all_line_pop_pe_by_year$Idaho
all_line_pop_pe_by_year$Hawaii




# Adjust the years to include all desired ones
all_years <- seq(min(pe_proj_pop$year, na.rm = TRUE), max(pe_proj_pop$year, na.rm = TRUE))

# Generate Line Charts for Past Parole Eligibility Projections
all_line_pop_pe_by_year <- map(states, function(x) {
  # Filter data for the current state and prepare for charting
  df1 <- pe_proj_pop |>
    filter(state == x) |>
    complete(year = all_years, fill = list(pct_past_pe = NA, proj_pct_past_pe = NA)) |> # Ensure all years are included
    mutate(
      # Get the last observed value for percentage past parole eligibility
      last_value_past_pe = last(na.omit(pct_past_pe)),

      # Identify the first year needing projection filling (if any)
      year_to_fill = if (any(!is.na(proj_pct_past_pe))) {
        min(year[!is.na(proj_pct_past_pe)], na.rm = TRUE) - 1 # Fill one year before the first projected year
      } else {
        NA_real_
      },

      # Fill projected values with the last observed value for identified years
      proj_pct_past_pe = if_else(
        is.na(proj_pct_past_pe) & year == year_to_fill,
        last_value_past_pe,
        proj_pct_past_pe
      )
    ) |>
    select(-last_value_past_pe, -year_to_fill) # Remove helper columns after processing

  # Define chart properties
  title <- "People in Prison Past Parole Eligibility by Year"
  hc_accessibility_text <- "This chart shows the percentage of people in prison who
  are past their parole eligibility year, with projections highlighted in red."

  # Create Highcharts object
  highchart() |>
    hc_chart(type = "line") |>
    hc_title(text = title) |>
    hc_xAxis(categories = all_years, lineWidth = 1) |>
    hc_yAxis(
      title = list(text = "Percent Past Parole Eligibility"),
      min = 0, max = 100, # Define Y-axis range (0–100%)
      labels = list(format = "{value}%") # Add percentage format to Y-axis labels
    ) |>
    hc_add_series(
      name = "Past Parole Eligibility",
      data = round(df1$pct_past_pe, 0), # Add observed data
      color = teal, # Set line color
      marker = list(enabled = TRUE), # Enable markers on data points
      connectNulls = FALSE, # Do not connect lines for missing values
      tooltip = list(valueSuffix = "%") # Tooltip showing percentage
    ) |>
    hc_add_series(
      name = "Projected Past Parole Eligibility",
      data = round(df1$proj_pct_past_pe, 0), # Add projected data
      color = red, # Set line color for projections
      marker = list(enabled = TRUE),
      connectNulls = FALSE, # Do not connect lines for missing values
      tooltip = list(valueSuffix = "%")
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = TRUE) |>
    hc_exporting(
      enabled = TRUE,
      filename = paste0(gsub(" ", "_", tolower(title)), "_") # Set export file name
    ) |>
    hc_caption(text = ncrp_csg_source) |> # Add source caption
    fnc_add_hc_accessibility(hc_accessibility_text) # Add accessibility text
})

# Assign state names to the generated charts
all_line_pop_pe_by_year <- setNames(all_line_pop_pe_by_year, states)
all_line_pop_pe_by_year <- setNames(all_line_pop_pe_by_year, states)
all_line_pop_pe_by_year$Georgia
all_line_pop_pe_by_year$Colorado
all_line_pop_pe_by_year$Idaho
all_line_pop_pe_by_year$Hawaii
