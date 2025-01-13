fnc_hc_pie_chart_new_ <- function(df, variable, source1 = ncrp_source, source2 = csg_source) {
  # Get unique states from the data
  states <- unique(df$state)

  # Iterate over each state to generate pie charts
  all_pie_charts <- map(states, function(state_name) {
    # Filter the data for the current state
    df1 <- df |>
      ungroup() |> # Remove grouping to ensure accurate filtering
      filter(state == state_name) |> # Select data for the current state
      mutate(color = case_when( # Assign colors based on parole eligibility status
        parelig_status_new == "Will Be Eligible In 1+ Year" ~ color2,
        parelig_status_new == "Will Be Eligible Next Year" ~ color5,
        parelig_status_new == "Missing" ~ darkgray,
        parelig_status_new == "Past Parole Eligibility at End of Year" ~ color4
      ))

    # Extract the reporting year for the current state (assumes it's consistent within the state)
    year <- unique(df1$rptyear)

    # Generate descriptive accessibility text for the pie chart
    category_counts <- df1 |>
      group_by(!!sym(variable)) |> # Group by the specified variable
      # Calculate percentage for each category
      summarise(percentage = round(sum(n) / sum(df1$n) * 100, 0)) |>
      arrange(desc(percentage)) # Sort categories by descending percentage

    # Build a textual description of the chart for accessibility
    accessibility_text <- paste(
      "This pie chart shows the distribution of the prison population by", variable, "in", year, ".",
      paste(
        category_counts |>
          # Combine category and percentage
          transmute(text = paste0(!!sym(variable), ": ", percentage, "%")) |>
          pull(text), # Extract the formatted text
        collapse = ", " # Join all categories into a single string
      )
    )

    # Create the Highcharts pie chart
    highchart() |>
      hc_chart(type = "pie") |>
      hc_plotOptions(pie = list(
        dataLabels = list( # Define label formatting for the chart
          enabled = TRUE,
          format = '<span style="font-size:1em; font-weight:normal">{point.name}: </span>
          <br><span style="font-size:2em; font-weight:normal">{point.percentage:.0f}%</span>'
        ),
        # Use custom colors defined in the data
        colorByPoint = FALSE
      )) |>
      hc_series(list(
        # Add data to the chart
        data = list_parse(df1 |> mutate(y = n) |> transmute(
          name = !!sym(variable), y, color, tooltip
        ))
      )) |>
      hc_add_theme(base_hc_theme) |> # Add a base theme
      hc_tooltip(formatter = JS("function () { return this.point.tooltip; }")) |>
      hc_title(text = "Prison Population by Parole Eligibility Status") |>
      hc_exporting(enabled = TRUE, filename = paste0("prison_population_", state_name, "_", year)) |>
      hc_caption(text = paste0(source1, ", ", year, " and ", source2)) |> # Add chart caption with source information
      fnc_add_hc_accessibility(accessibility_text) # Function to add accessibility text
  })

  # Assign state names to the charts list for clarity
  all_pie_charts <- setNames(all_pie_charts, states)

  return(all_pie_charts)
}


# ---------------------------------------------------------------------------- #
# Pie charts of the prison population by parole eligibility status
# ---------------------------------------------------------------------------- #

# Filter the prison population data based on specified criteria
# The function filters to include only:
#   - People in prison for new crimes with sentence lengths of 1+ years (except life)
#   - States with active parole systems and low missingness (not in `states_to_exclude`)
#   - States that don't require filtering for admission type or sentence length (`states_nofilter`)
ncrp_yearendpop_filtered <- fnc_filter_pe_population_criteria(data = ncrp_yearendpop_consolidated,
                                                              exclude = states_to_exclude,
                                                              dont_filter = states_nofilter)

# Calculate the total prison population by state and reporting year
# This serves as the denominator for proportion calculations later
total_pe_pop_by_rptyear <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  summarise(yearendpop = n(), .groups = "drop")

# Compute the prison population proportions by parole eligibility status
# Includes statuses: "Missing," "Current," or "Future"
# Joins with the total population to calculate percentages (`prop`) and adds tooltips
pe_status_pop <- ncrp_yearendpop_filtered |>

  mutate(parelig_status_new =
           case_when(
              parelig_status == "Current" ~ "Past Parole Eligibility at End of Year",
              parelig_status == "Future" & time_between_ped_rptyear == 1 ~ "Will Be Eligible Next Year",
              parelig_status == "Future" & time_between_ped_rptyear > 1 ~ "Will Be Eligible In 1+ Years",
              TRUE ~ parelig_status),
         parelig_status_new = factor(parelig_status_new,
                                     levels = c(
                                       "Past Parole Eligibility at End of Year",
                                       "Will Be Eligible Next Year",
                                       "Will Be Eligible In 1+ Years",
                                       "Missing"
                                     ))
    ) |>
  group_by(state, rptyear) |>
  count(parelig_status_new) |>
  left_join(total_pe_pop_by_rptyear, by = c("state", "rptyear")) |>
  mutate(prop = (n / yearendpop) * 100) |> # Calculate proportion
  fnc_create_tooltip(variable_label = "Parole Eligibility Status", variable = parelig_status_new) |> # Add tooltips
  fnc_filter_by_year(which_overall_year) # Filter data based on the best year for each state

# Generate pie charts visualizing parole eligibility status proportions for each state
# `fnc_hc_pie_chart_new` creates individual charts with data and accessibility text for each state
all_pie_pe_type <- fnc_hc_pie_chart_new(
  df = pe_status_pop,
  variable = "parelig_status_new"
)

# State example:
all_pie_pe_type$Georgia
all_pie_pe_type$Michigan

# Generate summary sentences for each state describing parole eligibility proportions
#  "Most recent data shows that 69 percent of people in prison were eligible for
#   parole and incarcerated past parole eligibility at the end of the year, while
#   another 31 will reach their parole eligibility next year."
all_sentence_pe_type <- {
  # Get the list of unique states from the filtered data
  states <- unique(pe_status_pop$state)

  # Use `map` to iterate over each state and generate a summary sentence
  map(states, function(state_name) {
    # Filter the data for the current state
    df <- pe_status_pop |> filter(state == state_name)

    # Extract the reporting year for the current state (assumes consistency across rows)
    year <- unique(df$rptyear)

    # Get proportions of people currently eligible and those eligible in the future
    current_prop <- df |> filter(parelig_status_new == "Past Parole Eligibility at End of Year") |> pull(prop)
    future_prop <- df |> filter(parelig_status_new == "Will Be Eligible Next Year") |> pull(prop)

    # Construct the summary sentence for the state
    paste0(
      "Most recent data shows that ",
      round(current_prop, 0),
      " percent of people in prison were eligible for parole and incarcerated ",
      "past parole eligibility at the end of the year,",
      " while another ", round(future_prop, 0),
      " were expected to reach their parole eligibility in the following year."
    )
  }) |> setNames(states) # Assign state names to the generated sentences
}

# State example:
all_sentence_pe_type$Georgia

