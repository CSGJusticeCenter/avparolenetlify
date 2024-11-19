
# Helper function to filter data by state and year
fnc_filter_data_by_state_year <- function(df, state_var) {
  year <- df |>
    filter(state == state_var) |>
    pull(rptyear) |>
    max(na.rm = TRUE)

  df_filtered <- df |>
    ungroup() |>
    filter(state == state_var) |>
    filter(rptyear == year)

  list(data = df_filtered, year = year)
}

# Helper function to generate sentences based on sex comparisons
fnc_generate_sentence_sex <- function(df1, year, type, los_col, state_var) {
  # Focus on comparisons with males
  df_male <- df1 |> dplyr::filter(sex == "Male")

  # Initialize an empty sentence variable
  sentence <- ""

  # Generate sentence for female vs male comparison
  df_female <- df1 |> dplyr::filter(sex == "Female")
  if (nrow(df_female) > 0 && nrow(df_male) > 0) {
    los_diff_female <- round(df_female[[los_col]], 1) - round(df_male[[los_col]], 1)
    abs_los_diff_female <- abs(los_diff_female)

    if (!is.na(los_diff_female)) {
      if (los_diff_female > 0) {
        sentence <- paste0("In ", year, ", females ",
                           if (type == "in prison") "released" else "who were still incarcerated",
                           " spent on average ", abs_los_diff_female,
                           if (abs_los_diff_female == 1) " more year" else " more years",
                           " ", if (type == "in prison") "in prison" else "past parole eligibility",
                           " compared to males in ", state_var, ".")
      } else if (los_diff_female < 0) {
        sentence <- paste0("In ", year, ", females ",
                           if (type == "in prison") "released" else "who were still incarcerated",
                           " spent on average ", abs_los_diff_female,
                           if (abs_los_diff_female == 1) " less year" else " less years",
                           " ", if (type == "in prison") "in prison" else "past parole eligibility",
                           " compared to males in ", state_var, ".")
      }
    }
  }

  if (sentence != "") {
    return(sentence)
  } else {
    return(paste0("Females and males spent the same average number of years ",
                  if (type == "in prison") "in prison." else "past parole eligibility."))
  }
}

# Main function to generate disparity sentences
fnc_generate_disparity_sentences <- function(df, type, compare_var, los_col) {

  # Get unique states to iterate over
  states <- unique(df$state)

  # Generate sentence for each state
  all_sentences <- purrr::map(.x = states, .f = function(state_var) {

    # Use helper function to filter data by state and year
    filtered_data <- fnc_filter_data_by_state_year(df, state_var)
    df1 <- filtered_data$data
    year <- filtered_data$year

    # Handle missing data for the state
    if (nrow(df1) == 0) {
      return(paste0("No data available for ", state_var))
    }

    # Check for the comparison variable ("sex" or "race")
    if (compare_var == "sex") {
      # Use the helper function to generate the sentence for sex comparison
      return(fnc_generate_sentence_sex(df1, year, type, los_col, state_var))

    } else if (compare_var == "race") {

      # Logic for race comparison remains the same
      df1 <- df1 |>
        dplyr::mutate(race = dplyr::case_when(
          race == "White, non-Hispanic" ~ "White",
          race == "Black, non-Hispanic" ~ "Black",
          race == "Hispanic, any race" ~ "Hispanic",
          race == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races"
        ))

      # Focus on comparisons with White individuals
      df_white <- df1 |> dplyr::filter(race == "White")

      # Initialize variables to hold sentences for each race comparison
      black_sentence <- ""
      hispanic_sentence <- ""
      other_sentence <- ""

      # Generate sentence for Black vs White comparison
      df_black <- df1 |> dplyr::filter(race == "Black")
      if (nrow(df_black) > 0 && nrow(df_white) > 0) {
        los_diff_black <- round(df_black[[los_col]], 1) - round(df_white[[los_col]], 1)
        abs_los_diff_black <- round(abs(los_diff_black), 1)

        if (!is.na(los_diff_black)) {
          if (los_diff_black > 0) {
            black_sentence <- paste0("Black people ",
                                     if (type == "in prison") "released" else "who were still incarcerated",
                                     " spent on average ", abs_los_diff_black,
                                     " more years ", if (type == "in prison") "in prison" else "past parole eligibility")
          } else if (los_diff_black < 0) {
            black_sentence <- paste0("Black people ",
                                     if (type == "in prison") "released" else "who were still incarcerated",
                                     " spent on average ", abs_los_diff_black,
                                     if (abs_los_diff_black == 1) " less year" else " less years",
                                     " ", if (type == "in prison") "in prison" else "past parole eligibility")
          }
        }
      }

      # Generate sentence for Hispanic vs White comparison
      df_hispanic <- df1 |> dplyr::filter(race == "Hispanic")
      if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
        los_diff_hispanic <- round(df_hispanic[[los_col]], 1) - round(df_white[[los_col]], 1)
        abs_los_diff_hispanic <- round(abs(los_diff_hispanic), 1)

        if (!is.na(los_diff_hispanic)) {
          if (los_diff_hispanic > 0) {
            hispanic_sentence <- paste0("Hispanic people ",
                                        if (type == "in prison") "released" else "who were still incarcerated",
                                        " spent on average ", abs_los_diff_hispanic,
                                        " more years ", if (type == "in prison") "in prison" else "past parole eligibility")
          } else if (los_diff_hispanic < 0) {
            hispanic_sentence <- paste0("Hispanic people ",
                                        if (type == "in prison") "released" else "who were still incarcerated",
                                        " spent on average ", abs_los_diff_hispanic,
                                        if (abs_los_diff_hispanic == 1) " less year" else " less years",
                                        " ", if (type == "in prison") "in prison" else "past parole eligibility")
          }
        }
      }

      # Generate sentence for Other races vs White comparison
      df_other <- df1 |> dplyr::filter(race == "non-Hispanic people of other races")
      if (nrow(df_other) > 0 && nrow(df_white) > 0) {
        los_diff_other <- round(df_other[[los_col]], 1) - round(df_white[[los_col]], 1)
        abs_los_diff_other <- round(abs(los_diff_other), 1)

        if (!is.na(los_diff_other)) {
          if (los_diff_other > 0) {
            other_sentence <- paste0("non-Hispanic people of other races ",
                                     if (type == "in prison") "released" else "who were still incarcerated",
                                     " spent on average ", abs_los_diff_other,
                                     " more years ", if (type == "in prison") "in prison" else "past parole eligibility")
          } else if (los_diff_other < 0) {
            other_sentence <- paste0("non-Hispanic people of other races ",
                                     if (type == "in prison") "released" else "who were still incarcerated",
                                     " spent on average ", abs_los_diff_other,
                                     if (abs_los_diff_other == 1) " less year" else " less years",
                                     " ", if (type == "in prison") "in prison" else "past parole eligibility")
          }
        }
      }

      # Combine sentences or indicate no significant differences
      sentences <- c(black_sentence, hispanic_sentence, other_sentence)
      sentences <- sentences[sentences != ""]
      if (length(sentences) > 0) {
        return(paste0("In ", year, ", ", paste(sentences, collapse = ", and "), " compared to White people."))
      } else {
        return("No significant differences in average years spent compared to White people.")
      }

    } else {
      return("Invalid comparison variable.")
    }
  })

  # Assign state names to list
  all_sentences <- setNames(all_sentences, states)

  return(all_sentences)
}

# Function to create lollipop chart with fixed colors and labels
fnc_create_lollipop_chart <- function(df, group_var, state_name, height = 200, source = ncrp_csg_source) {

  # Define consistent group labels, colors, and shapes
  if (group_var == "sex") {
    group_labels <- c("Male", "Female")
    colors <- c(teal, purple)  # Colors for male and female
    shapes <- c("circle", "triangle")  # Shapes for male and female
  } else {
    group_labels <- c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race", "Other race(s), non-Hispanic")
    colors <- c(red, teal, blue, purple)  # Colors for race groups
    shapes <- c("square", "circle", "diamond", "triangle")  # Shapes for race groups
  }

  # Filter data for the specified state
  df1 <- df |>
    ungroup() |>
    filter(state == state_name) |>
    arrange(desc(average_los)) |>
    mutate(group_num = row_number(),
           color = case_when(
             !!sym(group_var) == group_labels[1] ~ colors[1],
             !!sym(group_var) == group_labels[2] ~ colors[2],
             !!sym(group_var) == group_labels[3] ~ colors[3],
             !!sym(group_var) == group_labels[4] ~ colors[4]
           ))

  year <- unique(df1$rptyear)

  # Determine the title based on the group_var
  chart_title <- if (group_var == "sex") {
    paste("Average Time Served by Sex,", year)
  } else if (group_var == "race") {
    paste("Average Time Served by Race and Ethnicity,", year)
  } else {
    paste("Average Time Served by", group_var, ",", year)
  }

  # Generate accessibility text based on the data
  accessibility_text <- paste0("The chart below shows the average time served for different ",
                               group_var, " groups in ", state_name, ". ",
                               group_labels[1], " spent on average ", df1$average_los[df1$group_num == 1],
                               " years, followed by ", group_labels[2], " with ", df1$average_los[df1$group_num == 2],
                               " years, ", group_labels[3], " with ", df1$average_los[df1$group_num == 3],
                               " years, and ", group_labels[4], " with ", df1$average_los[df1$group_num == 4],
                               " years.")

  max_los <- max(df1$average_los, na.rm = TRUE)

  # Create a named list for y-axis labels
  y_labels <- as.list(setNames(as.character(df1[[group_var]]), df1$group_num))

  # Create the dataframe for lines in the lollipop chart
  df_lines <- df1 |>
    mutate(start_x = 0, end_x = average_los) |>
    select(group_num, start_x, end_x, !!sym(group_var))

  # Reshape data for highcharter
  df_lines <- df_lines |>
    gather(key = "point", value = "value", start_x, end_x)

  # Initialize the highchart object
  highcharts <- highchart() |>
    hc_title(text = chart_title) |>
    hc_add_series(
      df_lines,
      type = 'line',
      hcaes(x = value, y = group_num, group = !!sym(group_var)),
      lineWidth = 1,
      color = "black",
      dashStyle = "solid",
      opacity = 1,
      marker = list(enabled = FALSE),
      enableMouseTracking = FALSE,
      showInLegend = FALSE
    )

  # Add scatter series for each group with appropriate marker symbols
  for (i in seq_along(group_labels)) {
    highcharts <- highcharts |>
      hc_add_series(
        df1 %>% filter(!!sym(group_var) == group_labels[i]),
        type = 'scatter',
        color = colors[i],
        hcaes(x = average_los, y = group_num, group = !!sym(group_var), name = !!sym(group_var)),
        marker = list(
          radius = 5,
          symbol = shapes[i]  # Use unique shape for each group
        ),
        dataLabels = list(
          enabled = TRUE,
          format = '{point.x:.1f} Years',
          align = "left",
          y = 9,
          x = 8,
          style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
        )
      )
  }

  # Add y-axis and x-axis customizations
  highcharts <- highcharts |>
    hc_add_theme(base_hc_theme) |>
    hc_yAxis(
      labels = list(
        enabled = TRUE,
        style = list(
          color = 'black',
          fontWeight = "regular",
          fontSize = "12px"
        )
      ),
      title = list(text = ""),
      majorGridLineColor = "transparent",
      gridLineColor = "transparent",
      lineColor = "transparent",
      tickColor = "white",
      categories = y_labels
    ) |>
    hc_xAxis(
      title = list(text = ""),
      labels = list(enabled = FALSE),
      lineColor = "transparent",
      tickLength = 0,
      gridLineColor = "transparent",
      tickColor = "transparent",
      max = max_los * 1.5
    ) |>
    hc_exporting(enabled = FALSE) |>
    hc_tooltip(enabled = FALSE) |>
    hc_legend(enabled = FALSE) |>
    hc_size(height = height) |>
    fnc_add_hc_accessibility(accessibility_text) |>
    hc_caption(text = source)

  return(highcharts)
}

# Function to generate lollipop charts dynamically without needing to specify colors and labels
fnc_generate_lollipop_charts <- function(df, compare_var, height = 200) {

  # Get unique states to iterate over
  states <- unique(df$state)

  # Generate lollipop chart for each state
  all_charts <- purrr::map(.x = states, .f = function(state_var) {

    # Create the lollipop chart for the state
    fnc_create_lollipop_chart(
      df = df,
      group_var = compare_var,
      state_name = state_var,
      source = ncrp_source,
      height = height
    )
  })

  # Assign state names to the list of charts
  all_charts <- setNames(all_charts, states)

  return(all_charts)
}

fnc_generate_offense_disparity_sentence <- function(data, grouping_var = "race", time_var = "average_los", which_year) {
  # Get unique states to iterate over
  states <- unique(data$state)

  # Generate sentence for each state
  all_sentences <- purrr::map(.x = states, .f = function(x) {

    df1 <- data |>
      dplyr::filter(state == x & fbi_index != "Other or Unspecified")

    year <- unique(df1$rptyear)

    # Handling missing data
    if (nrow(df1) == 0) {
      return(paste0("No data available for ", x))
    }

    # Calculate the difference in average LOS between groups for each offense type
    df_disparity <- df1 %>%
      dplyr::group_by(fbi_index) %>%
      dplyr::reframe(
        max_los = max(!!rlang::sym(time_var)),
        min_los = min(!!rlang::sym(time_var)),
        diff_los = max_los - min_los,
        group_longest = .data[[grouping_var]][which.max(!!rlang::sym(time_var))],
        group_shortest = .data[[grouping_var]][which.min(!!rlang::sym(time_var))]
      ) %>%
      dplyr::arrange(dplyr::desc(diff_los))

    # Adjust group names for race and ethnicity or sex, depending on grouping_var
    if (grouping_var == "race") {
      df_disparity <- df_disparity %>%
        dplyr::mutate(
          group_longest = dplyr::case_when(
            group_longest == "Black, non-Hispanic" ~ "Black",
            group_longest == "White, non-Hispanic" ~ "White",
            group_longest == "Hispanic, any race" ~ "Hispanic",
            group_longest == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races",
            TRUE ~ group_longest
          ),
          group_shortest = dplyr::case_when(
            group_shortest == "Black, non-Hispanic" ~ "Black",
            group_shortest == "White, non-Hispanic" ~ "White",
            group_shortest == "Hispanic, any race" ~ "Hispanic",
            group_shortest == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races",
            TRUE ~ group_shortest
          )
        )
    }

    # Filter for comparisons: Black, Hispanic, or non-Hispanic people of other races vs. White
    if (grouping_var == "race") {
      df_disparity_filtered <- df_disparity %>%
        dplyr::filter(group_shortest == "White" & group_longest %in% c("Black", "Hispanic", "non-Hispanic people of other races"))
    } else {
      df_disparity_filtered <- df_disparity %>%
        dplyr::filter(group_shortest == "Female" & group_longest == "Male") %>%
        dplyr::mutate(
          group_longest = "males",
          group_shortest = "females"
        )
    }

    # If no disparities exist, return default message
    if (nrow(df_disparity_filtered) == 0) {
      time_description <- ifelse(time_var == "time_served", "time served in prison", "time spent in prison past parole eligibility")
      return(paste0("The chart below shows the average ", time_description, " by offense type and ",
                    ifelse(grouping_var == "race", "race and ethnicity", grouping_var), " in 2020."))
    }

    # Remove "Other Violent Offenses" if it has the largest disparity
    if (df_disparity_filtered$fbi_index[1] == "Other Violent Offenses" & nrow(df_disparity_filtered) > 1) {
      df_disparity_filtered <- df_disparity_filtered %>% dplyr::slice(2)
    }

    # Get the largest remaining disparity
    largest_disparity <- df_disparity_filtered %>% dplyr::slice(1)

    # Extract values for the sentence
    offense_type <- largest_disparity$fbi_index
    group_longest <- largest_disparity$group_longest
    los_longest <- round(largest_disparity$max_los, 1)
    group_shortest <- largest_disparity$group_shortest
    los_shortest <- round(largest_disparity$min_los, 1)
    disparity_diff <- round(largest_disparity$diff_los, 1)

    # Construct the sentence
    time_description <- ifelse(time_var == "average_los", "time served in prison", "time spent in prison past parole eligibility")
    sentence <- paste0(
      "The chart below shows the average ", time_description, " by offense type and ",
      ifelse(grouping_var == "race", "race and ethnicity", grouping_var), " in ", year, ". ",
      "The largest disparity was observed among ", tolower(offense_type), " offenses, where ",
      group_longest, if (grouping_var == "race" && group_longest != "White") " people" else "",  # Add "people" for race labels other than "White"
      " spent on average ", disparity_diff, " more years in prison compared to ",
      group_shortest, if (grouping_var == "race") " people" else "", "."
    )

    return(sentence)
  })

  # Assign state names to list
  all_sentences <- setNames(all_sentences, states)

  return(all_sentences)
}

fnc_create_scatter_charts_by_state <- function(df, group_var, measure, source = ncrp_csg_source) {

  # Get unique states to iterate over
  states <- unique(df$state)

  # Iterate over each state to generate charts
  all_charts <- purrr::map(.x = states, .f = function(state_name) {

    # Define consistent group labels, colors, and shapes dynamically
    if (group_var == "sex") {
      group_labels <- c("Male", "Female")
      colors <- c(teal, purple)  # Colors for male and female
      shapes <- c("circle", "triangle")  # Shapes for male and female
    } else {
      group_labels <- c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race", "Other race(s), non-Hispanic")
      colors <- c(red, teal, blue, purple)  # Colors for race groups
      shapes <- c("square", "circle", "diamond", "triangle")  # Shapes for race groups
    }

    # Filter data for the specified state
    df1 <- df |>
      ungroup() |>
      filter(state == state_name) |>
      arrange(desc(!!sym(measure))) |>
      mutate(group_num = row_number(),
             color = case_when(
               !!sym(group_var) == group_labels[1] ~ colors[1],
               !!sym(group_var) == group_labels[2] ~ colors[2],
               !!sym(group_var) == group_labels[3] ~ colors[3],
               !!sym(group_var) == group_labels[4] ~ colors[4]
             ))

    year <- unique(df1$rptyear)

    # Define titles and labels based on the measure
    x_axis_title <- ifelse(measure == "average_los", "Average Time Served (Years)", "Average Years Past Parole Eligibility")
    chart_title <- paste0("Average ", ifelse(measure == "average_los", "Time Served", "Years Past Parole Eligibility"),
                          " by Offense and ", ifelse(group_var == "sex", "Gender", "Race and Ethnicity"), ", ", year)

    # Generate accessibility text
    accessibility_measure <- ifelse(measure == "average_los", "average length of stay", "average years past parole eligibility")
    accessibility_text <- paste0("The chart shows the ", accessibility_measure, " for different ",
                                 group_var, " groups in ", state_name, ". ", group_labels[1],
                                 " spent an average of ", df1[[measure]][df1$group_num == 1],
                                 " years, followed by ", group_labels[2], " with ",
                                 df1[[measure]][df1$group_num == 2], " years, and ",
                                 group_labels[3], " with ", df1[[measure]][df1$group_num == 3], " years.")

    max_los <- max(df1[[measure]], na.rm = TRUE)

    # Create a named list for y-axis labels
    y_labels <- setNames(as.list(unique(as.character(df1$fbi_index))),
                         unique(as.numeric(as.factor(df1$fbi_index))))

    # Initialize the highchart object
    highcharts <- highchart() |>
      hc_title(text = chart_title) |>
      hc_yAxis(
        title = list(text = ""),
        labels = list(enabled = TRUE, style = list(color = "black")),
        categories = y_labels,
        gridLineColor = "transparent"
      ) |>
      hc_xAxis(
        title = list(text = x_axis_title, style = list(color = "black")),
        labels = list(style = list(color = "black")),
        gridLineDashStyle = "Dash",  # Add dashed grid lines
        gridLineWidth = 1,           # Ensure grid lines are visible
        gridLineColor = "lightgray",  # Set grid line color
        tickLength = 0
      ) |>
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS("function() {
    return '<b>' + this.series.name + '</b><br/>' +
           'Offense: ' + (this.point.fbi_index || 'Unknown') + '<br/>' +
           'Average Years: ' + this.point.x.toFixed(1) + '<br/>' +
           'People: ' + (this.point.people ? this.point.people.toLocaleString() : 'N/A');
  }")
      ) |>
      hc_legend(layout = "horizontal", verticalAlign = "top") |>
      hc_caption(text = source) |>
      hc_add_theme(base_hc_theme) |>
      fnc_add_hc_accessibility(accessibility_text)

    # Add scatter series for each group with appropriate marker symbols
    for (i in seq_along(group_labels)) {
      highcharts <- highcharts |>
        hc_add_series(
          df1 %>% filter(!!sym(group_var) == group_labels[i]),
          type = 'scatter',
          color = colors[i],
          hcaes(x = !!sym(measure), y = as.numeric(factor(fbi_index)), group = !!sym(group_var)),
          marker = list(symbol = shapes[i], radius = 5)
        )
    }

    return(highcharts)
  })

  # Assign state names to the charts list
  all_charts <- setNames(all_charts, states)

  return(all_charts)
}






#-------------------------------------------------------------------------------
# PEOPLE INFOGRAPHICS
#-------------------------------------------------------------------------------

#' @title Blank Out Plot Theme
#' @description This function sets up a theme for blanking out plot elements like axes, scales, and legends.
#' @return A list of ggplot2 theme and scale elements for use in plots.
#' @export
fnc_blankitout <- function(){
  list(
    theme_void(),  # Removes background and gridlines for a clean appearance.
    scale_x_continuous(expand = expansion(mult = ex_w, add = 0)),  # Customizes x-axis scale expansion.
    scale_y_continuous(expand = expansion(mult = ex_h, add = 0)),  # Customizes y-axis scale expansion.
    theme(legend.position = "none", aspect.ratio = img_ar_hw)  # Removes legend and sets the aspect ratio for the plot.
  )
}

#' @title Icon Options for Plotting
#' @description Generates a list of plot options based on a partial fill value, colors, and a background for creating icon-based infographics.
#' @param partialval A numeric value between 0 and 1 representing the percentage of the partial icon fill.
#' @param empty Color for the empty part of the icon.
#' @param fill Color for the full part of the icon.
#' @param partial Color for the partially filled icon.
#' @param bg Background color for the icons.
#' @param fillHoriz A logical flag indicating whether to fill icons horizontally (TRUE) or vertically (FALSE).
#' @return A list of ggplot objects representing empty, full, and partial icon states.
#' @export
fnc_icon_options <- function(partialval, empty = "#FFFFFF", fill = dark_color, partial = light_color, bg = "#FFFFFF", fillHoriz = FALSE) {
  # Ensure partialval is within valid range
  if (partialval < 0 | partialval >= 1) stop("partialval must be between 0 and 1")

  # Define color sets for different states of the icon (empty, full, partial)
  cols_lst <- list(
    "empty" = c(bg, empty),
    "full" = c(bg, fill),
    "partial" = c(bg, partial, fill)
  )

  # Define percentage fills for each icon state
  pcts_lst <- list(
    "empty" = 0,
    "full" = 100,
    "partial" = partialval * 100
  )

  # Initialize the plot list to store generated plots for each state
  plot_lst <- list("empty" = NULL, "full" = NULL, "partial" = NULL)

  # Determine the boundaries for filling either horizontally or vertically
  if (fillHoriz == FALSE) {
    pos1 <- which(apply(img[,,1], 2, function(y) any(y == 1)))  # Determine filled vertical range
    max <- max(pos1)
  } else {
    pos1 <- which(apply(img[,,1], 1, function(y) any(y == 1)))  # Determine filled horizontal range
    max <- max(pos1)
  }
  h <- dim(img)[1]  # Icon height
  w <- dim(img)[2]  # Icon width
  min <- min(pos1)

  # Loop through each icon state and generate corresponding plot
  for (j in names(plot_lst)) {
    pcts <- pcts_lst[[j]]  # Get the fill percentage for the current state
    pospct <- round((max - min) * pcts / 100 + min)  # Calculate the fill position based on percentage
    finalimg <- img[h:1,,1]  # Flip the image vertically for correct orientation
    bkgr <- (finalimg == 1)  # Background mask
    colfill <- matrix(rep(FALSE, h*w), nrow = h)  # Initialize fill matrix

    # Apply the fill either horizontally or vertically
    if (fillHoriz == FALSE) {
      colfill[1:h, max:pospct] <- TRUE
    } else {
      colfill[max:pospct, 1:w] <- TRUE
    }

    # Assign partially filled cells in the image
    finalimg[bkgr & colfill] <- 0.5
    df <- reshape2::melt(finalimg)  # Convert matrix to long format for plotting

    # Remove partial fill for the 'full' state
    if (j == "full") {
      df[df$value == 0.5, ] <- 0
    }

    # Create the ggplot for each icon state
    plot <- ggplot(df, aes(x = Var2, y = Var1, fill = factor(value))) +
      geom_raster() +
      scale_fill_manual(values = cols_lst[[j]]) +  # Apply the corresponding color scheme
      fnc_blankitout()  # Apply the blank theme

    plot_lst[[j]] <- plot  # Store the plot in the list
  }

  return(plot_lst)  # Return the list of generated plots
}

#' @title Create Icon Infographic
#' @description Generates an infographic representing a Relative Rate Index (RRI) using icons to indicate full, partial, and empty states.
#' @param rri_raw The raw Relative Rate Index value.
#' @param rri_digits Number of decimal places to round the RRI value.
#' @param fillcolor Color to fill the full icons.
#' @param partialcolor Color to fill partially filled icons.
#' @param emptyhumans Logical flag to indicate whether empty icons should be included.
#' @param emptycolor Color for the empty icons.
#' @param infogs Total number of icons to display.
#' @param infogs_ncol Number of columns for the grid layout of icons.
#' @param fillHoriz Logical flag to determine if icons should be filled horizontally or vertically.
#' @return A ggplot2 object representing the generated icon infographic.
#' @export
fnc_create_icons <- function(rri_raw, rri_digits = 1, fillcolor = dark_color, partialcolor = light_color,
                             emptyhumans = TRUE, emptycolor = "white", infogs = default_ncols,
                             infogs_ncol = default_ncols, fillHoriz = FALSE) {

  # Round the RRI value and compute full and partial icons
  RRI <- round(rri_raw, digits = rri_digits)
  numfull <- floor(RRI)  # Number of fully filled icons
  numremain <- RRI - numfull  # Portion of the partial icon

  # Generate plot options for full, partial, and empty icons
  plot_opts <- fnc_icon_options(partialval = numremain, empty = emptycolor, fill = fillcolor, partial = partialcolor, fillHoriz = fillHoriz)

  plot_list <- list()  # Initialize list for storing plots

  # Create full and partial icons based on RRI value
  if (RRI > 1 & numremain != 0) {
    for (i in 1:numfull) {
      plot_list[[i]] <- plot_opts$full
    }
    plot_list[[numfull + 1]] <- plot_opts$partial
  } else if (RRI > 1 & numremain == 0) {
    for (i in 1:numfull) {
      plot_list[[i]] <- plot_opts$full
    }
  } else if (RRI == 1) {
    plot_list[[1]] <- plot_opts$full
  } else if (RRI < 1) {
    plot_list[[1]] <- plot_opts$partial
  }

  # Add empty icons if needed
  if (emptyhumans == TRUE & length(plot_list) != infogs) {
    st_empty <- ifelse(numremain != 0, numfull + 2, numfull + 1)
    for (i in st_empty:infogs) {
      plot_list[[i]] <- plot_opts$empty
    }
  }

  # Determine the number of rows for the icon grid
  rows <- ifelse(infogs > infogs_ncol, ceiling(rri_raw / infogs_ncol), 1)

  # Return the grid of icon plots
  plot_grid(plotlist = plot_list, nrow = rows)
}

#' @title Create Infographic with Icons and RRI Label
#' @description Combines a label of the RRI value with a grid of icons representing the RRI.
#' @param rri_raw The raw Relative Rate Index value.
#' @param infographic_color The color for the infographic elements.
#' @return A ggplot object combining the RRI label and the icon grid.
#' @export
fnc_create_infographic <- function(rri_raw, infographic_color) {

  # Round the RRI value and format as a text label
  rri_text <- paste0(round(rri_raw, digits = 1), "x")

  # Generate the icons for the infographic
  ggtemp_justpeople <- fnc_create_icons(
    rri_raw = rri_raw,
    infogs = default_ncols,
    infogs_ncol = default_ncols,
    fillcolor = infographic_color,
    partialcolor = light_color,
    emptyhumans = TRUE,
    emptycolor = "white",
    fillHoriz = FALSE
  )

  # Create the plot for displaying the RRI text label
  rri_label_plot <- ggplot() +
    annotate("text", x = 1, y = 1, label = rri_text, size = 12, hjust = 0.5,
             fontface = "bold",
             color = infographic_color,
             family = "Graphik") +
    theme_void()

  # Combine the RRI label plot with the icon grid
  final_plot <- plot_grid(
    rri_label_plot, ggtemp_justpeople,
    nrow = 1, rel_widths = c(1, 6)  # Adjust widths to balance the label and icons
  )

  print(final_plot)  # Display the final infographic plot
}




















#' @title Icon Options for Plotting
#' @description Generates a list of plot options based on a partial fill value, colors, and a background for creating icon-based infographics.
#' @param partialval A numeric value between 0 and 1 representing the percentage of the partial icon fill.
#' @param empty Color for the empty part of the icon.
#' @param fill Color for the full part of the icon.
#' @param partial Color for the partially filled icon.
#' @param bg Background color for the icons.
#' @param fillHoriz A logical flag indicating whether to fill icons horizontally (TRUE) or vertically (FALSE).
#' @return A list of ggplot objects representing empty, full, and partial icon states.
#' @export
fnc_icon_options_homepage <- function(partialval, empty = "#FFFFFF", fill = dark_color, partial = light_color, bg = "#FFFFFF", fillHoriz = FALSE) {
  # Ensure partialval is within valid range
  if (partialval < 0 | partialval >= 1) stop("partialval must be between 0 and 1")

  # Define color sets for different states of the icon (empty, full, partial)
  cols_lst <- list(
    "empty" = c(bg, empty),
    "full" = c(bg, fill),
    "partial" = c(bg, partial, fill)
  )

  # Define percentage fills for each icon state
  pcts_lst <- list(
    "empty" = 0,
    "full" = 100,
    "partial" = partialval * 100
  )

  # Initialize the plot list to store generated plots for each state
  plot_lst <- list("empty" = NULL, "full" = NULL, "partial" = NULL)

  # Determine the boundaries for filling either horizontally or vertically
  if (fillHoriz == FALSE) {
    pos1 <- which(apply(img[,,1], 2, function(y) any(y == 1)))  # Determine filled vertical range
    max <- max(pos1)
  } else {
    pos1 <- which(apply(img[,,1], 1, function(y) any(y == 1)))  # Determine filled horizontal range
    max <- max(pos1)
  }
  h <- dim(img)[1]  # Icon height
  w <- dim(img)[2]  # Icon width
  min <- min(pos1)

  # Loop through each icon state and generate corresponding plot
  for (j in names(plot_lst)) {
    pcts <- pcts_lst[[j]]  # Get the fill percentage for the current state
    pospct <- round((max - min) * pcts / 100 + min)  # Calculate the fill position based on percentage
    finalimg <- img[h:1,,1]  # Flip the image vertically for correct orientation
    bkgr <- (finalimg == 1)  # Background mask
    colfill <- matrix(rep(FALSE, h*w), nrow = h)  # Initialize fill matrix

    # Apply the fill either horizontally or vertically
    if (fillHoriz == FALSE) {
      colfill[1:h, max:pospct] <- TRUE
    } else {
      colfill[max:pospct, 1:w] <- TRUE
    }

    # Assign partially filled cells in the image
    finalimg[bkgr & colfill] <- 0.5
    df <- reshape2::melt(finalimg)  # Convert matrix to long format for plotting

    # Remove partial fill for the 'full' state
    if (j == "full") {
      df[df$value == 0.5, ] <- 0
    }

    # Create the ggplot for each icon state
    plot <- ggplot(df, aes(x = Var2, y = Var1, fill = factor(value))) +
      geom_raster() +
      scale_fill_manual(values = cols_lst[[j]]) +  # Apply the corresponding color scheme
      fnc_blankitout_homepage()  # Apply the blank theme

    plot_lst[[j]] <- plot  # Store the plot in the list
  }

  return(plot_lst)  # Return the list of generated plots
}

#' @title Create Icon Infographic
#' @description Generates an infographic representing a Relative Rate Index (RRI) using icons.
#' @param rri_raw The raw Relative Rate Index value.
#' @param rri_digits Number of decimal places to round the RRI value.
#' @param fillcolor Color to fill the full icons.
#' @param partialcolor Color to fill partially filled icons.
#' @param emptyhumans Logical flag to indicate whether empty icons should be included.
#' @param emptycolor Color for the empty icons.
#' @param infogs Total number of icons to display.
#' @param infogs_ncol Number of columns for the grid layout of icons.
#' @param fillHoriz Logical flag to determine if icons should be filled horizontally or vertically.
#' @return A ggplot2 object representing the generated icon infographic.
#' @export
fnc_create_icons_homepage <- function(rri_raw, rri_digits = 1, fillcolor = "darkgray", partialcolor = "white",
                                      emptyhumans = TRUE, emptycolor = "white", infogs = default_ncols,
                                      infogs_ncol = default_ncols, fillHoriz = FALSE) {

  # Round the RRI value and compute full and partial icons
  RRI <- round(rri_raw, digits = rri_digits)
  numfull <- floor(RRI)  # Number of fully filled icons
  numremain <- RRI - numfull  # Portion of the partial icon

  # Generate plot options for full, partial, and empty icons
  plot_opts <- fnc_icon_options_homepage(partialval = numremain, empty = emptycolor, fill = fillcolor, partial = partialcolor, fillHoriz = fillHoriz)

  plot_list <- list()  # Initialize list for storing plots

  # Set the first icon in green
  first_icon_color <- color4
  first_icon_opts <- fnc_icon_options_homepage(partialval = 0, empty = emptycolor, fill = first_icon_color, partial = first_icon_color, fillHoriz = fillHoriz)
  plot_list[[1]] <- first_icon_opts$full

  # Create full icons in gray based on RRI value
  for (i in 2:(numfull + 1)) {
    plot_list[[i]] <- plot_opts$full
  }

  # Add a partially filled icon if needed
  if (numremain > 0) {
    plot_list[[numfull + 1]] <- plot_opts$partial
  }

  # Add empty icons if needed
  if (emptyhumans && length(plot_list) < infogs) {
    for (i in (numfull + 2):infogs) {
      plot_list[[i]] <- plot_opts$empty
    }
  }

  # Determine the number of rows for the icon grid
  rows <- ifelse(infogs > infogs_ncol, ceiling(length(plot_list) / infogs_ncol), 1)

  # Return the grid of icon plots
  plot_grid(plotlist = plot_list, nrow = rows)
}
