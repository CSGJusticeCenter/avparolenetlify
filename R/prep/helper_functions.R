#------ Import Data Functions ------#

#' Load NCRP Data
#'
#' This function loads NCRP data for a given file ID.
#'
#' @param file_id A string value representing the file ID of the data to be loaded.
#' @return The loaded data object.
#' @export
fnc_load_ncrp_data <- function(file_id) {
  file_path <- paste0(config$sp_data_path, "/data/raw/ICPSR_38492-V1/ICPSR_38492/DS000", file_id, "/38492-000", file_id, "-Data.rda")
  if (file.exists(file_path)) {
    data_env <- new.env()
    load(file_path, envir = data_env)
    data_object <- ls(envir = data_env)[1]  # Assuming only one object is loaded
    data <- get(data_object, envir = data_env)
    return(data)
  } else {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
}


#' Load BJS Prisoners Data by Gender
#'
#' This function loads BJS prisoners data by gender for given years.
#'
#' @param year A numeric value representing the year of the data to be loaded.
#' @param subfolder A string representing the subfolder where the data is stored.
#' @param file_name A string representing the file name of the data.
#' @return A data frame of the loaded data or NULL if the file does not exist.
#' @export
fnc_load_bjs_prison_data <- function(year, subfolder, file_name) {
  file_path <- paste0(config$sp_data_path, "/data/raw/", subfolder, "/", file_name)
  if (file.exists(file_path)) {
    return(read.csv(file_path))
  } else {
    warning(paste("File for year", year, "not found:", file_path))
    return(NULL)
  }
}

#' Load Annual Parole Survey Data
#'
#' This function loads Annual Parole Survey data for a given year and ICPSR code.
#'
#' @param year A numeric value representing the year of the data to be loaded.
#' @param icpsr_code A string representing the ICPSR code of the data.
#' @return The loaded data object.
#' @export
fnc_load_aps_data <- function(year, icpsr_code) {
  file_path <- paste0(config$sp_data_path, "/data/raw/ICPSR_", icpsr_code, "-V1/ICPSR_",
                      icpsr_code, "/DS0001/", icpsr_code, "-0001-Data.rda")
  if (file.exists(file_path)) {
    loaded_object_name <- load(file_path)
    return(get(loaded_object_name))
  } else {
    warning(paste("File for year", year, "with ICPSR code", icpsr_code, "not found:", file_path))
    return(NULL)
  }
}


#------ Data Formatting Functions ------#

#' Re-categorize Admission Type
#'
#' This function re-categorizes the admission type in the given data frame.
#'
#' @param df A data frame containing the admission type data.
#' @return A data frame with re-categorized admission type.
#' @export
fnc_create_admtype <- function(df) {
  df <- df %>%
    mutate(admtype = case_when(
      admtype == "Other admission (including unsentenced, transfer, AWOL/escapee return)" ~ "Other or Unknown",
      is.na(admtype) ~ "Other or Unknown",
      TRUE ~ admtype
    )) %>%
    mutate(admtype = factor(admtype,
                            levels = c("New court commitment",
                                       "Parole return/revocation",
                                       "Other or Unknown")))
  return(df)
}

#' Create Parole Eligibility Status
#'
#' This function creates the parole eligibility status in the given data frame.
#'
#' @param df A data frame containing the parole eligibility data.
#' @return A data frame with parole eligibility status.
#' @export
fnc_create_parelig_status <- function(df) {
  df <- df %>%
    mutate(time_between_ped_rptyear = parelig_year - rptyear) %>%
    mutate(
      parelig_status = case_when(
        parelig_year <= rptyear ~ "Current",
        parelig_year > rptyear & time_between_ped_rptyear <= 5 ~ "Future 1-5 Years",
        parelig_year > rptyear & time_between_ped_rptyear > 5 ~ "Future 6+ Years",
        is.na(parelig_year) ~ "Missing"
      ),
      parelig_status = factor(parelig_status,
                              levels = c("Current",
                                         "Future 1-5 Years",
                                         "Future 6+ Years",
                                         "Missing"))
    )
  return(df)
}

#' Re-categorize Offense Type
#'
#' This function re-categorizes the offense type in the given data frame.
#'
#' @param df A data frame containing the offense type data.
#' @return A data frame with re-categorized offense type.
#' @export
fnc_create_fbi_index <- function(df) {
  df <- df %>%
    mutate(fbi_index = case_when(
      offdetail == "Aggravated or simple assault" ~ "Aggravated or Simple Assault",
      offdetail == "Murder (including non-negligent manslaughter)" ~ "Murder and Non-negligent Manslaughter",
      offdetail == "Negligent manslaughter" ~ "Other Violent Offenses",
      offdetail == "Other violent offenses" ~ "Other Violent Offenses",
      offdetail == "Rape/sexual assault" ~ "Rape or Sexual Assault",
      offdetail == "Robbery" ~ "Robbery",
      offdetail == "Other/unspecified" ~ "Other or Unknown",
      is.na(offdetail) ~ "Other or Unknown",
      TRUE ~ offgeneral
    )) %>%
    mutate(fbi_index = factor(fbi_index,
                              levels = c("Murder and Non-negligent Manslaughter",
                                         "Rape or Sexual Assault",
                                         "Robbery",
                                         "Aggravated or Simple Assault",
                                         "Other Violent Offenses",
                                         "Property",
                                         "Public order",
                                         "Drugs",
                                         "Other or Unknown")))
  return(df)
}

#' Clean BJS Data
#'
#' This function cleans BJS data.
#'
#' @param df A data frame containing BJS data.
#' @return A cleaned data frame.
#' @export
fnc_clean_bjs_data <- function(df) {
  df <- df %>%
    mutate(state = str_replace(state, "/.*", "")) %>%
    mutate(state = str_replace(state, "Alaskab", "Alaska")) %>%
    mutate(state = str_replace(state, "Utahc", "Utah")) %>%
    filter(state != "" &
             state != "State" &
             state != "Federal" &
             state != "District of Columbia" &
             state != "U.S. Total" &
             state != "U.S. total" &
             state != "U.S. tota") %>%
    mutate(bjs_prison_population = str_replace_all(bjs_prison_population, "[^\\d]", "")) %>%
    mutate(bjs_prison_population = as.numeric(bjs_prison_population))
  return(df)
}

#' Prepare APS Data
#'
#' This function prepares APS data depending on the year.
#'
#' @param data A data frame containing APS data.
#' @param year A numeric value representing the year of the data.
#' @param pre_2008 A boolean value indicating if the data is from before 2008.
#' @return A prepared data frame.
#' @export
fnc_prepare_aps_data <- function(data, year, pre_2008 = FALSE) {
  data <- data %>%
    clean_names() %>%
    mutate(rptyear = year)

  if (pre_2008) {
    data <- data %>%
      select(-stateid) %>%
      rename(stateid = state) %>%
      mutate(stateid = str_trim(stateid)) %>%
      left_join(state_names_abb, by = "stateid") %>%
      mutate(enreltsr = NA) %>%
      select(state, rptyear, endisrel, enmanrel, enreltsr, incarcerated_from_parole = exincrev) %>%
      mutate(released_to_parole = rowSums(.[c("endisrel", "enmanrel")], na.rm = TRUE)) %>%
      mutate(released_to_parole = ifelse(released_to_parole == 0, NA, released_to_parole))
  } else {
    data <- data %>%
      mutate(state = str_sub(stateid, 6, -1)) %>%
      mutate(rptyear = as.numeric(rptyear)) %>%
      select(state, rptyear, endisrel, enmanrel, enreltsr, incarcerated_from_parole = exincrev) %>%
      mutate(released_to_parole = rowSums(.[c("endisrel", "enmanrel", "enreltsr")], na.rm = TRUE)) %>%
      mutate(released_to_parole = ifelse(released_to_parole == 0, NA, released_to_parole))
  }

  return(data)
}




#------ Highchart Themes and Functions ------#

#' Common Style Elements
#'
#' This list defines the common style elements used across different themes.
#' @return A list of common style elements.
#' @export
common_style <- list(
  fontFamily = "Graphik",
  color = "black",
  fontSize = "12px",
  fontWeight = "regular"
)

#' Common Chart Style
#'
#' This list defines the common chart style elements used across different themes.
#' @return A list of common chart style elements.
#' @export
common_chart_style <- list(
  fontFamily = "Graphik",
  fontSize = "12px",
  color = "black"
)

#' Common Title Style
#'
#' This list defines the common title style elements used across different themes.
#' @return A list of common title style elements.
#' @export
common_title_style <- list(
  fontFamily = "Graphik",
  fontWeight = "bold",
  color = "black"
)

#' Base Highcharts Theme
#'
#' This theme serves as the base for other themes.
#' @export
base_hc_theme <- hc_theme(
  colors = c(colors$green2, colors$yellow, colors$red, colors$purple, colors$blue),
  chart = list(style = common_chart_style),
  title = list(align = "center", style = modifyList(common_title_style, list(fontSize = "16px"))),
  subtitle = list(align = "center", style = modifyList(common_title_style, list(fontSize = "14px"))),
  legend = list(
    align = "center",
    verticalAlign = "top",
    itemStyle = common_style
  ),
  xAxis = list(
    labels = list(enabled = TRUE, style = common_style),
    gridLineColor = "transparent",
    lineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  yAxis = list(
    labels = list(enabled = TRUE, style = common_style),
    gridLineColor = "transparent",
    lineColor = "transparent",
    majorGridLineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  tooltip = list(
    style = common_style
  ),
  plotOptions = list(
    line = list(marker = list(enabled = FALSE)),
    spline = list(marker = list(enabled = FALSE)),
    area = list(marker = list(enabled = FALSE)),
    areaspline = list(marker = list(enabled = FALSE)),
    arearange = list(marker = list(enabled = FALSE)),
    bubble = list(maxSize = "10%"),
    column = list(
      dataLabels = list(
        style = list(color = "black")
      )
    )
  )
)

#' Highcharts Theme with Lines
#'
#' This theme includes additional settings for line charts.
#' @export
hc_theme_with_line <- hc_theme(
  colors = c(colors$blue, colors$red, colors$green2, colors$yellow, colors$purple, colors$brown),
  chart = list(style = common_chart_style),
  title = list(align = "center", style = modifyList(common_title_style, list(fontSize = "16px"))),
  subtitle = list(align = "center", style = modifyList(common_title_style, list(fontSize = "14px"))),
  legend = list(align = "center", verticalAlign = "top", itemStyle = common_style),
  xAxis = list(labels = list(enabled = TRUE, style = common_style), gridLineColor = "transparent", lineColor = "transparent", minorGridLineColor = "transparent", tickColor = "transparent"),
  yAxis = list(labels = list(enabled = TRUE, style = common_style)),
  plotOptions = list(
    line = list(marker = list(enabled = FALSE)),
    spline = list(marker = list(enabled = FALSE)),
    area = list(marker = list(enabled = FALSE)),
    areaspline = list(marker = list(enabled = FALSE)),
    arearange = list(marker = list(enabled = FALSE)),
    bubble = list(maxSize = "10%"),
    column = list(
      dataLabels = list(
        style = list(color = "black")
      )
    )
  )
)

#' Highcharts Theme for Maps
#'
#' This theme is specifically designed for maps.
#' @export
hc_theme_map <- hc_theme_merge(
  hc_theme_smpl(),
  base_hc_theme,
  hc_theme(
    chart = list(style = modifyList(common_chart_style, list(fontSize = "14px"))),
    title = list(align = "center", style = modifyList(common_title_style, list(fontSize = "22px"))),
    plotOptions = list(
      series = list(states = list(inactive = list(opacity = 1))),
      line = list(marker = list(enabled = TRUE)),
      spline = list(marker = list(enabled = TRUE)),
      area = list(marker = list(enabled = TRUE)),
      areaspline = list(marker = list(enabled = TRUE))
    ),
    legend = list(
      itemStyle = modifyList(common_style, list(fontSize = "16px"))
    )
  )
)
















# Create single horizontal bar
fnc_hc_single_horz_bar <- function(df, x_var, y_var, group_var, accessibility_text) {

  highchart <- hchart(df, "bar",
                      hcaes(x = !!sym(x_var),
                            y = !!sym(y_var),
                            group = !!sym(group_var)),
                      dataLabels = list(enabled = TRUE,
                                        format = "{point.prop_label}",
                                        style = list(fontWeight = "bold",
                                                     fontSize = "12px",
                                                     fontFamily = "Graphik"))) %>%
    hc_yAxis(title = list(text = ""),
             min = 0, max = 1) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = FALSE)) %>%
    hc_add_theme(base_hc_theme) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(stacking = "normal",
                    animation = FALSE,
                    cursor = "pointer",
                    borderWidth = 3,
                    minPointLength = 4),
      accessibility = list(enabled = TRUE,
                           keyboardNavigation = list(enabled = TRUE),
                           linkedDescription = accessibility_text,
                           landmarkVerbosity = "one"),
      area = list(accessibility = list(description = accessibility_text)))
  return(highchart)
}
