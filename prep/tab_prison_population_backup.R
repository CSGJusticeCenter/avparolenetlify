fnc_stackedbar_admtype_chart <- function(df, group_by_col) {
  highchart <- hchart(df, "bar",
                      hcaes(x = admtype,
                            y = prop,
                            group = !!sym(group_by_col)
                      ),
                      dataLabels = list(enabled = TRUE,
                                        format = "{point.prop_label}",
                                        style = list(fontWeight = "bold",
                                                     fontSize = "12px",
                                                     fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1
    ) %>%
    hc_xAxis(categories = c("New court commitment",
                            "Parole return/revocation",
                            "Other or Unknown"),
             title = list(text = ""),
             labels = list(enabled = TRUE)) %>%
    hc_legend(enabled = TRUE,
              reversed = TRUE) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(
        stacking = "normal", animation = FALSE, cursor = "pointer",
        borderWidth = 3, minPointLength = 4),
      accessibility = list(enabled = TRUE,
                           keyboardNavigation = list(enabled = TRUE),
                           linkedDescription = accessibility_text,
                           landmarkVerbosity = "one"),
      area = list(accessibility = list(description = accessibility_text)))
  return(highchart)
}

fnc_values_tooltip <- function(df, count_column) {
  df %>%
    count({{ count_column }}) %>%
    mutate(
      prop = (n / sum(n)),
      prop_label = paste0(round(prop*100, 0), "%"),
      n_label = formattable::comma(n, 0),
      tooltip = paste0("<b>", state, "</b><br><br>",
                       "<b>", {{ count_column }}, "</b><br><br>",
                       "Percentage of People: <b>", prop_label, "</b>", sep = "")
    )
}



# Get number/prop people by race and admission type
ncrp_yearendpop_race <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>%
  group_by(state, admtype) %>%
  fnc_values_tooltip(race)

# Highchart
states <- unique(ncrp_yearendpop_race$state)
all_stackedbar_prison_race <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_race %>%
    ungroup() %>%
    filter(state == x)
  highcharts <- fnc_stackedbar_admtype_chart(df1, "race")
  highcharts <- highcharts %>% hc_chart(marginBottom = 45) %>%
    return(highcharts)
})
all_stackedbar_prison_race <- setNames(all_stackedbar_prison_race, states)
all_stackedbar_prison_race$Georgia

# Create sentences describing who is in prison by race and ethnicity
states <- ncrp_yearendpop_race %>%
  group_by(state) %>%
  filter(any(admtype == "New court commitment") &
           any(admtype == "Parole return/revocation")) %>%
  pull(state) %>%
  unique()

all_sentence_prison_race <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_race %>%
    filter(state == x)

  max_new_court <- df1 %>%
    filter(admtype == "New court commitment") %>%
    arrange(desc(prop)) %>%
    slice(1) %>%
    pull(race)

  max_parole_return <- df1 %>%
    filter(admtype == "Parole return/revocation") %>%
    arrange(desc(prop)) %>%
    slice(1) %>%
    pull(race)

  if(max_new_court == max_parole_return) {
    sentences <- paste0("In ", select_year, ", ", max_new_court,
                        " individuals made up the largest portion of people in prison for both new court commitments and parole returns and revocations.")
  } else {
    sentences <- paste0("In ", select_year, ", ", max_new_court,
                        " individuals made up the largest portion of people in prison for new court commitments, while ",
                        max_parole_return,
                        " individuals made up the largest portion of people in prison for parole returns and revocations.")
  }

  return(sentences)
})

all_sentence_prison_race <- setNames(all_sentence_prison_race, states)
all_sentence_prison_race$Georgia




##########
# Age
##########

# Get number/prop people by ageyrend
ncrp_yearendpop_ageyrend <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>%
  group_by(state, admtype) %>%
  fnc_values_tooltip(ageyrend)

# Highchart
states <- unique(ncrp_yearendpop_ageyrend$state)
all_stackedbar_prison_ageyrend <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_ageyrend %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  highcharts <- fnc_stackedbar_admtype_chart(df1, "ageyrend")
  return(highcharts)
})
all_stackedbar_prison_ageyrend <- setNames(all_stackedbar_prison_ageyrend, states)
all_stackedbar_prison_ageyrend$Georgia

# Sentence
states <- ncrp_yearendpop_ageyrend  %>%
  group_by(state) %>%
  filter(any(admtype == "New court commitment") &
           any(admtype == "Parole return/revocation")) %>%
  pull(state) %>%
  unique()

all_sentence_prison_ageyrend  <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_ageyrend  %>%
    filter(state == x)

  max_new_court_age <- df1 %>%
    filter(admtype == "New court commitment") %>%
    arrange(desc(prop)) %>%
    slice(1) %>%
    pull(ageyrend)

  max_parole_return_age <- df1 %>%
    filter(admtype == "Parole return/revocation") %>%
    arrange(desc(prop)) %>%
    slice(1) %>%
    pull(ageyrend)

  if(max_new_court_age == max_parole_return_age) {
    sentences <- paste0("In ", select_year, ", individuals aged ", max_new_court_age,
                        " made up the largest portion of people in prison for both new court commitments and parole returns and revocations.")
  } else {
    sentences <- paste0("In ", select_year, ", individuals aged ", max_new_court_age,
                        " made up the largest portion of people in prison for new court commitments, while individuals aged ",
                        max_parole_return_age,
                        " made up the largest portion of people in prison for parole returns and revocations.")
  }

  return(sentences)
})

all_sentence_prison_ageyrend <- setNames(all_sentence_prison_ageyrend, states)
all_sentence_prison_ageyrend$Georgia






##########
# Gender
##########

# Get number/prop people by gender
ncrp_yearendpop_gender <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>%
  group_by(state, admtype) %>%
  fnc_values_tooltip(sex)

# Highchart
states <- unique(ncrp_yearendpop_gender$state)
all_stackedbar_prison_gender <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_gender %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  highcharts <- fnc_stackedbar_admtype_chart(df1, "sex")
  return(highcharts)
})

all_stackedbar_prison_gender <- setNames(all_stackedbar_prison_gender, states)
all_stackedbar_prison_gender$Georgia

# Create sentences describing who is in prison by gender
states <- ncrp_yearendpop_gender %>%
  group_by(state) %>%
  filter(any(admtype == "New court commitment") &
           any(admtype == "Parole return/revocation")) %>%
  pull(state) %>%
  unique()

all_sentence_prison_gender <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_gender %>%
    filter(state == x)

  max_new_court <- df1 %>%
    filter(admtype == "New court commitment") %>%
    arrange(desc(prop)) %>%
    slice(1) %>%
    pull(sex)

  max_parole_return <- df1 %>%
    filter(admtype == "Parole return/revocation") %>%
    arrange(desc(prop)) %>%
    slice(1) %>%
    pull(sex)

  if(max_new_court == max_parole_return) {
    sentences <- paste0("In ", select_year, ", ", max_new_court,
                        " individuals made up the largest portion of people in prison for both new court commitments and parole returns and revocations.")
  } else {
    sentences <- paste0("In ", select_year, ", ", max_new_court,
                        " individuals made up the largest portion of people in prison for new court commitments, while ",
                        max_parole_return,
                        " individuals made up the largest portion of people in prison for parole returns and revocations.")
  }

  return(sentences)
})

all_sentence_prison_gender <- setNames(all_sentence_prison_gender, states)
all_sentence_prison_gender$Georgia
