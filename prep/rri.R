
# # load NCRP year end population
# load(file = paste0(sp_data_path, "/data/analysis/ncrp_yearendpop.rds"))

##########################
# Get census data
##########################

# weighted estimate of percentage of race from 2020 census
# pulled estimated counts and construct percent estimate
# these are the ids of race variables that we want to pull
race_vars <- c(estimate_white              = "P3_003N",
               estimate_black              = "P3_004N",
               estimate_asian              = "P3_006N",
               estimate_native_hawaiian_pi = "P3_007N",
               estimate_hispanic           = "P4_002N",
               estimate_american_indian    = "P1_005N")

# get list of states
states <- state.name

# define  function to retrieve and process census data for a given state
fnc_get_census_data <- function(state) {
  census_race_data <- tidycensus::get_decennial(
    geography = "state",
    state = state,
    variables = race_vars,
    summary_var = "P3_001N",
    year = 2020,
    geometry = FALSE
  ) %>%
    clean_names() %>%
    select(-geoid) %>%
    mutate(
      race_eth = case_when(
        variable %in% c("estimate_american_indian", "estimate_asian", "estimate_native_hawaiian_pi") ~ "Other race(s), non-Hispanic",
        variable == "estimate_black" ~ "Black, non-Hispanic",
        variable == "estimate_hispanic" ~ "Hispanic, any race",
        variable == "estimate_white" ~ "White, non-Hispanic",
        TRUE ~ "NA"
      )
    ) %>%
    filter(race_eth != "Other race(s), non-Hispanic") %>%
    group_by(race_eth) %>%
    summarise(race_eth_pop = sum(value)) %>%
    ungroup() %>%
    mutate(total_pop = sum(race_eth_pop)
           # estimate = (race_eth_pop / total_pop) * 100
           )

  return(census_race_data)
}

# use lapply to retrieve and process data for each state
census_data_list <- lapply(states, fnc_get_census_data)

# convert the list of tibbles into a dataframe
census_data_df <- bind_rows(census_data_list)

# add the "state" column to the final dataframe
census_data_df$state <- rep(states, each = nrow(census_data_df) / length(states))






################################################################################

# Calculate RRI for sentence length

################################################################################

# get list of states in NCRP data
states <- ncrp_yearendpop %>%
  filter(rptyear == 2020) %>%
  pull(state) %>%
  unique()

# loop through each state and calculate RRI's
all_census_ncrp_rri <- map(.x = states,  .f = function(x) {

  # filter to state
  census_race_2020 <- census_data_df %>%
    filter(state == x)

  # filter to rptyear 2020 and to state
  ncrp_race_2020 <- ncrp_yearendpop %>%
    filter(rptyear == 2020 &
           state == x &
           race != "Other race(s), non-Hispanic") %>%
    mutate(unique_id = row_number()) %>%
    rename(race_eth = race)

  # prep census and dataframe for join and rri calculation
  census_race_2020 %<>%
    ungroup() %>%
    dplyr::select(race_eth, race_eth_pop)
  ncrp_race_2020 %<>%
    ungroup() %>%
    distinct(unique_id, .keep_all = TRUE)

  # join two files
  ncrp_census_prelim_rri <- left_join(ncrp_race_2020, census_race_2020, by = "race_eth")

  # white, non-hispanic only for rri calculation
  rri_analytic_white <- ncrp_census_prelim_rri %>%
    filter(!is.na(race_eth) == TRUE,
           race_eth == "White, non-Hispanic") %>%
    summarise(people_in_prison_per10kadult         = round(n_distinct(unique_id)/mean(race_eth_pop)*10000, digits = 1),
              people_in_prison_rri                 = NA,

              sentence_length_1_year_per10kadult = round(n_distinct(unique_id[sentlgth == "< 1 year"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_1_year_rri         = NA,

              sentence_length_1_1_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "1-1.9 years"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_1_1_9_years_rri         = NA,

              sentence_length_2_4_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "2-4.9 years"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_2_4_9_years_rri         = NA,

              sentence_length_5_9_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "2-4.9 years"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_5_9_9_years_rri         = NA,

              sentence_length_10_24_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "10-24.9 years"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_10_24_9_years_rri         = NA,

              sentence_length_25_years_per10kadult = round(n_distinct(unique_id[sentlgth == "Life, LWOP, Life plus additional years, Death"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_25_years_rri         = NA,

              sentence_length_lwop_per10kadult = round(n_distinct(unique_id[sentlgth == "Life, LWOP, Life plus additional years, Death"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_lwop_rri         = NA
              ) %>%
    dplyr::mutate(race_eth = "White, non-Hispanic")

  # now calculate rates and RRIs using above dataframe (for White individuals) as reference
  rri_analytic <- ncrp_census_prelim_rri %>%
    dplyr::filter(!is.na(race_eth) == TRUE,
                  race_eth %in% c("Black, non-Hispanic","Hispanic, any race")) %>%
    group_by(race_eth) %>%
    summarise(people_in_prison_per10kadult         = round(n_distinct(unique_id)/mean(race_eth_pop)*10000, digits = 1),
              people_in_prison_rri                 = round(people_in_prison_per10kadult/rri_analytic_white$people_in_prison_per10kadult, digits = 2),

              sentence_length_1_year_per10kadult = round(n_distinct(unique_id[sentlgth == "< 1 year"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_1_year_rri         = round(sentence_length_1_year_per10kadult/rri_analytic_white$sentence_length_1_year_per10kadult, digits = 2),

              sentence_length_1_1_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "1-1.9 years"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_1_1_9_years_rri         = round(sentence_length_1_1_9_years_per10kadult/rri_analytic_white$sentence_length_1_1_9_years_per10kadult, digits = 2),

              sentence_length_2_4_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "2-4.9 years"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_2_4_9_years_rri         = round(sentence_length_2_4_9_years_per10kadult/rri_analytic_white$sentence_length_2_4_9_years_per10kadult, digits = 2),

              sentence_length_5_9_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "2-4.9 years"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_5_9_9_years_rri         = round(sentence_length_5_9_9_years_per10kadult/rri_analytic_white$sentence_length_5_9_9_years_per10kadult, digits = 2),

              sentence_length_10_24_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "10-24.9 years"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_10_24_9_years_rri         = round(sentence_length_10_24_9_years_per10kadult/rri_analytic_white$sentence_length_10_24_9_years_per10kadult, digits = 2),

              sentence_length_25_years_per10kadult = round(n_distinct(unique_id[sentlgth == "Life, LWOP, Life plus additional years, Death"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_25_years_rri         = round(sentence_length_25_years_per10kadult/rri_analytic_white$sentence_length_25_years_per10kadult, digits = 2),

              sentence_length_lwop_per10kadult = round(n_distinct(unique_id[sentlgth == "Life, LWOP, Life plus additional years, Death"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_lwop_rri         = round(sentence_length_lwop_per10kadult/rri_analytic_white$sentence_length_lwop_per10kadult, digits = 2)
              ) %>%
    ungroup()

  # reformat table
  rri_analytic_table <- rri_analytic %>%
    select(race_eth,
           people_in_prison_rri,
           sentence_length_1_year_rri,
           sentence_length_1_1_9_years_rri,
           sentence_length_2_4_9_years_rri,
           sentence_length_5_9_9_years_rri,
           sentence_length_10_24_9_years_rri,
           sentence_length_25_years_rri,
           sentence_length_lwop_rri) %>%
    gather(sample, rri, people_in_prison_rri:sentence_length_lwop_rri) %>%
    mutate(state = x)

  return(rri_analytic_table)
})

# make a dataframe
all_census_ncrp_rri <- setNames(all_census_ncrp_rri, states)
all_census_ncrp_rri <- bind_rows(all_census_ncrp_rri)

# prep for graph
all_census_ncrp_rri_prep <- all_census_ncrp_rri %>%
    mutate(color = ifelse(rri < 1, "#ff640080", "#ff6400"),
         sample = case_when(
           sample == "people_in_prison_rri" ~ "In Prison",
           sample == "sentence_length_1_year_rri"        ~ "Sentence Length < 1 year",
           sample == "sentence_length_1_1_9_years_rri"   ~ "Sentence Length 1-1.9 years",
           sample == "sentence_length_2_4_9_years_rri"   ~ "Sentence Length 2-4.9 years",
           sample == "sentence_length_5_9_9_years_rri"   ~ "Sentence Length 5-9.9 years",
           sample == "sentence_length_10_24_9_years_rri" ~ "Sentence Length 10-24.9 years",
           sample == "sentence_length_25_years_rri"      ~ "Sentence Length >=25 years",
           sample == "sentence_length_lwop_rri"          ~ "Sentence Length Life, LWOP, Death"
         ),
         rri = round(rri, 1),
         tooltip = case_when(
           sample == "In Prison" & rri < 1  ~ paste(race_eth, " people are", rri, " times less likely <br>to be in prison than White people."),
           sample == "In Prison" & rri == 1 ~ paste(race_eth, " people are equally as likely to be in prison as White people."),
           sample == "In Prison" & rri > 1  ~ paste(race_eth, " people are", rri, " times more likely <br>to be in prison than White people."),

           sample == "Sentence Length < 1 year" & rri < 1  ~ paste(race_eth, " people are", rri, " times less likely <br>to have a sentence length of < 1 year than White people."),
           sample == "Sentence Length < 1 year" & rri == 1 ~ paste(race_eth, " people are equally as likely to have a sentence length of < 1 year as White people."),
           sample == "Sentence Length < 1 year" & rri > 1  ~ paste(race_eth, " people are", rri, " times more likely <br>to have a sentence length of < 1 year than White people."),

           sample == "Sentence Length 1-1.9 years" & rri < 1  ~ paste(race_eth, " people are", rri, " times less likely <br>to have a sentence length of 1-1.9 years than White people."),
           sample == "Sentence Length 1-1.9 years" & rri == 1 ~ paste(race_eth, " people are equally as likely to have a sentence length of 1-1.9 years as White people."),
           sample == "Sentence Length 1-1.9 years" & rri > 1  ~ paste(race_eth, " people are", rri, " times more likely <br>to have a sentence length of 1-1.9 years than White people."),

           sample == "Sentence Length 2-4.9 years" & rri < 1  ~ paste(race_eth, " people are", rri, " times less likely <br>to have a sentence length of 2-4.9 years than White people."),
           sample == "Sentence Length 2-4.9 years" & rri == 1 ~ paste(race_eth, " people are equally as likely to have a sentence length of 2-4.9 years as White people."),
           sample == "Sentence Length 2-4.9 years" & rri > 1  ~ paste(race_eth, " people are", rri, " times more likely <br>to have a sentence length of 2-4.9 years than White people."),

           sample == "Sentence Length 5-9.9 years" & rri < 1  ~ paste(race_eth, " people are", rri, " times less likely <br>to have a sentence length of 5-9.9 years than White people."),
           sample == "Sentence Length 5-9.9 years" & rri == 1 ~ paste(race_eth, " people are equally as likely to have a sentence length of 5-9.9 years as White people."),
           sample == "Sentence Length 5-9.9 years" & rri > 1  ~ paste(race_eth, " people are", rri, " times more likely <br>to have a sentence length of 5-9.9 years than White people."),

           sample == "Sentence Length 10-24.9 years" & rri < 1  ~ paste(race_eth, " people are", rri, " times less likely <br>to have a sentence length of 10-24.9 years than White people."),
           sample == "Sentence Length 10-24.9 years" & rri == 1 ~ paste(race_eth, " people are equally as likely to have a sentence length of 10-24.9 years as White people."),
           sample == "Sentence Length 10-24.9 years" & rri > 1  ~ paste(race_eth, " people are", rri, " times more likely <br>to have a sentence length of 10-24.9 years than White people."),

           sample == "Sentence Length >=25 years" & rri < 1  ~ paste(race_eth, " people are", rri, " times less likely <br>to have a sentence length of >=25 years than White people."),
           sample == "Sentence Length >=25 years" & rri == 1 ~ paste(race_eth, " people are equally as likely to have a sentence length of >=25 years as White people."),
           sample == "Sentence Length >=25 years" & rri > 1  ~ paste(race_eth, " people are", rri, " times more likely <br>to have a sentence length of >=25 years than White people."),

           sample == "Sentence Length Life, LWOP, Death" & rri < 1  ~ paste(race_eth, " people are", rri, " times less likely <br>to have a sentence length of life, life without parole, or death than White people."),
           sample == "Sentence Length Life, LWOP, Death" & rri == 1 ~ paste(race_eth, " people are equally as likely to have a sentence length of life, life without parole, or death as White people."),
           sample == "Sentence Length Life, LWOP, Death" & rri > 1  ~ paste(race_eth, " people are", rri, " times more likely <br>to have a sentence length of life, life without parole, or death than White people."),

           TRUE ~ NA_character_
         )) %>%
  mutate(color = case_when(rri < 1 ~ "#ff640080",
                           rri == 1 ~ "gray",
                           rri > 1 ~ "#ff6400"),

         type = case_when(rri < 1 ~ "Underrepresented",
                           rri == 1 ~ "Equally Represented",
                           rri > 1 ~ "Overrepresented")
  ) %>%

  mutate_all(~ ifelse(is.nan(.), NA, .)) %>%
  mutate_all(~ ifelse(is.infinite(.), NA, .)) %>%

  # REMOVE ALABAMA
  filter(state != "Alabama")


# loop through each state and create visualizations
states <- all_census_ncrp_rri_prep %>%
  filter(race_eth == "Black, non-Hispanic") %>%
  filter(!is.na(rri)) %>%
  filter(!is.infinite(rri)) %>%
  pull(state) %>% unique()

all_bar_rri_sentence_length_black <- map(.x = states,  .f = function(x) {

  df1 <- all_census_ncrp_rri_prep %>%
    filter(state == x) %>%
    filter(race_eth == "Black, non-Hispanic") %>%
    filter(!is.na(rri)) %>%
    filter(!is.infinite(rri))
  #
  # # Calculate max_value and set min_value
  # max_value <- max(df1$rri, na.rm = TRUE)
  # max_value <- ceiling(max_value)
  # min_value <- 0

  min_value <- 0
  # calculate the common max value
  max_value_black <- max(all_census_ncrp_rri_prep %>%
                           filter(state == x) %>%
                           filter(race_eth == "Black, non-Hispanic") %>%
                           filter(!is.na(rri)) %>%
                           filter(!is.infinite(rri)) %>%
                           pull(rri),
                         na.rm = TRUE)

  max_value_hispanic <- max(all_census_ncrp_rri_prep %>%
                              filter(state == x) %>%
                              filter(race_eth == "Hispanic, any race") %>%
                              filter(!is.na(rri)) %>%
                              filter(!is.infinite(rri)) %>%
                              pull(rri),
                            na.rm = TRUE)

  # Determine the larger max value
  max_value <- max(max_value_black, max_value_hispanic)
  max_value <- ceiling(max_value)

  # # get y axis labels
  # custom_labels <- list(
  #   list(y = 1, text = "1")
  # )

  # get y axis labels - option 2
  categories_list <- list()
  for (i in 0:max_value) {
    if (i == 1) {
      categories_list[[as.character(i)]] <- "1 = White<br>Reference Line"
    } else {
      categories_list[[as.character(i)]] <- " "
    }
  }

  highcharts <- df1 %>%
    hchart(type = "bar", hcaes(x = "sample", y = "rri", group = "type")) %>%
    hc_title(text = "Black People, Non-Hispanic") %>%
    hc_subtitle(text = "Relative Rate Index") %>%
    hc_xAxis(title = "",
             categories = c(
               "In Prison",
               "Sentence Length < 1 year",
               "Sentence Length 1-1.9 years",
               "Sentence Length 2-4.9 years",
               "Sentence Length 5-9.9 years",
               "Sentence Length 10-24.9 years",
               "Sentence Length >=25 years",
               "Sentence Length Life, LWOP, Death"
             )) %>%
   hc_yAxis(title = "",
            categories = categories_list,
            labels = list(rotation = 0,
                          step = 1),
            min = 0,
            max = max_value,
             plotBands = list(
               list(
                 color = "rgba(0, 0, 0, 0.2)",
                 from = 0,
                 to = 1))) %>%
    hc_legend(enabled = TRUE) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_plotOptions(series = list(stacking = "normal"),
                   bar = list(
                     dataLabels = list(enabled = TRUE, format = "{point.rri}", style = list(fontSize = "12px"))
                   )) %>%
    hc_chart(marginTop = 100, marginBottom = 80, spacingBottom = 80)

  unique_types <- unique(df1$type)

  if ("Overrepresented" %in% unique_types && "Underrepresented" %in% unique_types && "Equally Represented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("gray", "#ff6400", "#ff640080"))

  } else if ("Overrepresented" %in% unique_types && "Underrepresented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("#ff6400", "#ff640080"))

  } else if ("Overrepresented" %in% unique_types && "Equally Represented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("gray", "#ff6400"))

  } else if ("Underrepresented" %in% unique_types && "Equally Represented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("gray", "#ff640080"))

  } else if ("Overrepresented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("#ff6400"))

  } else if ("Underrepresented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("#ff640080"))

  } else if ("Equally Represented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("gray"))
  }

  return(highcharts)
})

all_bar_rri_sentence_length_black <- setNames(all_bar_rri_sentence_length_black, states)







# loop through each state and create visualizations
states <- all_census_ncrp_rri_prep %>%
  filter(race_eth == "Hispanic, any race") %>%
  filter(!is.na(rri)) %>%
  filter(!is.infinite(rri)) %>%
  pull(state) %>% unique()

all_bar_rri_sentence_length_hispanic <- map(.x = states,  .f = function(x) {

  df1 <- all_census_ncrp_rri_prep %>%
    filter(state == x) %>%
    filter(race_eth == "Hispanic, any race") %>%
    filter(!is.na(rri)) %>%
    filter(!is.infinite(rri))
  #
  # # Calculate max_value and set min_value
  # max_value <- max(df1$rri, na.rm = TRUE)
  # max_value <- ceiling(max_value)
  # min_value <- 0

  min_value <- 0
  # calculate the common max value
  max_value_black <- max(all_census_ncrp_rri_prep %>%
                           filter(state == x) %>%
                           filter(race_eth == "Black, non-Hispanic") %>%
                           filter(!is.na(rri)) %>%
                           filter(!is.infinite(rri)) %>%
                           pull(rri),
                         na.rm = TRUE)

  max_value_hispanic <- max(all_census_ncrp_rri_prep %>%
                              filter(state == x) %>%
                              filter(race_eth == "Hispanic, any race") %>%
                              filter(!is.na(rri)) %>%
                              filter(!is.infinite(rri)) %>%
                              pull(rri),
                            na.rm = TRUE)

  # Determine the larger max value
  max_value <- max(max_value_black, max_value_hispanic)
  max_value <- ceiling(max_value)

  # get y axis labels - option 2
  categories_list <- list()
  for (i in 0:max_value) {
    if (i == 1) {
      categories_list[[as.character(i)]] <- "1 = White<br>Reference Line"
    } else {
      categories_list[[as.character(i)]] <- " "
    }
  }

  highcharts <- df1 %>%
    hchart(type = "bar", hcaes(x = "sample", y = "rri", group = "type")) %>%
    hc_title(text = "Hispanic People, Any Race") %>%
    hc_subtitle(text = "Relative Rate Index") %>%
    hc_xAxis(title = "",
             categories = c(
               "In Prison",
               "Sentence Length < 1 year",
               "Sentence Length 1-1.9 years",
               "Sentence Length 2-4.9 years",
               "Sentence Length 5-9.9 years",
               "Sentence Length 10-24.9 years",
               "Sentence Length >=25 years",
               "Sentence Length Life, LWOP, Death"
             )) %>%
    hc_yAxis(title = "",
             categories = categories_list,
             labels = list(rotation = 0,
                           step = 1),
             min = 0,
             max = max_value,
             plotBands = list(
               list(
                 color = "rgba(0, 0, 0, 0.2)",
                 from = 0,
                 to = 1))) %>%
    hc_legend(enabled = TRUE) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_plotOptions(series = list(stacking = "normal"),
                   bar = list(
                     dataLabels = list(enabled = TRUE, format = "{point.rri}", style = list(fontSize = "12px"))
                   )) %>%
    hc_chart(marginTop = 100, marginBottom = 80, spacingBottom = 80)

  unique_types <- unique(df1$type)

  if ("Overrepresented" %in% unique_types && "Underrepresented" %in% unique_types && "Equally Represented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("gray", "#ff6400", "#ff640080"))

  } else if ("Overrepresented" %in% unique_types && "Underrepresented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("#ff6400", "#ff640080"))

  } else if ("Overrepresented" %in% unique_types && "Equally Represented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("gray", "#ff6400"))

  } else if ("Underrepresented" %in% unique_types && "Equally Represented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("gray", "#ff640080"))

  } else if ("Overrepresented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("#ff6400"))

  } else if ("Underrepresented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("#ff640080"))

  } else if ("Equally Represented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("gray"))
  }

  return(highcharts)
})

all_bar_rri_sentence_length_hispanic <- setNames(all_bar_rri_sentence_length_hispanic, states)








################################################################################

# Calculate RRI for release timing

################################################################################

































################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_bar_rri_sentence_length_black,    file=file.path(folder, "all_bar_rri_sentence_length_black.rds"))
  save(all_bar_rri_sentence_length_hispanic, file=file.path(folder, "all_bar_rri_sentence_length_hispanic.rds"))

}
