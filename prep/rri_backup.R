
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
#
# census_race_2020_table <-
#   tidycensus::get_decennial(geography = "state",
#                             state = "Florida",
#                             variables = race_vars,
#                             summary_var = "P3_001N",
#                             year = 2020,
#                             geometry = FALSE) %>%
#   clean_names() %>%
#   select(-geoid) %>%
#   mutate(
#     race_eth = case_when(
#       variable %in% c("estimate_american_indian", "estimate_asian", "estimate_native_hawaiian_pi") ~ "Other race(s), non-Hispanic",
#       variable == "estimate_black" ~ "Black, non-Hispanic",
#       variable == "estimate_hispanic" ~ "Hispanic, any race",
#       variable == "estimate_white" ~ "White, non-Hispanic",
#       TRUE ~ "NA"
#     )
#   ) %>%
#   filter(race_eth != "Other race(s), non-Hispanic") %>%
#   group_by(race_eth) %>%
#   summarise(race_eth_pop = sum(value)) %>%
#   ungroup() %>%
#   mutate(total_pop = sum(race_eth_pop),
#          estimate = (race_eth_pop / total_pop) * 100)

# define the list of state names
# states <- c("Alabama", "Florida")
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
    summarise(people_in_prison_per10kadult = round(n_distinct(unique_id)/mean(race_eth_pop)*10000, digits = 1),
              people_in_prison_rri = NA,
              sentence_length_25_years_per10kadult = round(n_distinct(unique_id[sentlgth == ">=25 years"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_25_years_rri = NA) %>%
    dplyr::mutate(race_eth = "White, non-Hispanic")

  # now calculate rates and RRIs using above dataframe (for White individuals) as reference
  rri_analytic <- ncrp_census_prelim_rri %>%
    dplyr::filter(!is.na(race_eth) == TRUE,
                  race_eth %in% c("Black, non-Hispanic","Hispanic, any race")) %>%
    group_by(race_eth) %>%
    summarize(people_in_prison_per10kadult         = round(n_distinct(unique_id)/mean(race_eth_pop)*10000,
                                                           digits = 1),
              people_in_prison_rri                 = round(people_in_prison_per10kadult/rri_analytic_white$people_in_prison_per10kadult, digits = 2),
              sentence_length_25_years_per10kadult = round(n_distinct(unique_id[sentlgth == ">=25 years"])/mean(race_eth_pop)*10000, digits = 1),
              sentence_length_25_years_rri         = round(sentence_length_25_years_per10kadult/rri_analytic_white$sentence_length_25_years_per10kadult, digits = 2)) %>%
    ungroup()

  # reformat table
  rri_analytic_table <- rri_analytic %>%
    select(race_eth,
           people_in_prison_rri,
           sentence_length_25_years_rri) %>%
    gather(sample, rri, people_in_prison_rri:sentence_length_25_years_rri) %>%
    mutate(state = x)

  return(rri_analytic_table)
})

# make a dataframe
all_census_ncrp_rri <- setNames(all_census_ncrp_rri, states)
all_census_ncrp_rri <- bind_rows(all_census_ncrp_rri)
















# filter to state
census_race_2020 <- census_data_df %>%
  filter(state == "Florida")

# filter to rptyear 2020 and to state
ncrp_race_2020 <- ncrp_yearendpop %>%
  filter(rptyear == 2020 &
           state == "Florida" &
           race != "Other race(s), non-Hispanic") %>%
  mutate(unique_id = row_number()) %>%
  rename(race_eth = race)

# prep census and dataframe for join and rri calculation
census_race_2020_table %<>%
  ungroup() %>%
  dplyr::select(race_eth, race_eth_pop)
ncrp_race_2020 %<>%
  ungroup() %>%
  distinct(unique_id, .keep_all = TRUE)

# join two files
ncrp_census_prelim_rri <- left_join(ncrp_race_2020, census_race_2020_table, by = "race_eth")

# white, non-hispanic only for rri calculation
rri_analytic_white <- ncrp_census_prelim_rri %>%
  filter(!is.na(race_eth) == TRUE,
         race_eth == "White, non-Hispanic") %>%
  summarise(people_in_prison_per10kadult = round(n_distinct(unique_id)/mean(race_eth_pop)*10000, digits = 1),
            people_in_prison_rri = NA,
            sentence_length_25_years_per10kadult = round(n_distinct(unique_id[sentlgth == ">=25 years"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_25_years_rri = NA) %>%
  dplyr::mutate(race_eth = "White, non-Hispanic")

# now calculate rates and RRIs using above dataframe (for White individuals) as reference
rri_analytic <- ncrp_census_prelim_rri %>%
  dplyr::filter(!is.na(race_eth) == TRUE,
                race_eth %in% c("Black, non-Hispanic","Hispanic, any race")) %>%
  group_by(race_eth) %>%
  summarize(people_in_prison_per10kadult         = round(n_distinct(unique_id)/mean(race_eth_pop)*10000,
                                                         digits = 1),
            people_in_prison_rri                 = round(people_in_prison_per10kadult/rri_analytic_white$people_in_prison_per10kadult, digits = 2),
            sentence_length_25_years_per10kadult = round(n_distinct(unique_id[sentlgth == ">=25 years"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_25_years_rri         = round(sentence_length_25_years_per10kadult/rri_analytic_white$sentence_length_25_years_per10kadult, digits = 2)) %>%
  ungroup()

# reformat table
rri_analytic_table <- rri_analytic %>%
  select(race_eth,
         people_in_prison_rri,
         sentence_length_25_years_rri) %>%
  gather(sample, rri, people_in_prison_rri:sentence_length_25_years_rri)























# create ungrouped denominators for grouped descriptives below
denom_all <- n_distinct(ncrp_race_2020$unique_id[!is.na(ncrp_race_2020$race_eth) == TRUE])

# list of denominator ranges to process
denominator_ranges <- c(
  "< 1 year",
  "1-1.9 years",
  "2-4.9 years",
  "5-9.9 years",
  "10-24.9 years",
  ">=25 years"
)

# initialize an empty list to store results
denom_results <- list()

# iterate over denominator ranges and calculate n_distinct for each
for (denom_range in denominator_ranges) {
  column_name <- paste0("denom_", gsub("[^a-zA-Z0-9]", "_", denom_range))
  query <- !is.na(ncrp_race_2020$race_eth) & ncrp_race_2020$sentlgth == denom_range
  result <- n_distinct(ncrp_race_2020$unique_id[query])
  denom_results[[column_name]] <- result
}

# convert the list to a data frame
rri_denominators <- data.frame(
  denominator = names(denom_results),
  distinct_count = unlist(denom_results)
)

# make vectors
for (denom in rri_denominators$denominator) {
  value <- rri_denominators$distinct_count[rri_denominators$denominator == denom]
  assign(denom, value)
}

# unique individuals in prison grouped by race/ethnicity
ncrp_race_2020_table <- ncrp_race_2020 %>%
  distinct(unique_id, .keep_all = TRUE) %>%
  filter(!is.na(race_eth) == TRUE) %>%
  group_by(race_eth) %>%

  summarise(people_in_prison_n = n_distinct(unique_id),
            people_in_prison_pct = round(n_distinct(unique_id)/denom_all*100, digits = 2),

            sentlgth_1_year_n = n_distinct(unique_id[sentlgth == "< 1 year"]),
            sentlgth_1_year_pct = round(n_distinct(unique_id[sentlgth == "< 1 year"]) /
                                          denom___1_year * 100, digits = 2),

            sentlgth_1_1_9_years_n = n_distinct(unique_id[sentlgth == "1-1.9 years"]),
            sentlgth_1_1_9_years_pct = round(n_distinct(unique_id[sentlgth == "1-1.9 years"]) /
                                               denom_1_1_9_years * 100, digits = 2),

            sentlgth_2_4_9_years_n = n_distinct(unique_id[sentlgth == "2-4.9 years"]),
            sentlgth_2_4_9_years_pct = round(n_distinct(unique_id[sentlgth == "2-4.9 years"]) /
                                               denom_2_4_9_years * 100, digits = 2),

            sentlgth_5_9_9_years_n = n_distinct(unique_id[sentlgth == "5-9.9 years"]),
            sentlgth_5_9_9_years_pct = round(n_distinct(unique_id[sentlgth == "5-9.9 years"]) /
                                               denom_5_9_9_years * 100, digits = 2),

            sentlgth_10_24_9_years_n = n_distinct(unique_id[sentlgth == "10-24.9 years"]),
            sentlgth_10_24_9_years_pct = round(n_distinct(unique_id[sentlgth == "10-24.9 years"]) /
                                                 denom_10_24_9_years * 100, digits = 2),

            sentlgth___25_years_n = n_distinct(unique_id[sentlgth == ">=25 years"]),
            sentlgth___25_years_pct = round(n_distinct(unique_id[sentlgth == ">=25 years"]) /
                                              denom___25_years * 100, digits = 2)
  ) %>%
  ungroup()

# select variables
ncrp_rri <- ncrp_race_2020_table %>%
  select(race_eth,
         people_in_prison_pct,
         sentlgth_1_year_pct,
         sentlgth_1_1_9_years_pct,
         sentlgth_2_4_9_years_pct,
         sentlgth_5_9_9_years_pct,
         sentlgth_10_24_9_years_pct,
         sentlgth___25_years_pct)

# reference index is "White, non-Hispanic"
reference_index <- which(ncrp_race_2020_table$race_eth == "White, non-Hispanic")


fnc_generate_tooltip <- function(sample, rri, race_eth) {
  if (rri < 1) {
    likelihood_text <- "less likely"
    rri_text <- rri
  } else if (rri == 1) {
    likelihood_text <- "equally as likely"
    rri_text <- ""
  } else {
    likelihood_text <- "more likely"
    rri_text <- rri
  }

  paste(race_eth, "are", rri_text, "times", likelihood_text,
        "to", sample, "than White people.")
}

# calculate RRI by variable
ncrp_rri_table <- ncrp_rri %>%
  mutate(
    reference_rate_people_in_prison = people_in_prison_pct[reference_index],
    rri_people_in_prison            = people_in_prison_pct / reference_rate_people_in_prison,

    reference_rate_sentlgth_1_year  = sentlgth_1_year_pct[reference_index],
    rri_sentlgth_1_year             = sentlgth_1_year_pct / reference_rate_sentlgth_1_year,

    reference_rate_sentlgth_1_1_9   = sentlgth_1_1_9_years_pct[reference_index],
    rri_sentlgth_1_1_9              = sentlgth_1_1_9_years_pct / reference_rate_sentlgth_1_1_9,

    reference_rate_sentlgth_2_4_9    = sentlgth_2_4_9_years_pct[reference_index],
    rri_sentlgth_2_4_9               = sentlgth_2_4_9_years_pct / reference_rate_sentlgth_2_4_9,

    reference_rate_sentlgth_5_9_9    = sentlgth_5_9_9_years_pct[reference_index],
    rri_sentlgth_5_9_9               = sentlgth_5_9_9_years_pct / reference_rate_sentlgth_5_9_9,

    reference_rate_sentlgth_10_24_9  = sentlgth_10_24_9_years_pct[reference_index],
    rri_sentlgth_10_24_9             = sentlgth_10_24_9_years_pct / reference_rate_sentlgth_10_24_9,

    reference_rate_sentlgth_25      = sentlgth___25_years_pct[reference_index],
    rri_sentlgth_25                 = sentlgth___25_years_pct / reference_rate_sentlgth_25
  ) %>%
  select(matches("^rri|race_eth$")) %>%
  gather(sample, rri, rri_people_in_prison:rri_sentlgth_25) %>%

  # remove White
  filter(race_eth != "White, non-Hispanic") %>%

  mutate(color = ifelse(rri < 1, "#ff640080", "#ff6400"),
         sample = case_when(
           sample == "rri_people_in_prison" ~ "In Prison",
           sample == "rri_sentlgth_1_year"  ~ "Sentence Length < 1 year",
           sample == "rri_sentlgth_1_1_9"   ~ "Sentence Length 1-1.9 years",
           sample == "rri_sentlgth_2_4_9"   ~ "Sentence Length 2-4.9 years",
           sample == "rri_sentlgth_5_9_9"   ~ "Sentence Length 5-9.9 years",
           sample == "rri_sentlgth_10_24_9" ~ "Sentence Length 10-24.9 years",
           sample == "rri_sentlgth_25"      ~ "Sentence Length >=25 years"
         ),
         rri = round(rri, 1),
         tooltip = case_when(
           sample == "In Prison" & rri < 1 ~ paste("Black, non-Hispanic people are", rri, "times less likely to be in prison than White people."),
           sample == "In Prison" & rri == 1 ~ "Black, non-Hispanic people are equally as likely to be in prison as White people.",
           sample == "In Prison" & rri > 1 ~ paste("Black, non-Hispanic people are", rri, "times more likely to be in prison than White people."),

           sample == "Sentence Length < 1 year" & rri < 1 ~ paste("Black, non-Hispanic people are", rri, "times less likely to have a sentence length of < 1 year than White people."),
           sample == "Sentence Length < 1 year" & rri == 1 ~ "Black, non-Hispanic people are equally as likely to have a sentence length of < 1 year as White people.",
           sample == "Sentence Length < 1 year" & rri > 1 ~ paste("Black, non-Hispanic people are", rri, "times more likely to have a sentence length of < 1 year than White people."),

           sample == "Sentence Length 1-1.9 years" & rri < 1 ~ paste("Black, non-Hispanic people are", rri, "times less likely to have a sentence length of 1-1.9 years than White people."),
           sample == "Sentence Length 1-1.9 years" & rri == 1 ~ "Black, non-Hispanic people are equally as likely to have a sentence length of 1-1.9 years as White people.",
           sample == "Sentence Length 1-1.9 years" & rri > 1 ~ paste("Black, non-Hispanic people are", rri, "times more likely to have a sentence length of 1-1.9 years than White people."),

           sample == "Sentence Length 2-4.9 years" & rri < 1 ~ paste("Black, non-Hispanic people are", rri, "times less likely to have a sentence length of 2-4.9 years than White people."),
           sample == "Sentence Length 2-4.9 years" & rri == 1 ~ "Black, non-Hispanic people are equally as likely to have a sentence length of 2-4.9 years as White people.",
           sample == "Sentence Length 2-4.9 years" & rri > 1 ~ paste("Black, non-Hispanic people are", rri, "times more likely to have a sentence length of 2-4.9 years than White people."),

           sample == "Sentence Length 5-9.9 years" & rri < 1 ~ paste("Black, non-Hispanic people are", rri, "times less likely to have a sentence length of 5-9.9 years than White people."),
           sample == "Sentence Length 5-9.9 years" & rri == 1 ~ "Black, non-Hispanic people are equally as likely to have a sentence length of 5-9.9 years as White people.",
           sample == "Sentence Length 5-9.9 years" & rri > 1 ~ paste("Black, non-Hispanic people are", rri, "times more likely to have a sentence length of 5-9.9 years than White people."),

           sample == "Sentence Length 10-24.9 years" & rri < 1 ~ paste("Black, non-Hispanic people are", rri, "times less likely to have a sentence length of 10-24.9 years than White people."),
           sample == "Sentence Length 10-24.9 years" & rri == 1 ~ "Black, non-Hispanic people are equally as likely to have a sentence length of 10-24.9 years as White people.",
           sample == "Sentence Length 10-24.9 years" & rri > 1 ~ paste("Black, non-Hispanic people are", rri, "times more likely to have a sentence length of 10-24.9 years than White people."),

           sample == "Sentence Length >=25 years" & rri < 1 ~ paste("Black, non-Hispanic people are", rri, "times less likely to have a sentence length of >=25 years than White people."),
           sample == "Sentence Length >=25 years" & rri == 1 ~ "Black, non-Hispanic people are equally as likely to have a sentence length of >=25 years as White people.",
           sample == "Sentence Length >=25 years" & rri > 1 ~ paste("Black, non-Hispanic people are", rri, "times more likely to have a sentence length of >=25 years than White people."),

           TRUE ~ NA_character_
         ))


categories_list <- list("0"=" ","1"="1 = White Reference Line","2"=" ","3"=" ")

data1 <- ncrp_rri_table %>%
  filter(race_eth == "Black, non-Hispanic") %>%
  mutate(color = ifelse(rri < 1, "#ff640080", "#ff6400"),
         type = ifelse(rri < 1, "Underrepresented", "Overrepresented"))

chart <- data1 %>%
  hchart(type = "bar", hcaes(x = "sample", y = "rri", group = "type")) %>%
  hc_title(text = "Black, non-Hispanic People") %>%
  hc_subtitle(text = "Relative Rate Index") %>%
  hc_xAxis(title = "",
           categories = c(
             "In Prison",
             "Sentence Length < 1 year",
             "Sentence Length 1-1.9 years",
             "Sentence Length 2-4.9 years",
             "Sentence Length 5-9.9 years",
             "Sentence Length 10-24.9 years",
             "Sentence Length >=25 years"
           )) %>%
  hc_yAxis(title = "",
           categories = categories_list,
           min = 0,
           max = 3,
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
                 ))

chart



