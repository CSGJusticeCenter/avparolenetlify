
ncrp_yearendpop_2017_clean_w_imputation_consolidated <- read_dta("C:/Users/mroberts/The Council of State Governments/JC Research - Documents/RES_Parole/data/analysis/clean_files/cleaning_processing/ncrp_yearendpop_2017_clean_w_imputation_consolidated.dta")

michigan <- ncrp_yearendpop_2017_clean_w_imputation_consolidated |>
  filter(state == "Michigan") |>
  filter(rptyear == 2017)
nrow(michigan) #39650
sum(is.na(michigan$admtype)) # 0
table(michigan$admtype)

# summary(michigan$years_to_estimated_pey)
# summary(michigan$time_between_ped_rptyear)

michigan <- ncrp_yearendpop_consolidated |>
  filter(state == "Michigan") |>
  filter(rptyear == 2017)
nrow(michigan) # 39650
sum(is.na(michigan$admtype)) # 0
table(michigan$admtype)
summary(michigan$years_to_estimated_pey)
summary(michigan$time_between_ped_rptyear)


# 79325

ncrp_yearendpop_filtered <- fnc_filter_pe_population_criteria(data = ncrp_yearendpop_consolidated,
                                                              exclude = states_to_exclude,
                                                              dont_filter = states_nofilter)
michigan <- ncrp_yearendpop_filtered |>
  filter(state == "Michigan") |>
  filter(rptyear == 2017)
nrow(michigan)
# 24036

times <- michigan |>
  group_by(years_to_estimated_pey) |>
  summarise(total = n())


# Calculate the total prison population by state and reporting year
# This serves as the denominator for proportion calculations later
# Each row is a person for that rptyear
mi_pe_pop_by_rptyear <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  summarise(yearendpop = n(), .groups = "drop") |>
  filter(state == "Michigan") |>
  filter(rptyear == 2017)
print(mi_pe_pop_by_rptyear)
# 24036

mi_pe_status_pop <- ncrp_yearendpop_filtered |>
  filter(state == "Michigan" & rptyear == 2017) |>
  mutate(estimated_pey_status_new =
           case_when(
             estimated_pey_status %in% c("current_year", "past") ~ "Past Parole Eligibility at End of Year",
             estimated_pey_status == "future" & years_to_estimated_pey == 1 ~ "Will Be Eligible Next Year",
             estimated_pey_status == "future" & years_to_estimated_pey > 1 ~ "Will Be Eligible In 1+ Years",
             estimated_pey_status == "missing" ~ "Missing Data or Possibly Never Eligible",
             TRUE ~ estimated_pey_status))

table(mi_pe_status_pop$estimated_pey_status_new)
table(mi_pe_status_pop$estimated_pey_status)
