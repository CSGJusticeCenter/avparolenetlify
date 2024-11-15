michigan_race_2019_cons <- ncrp_yearendpop_2019_clean_w_imputation_consolidated |>
  filter(state == "Michigan") |>
  group_by(race) |>
  summarise(total = n())

michigan_race_2019 <- ncrp_yearendpop_2019_clean_w_imputation |>
  filter(state == "Michigan") |>
  group_by(race) |>
  summarise(total = n())

arkansas_race_2019_cons <- ncrp_yearendpop_2019_clean_w_imputation_consolidated |>
  filter(state == "Arkansas") |>
  group_by(race) |>
  summarise(total = n())

arkansas_race_2019 <- ncrp_yearendpop_2019_clean_w_imputation |>
  filter(state == "Arkansas") |>
  group_by(race) |>
  summarise(total = n())

southdakota_race_2019_cons <- ncrp_yearendpop_2019_clean_w_imputation_consolidated |>
  filter(state == "South Dakota") |>
  group_by(race) |>
  summarise(total = n())

southdakota_race_2019 <- ncrp_yearendpop_2019_clean_w_imputation |>
  filter(state == "South Dakota") |>
  group_by(race) |>
  summarise(total = n())
