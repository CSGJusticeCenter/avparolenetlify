
temp <- ncrp_releases_combined |>
  filter(rptyear == 2020) |>
  group_by(calc_sent_lgth_compl, sentlgth) |>
  summarise(total = n())
