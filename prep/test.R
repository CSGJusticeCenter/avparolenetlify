temp <- ncrp_yearendpop %>%
  # filter(parelig_status == "Missing") %>%
  filter(rptyear == 2020) %>%
  group_by(state, offgeneral) %>%
  count(parelig_status) %>%
  mutate(prop = n/sum(n),
         yearendpop = sum(n))
