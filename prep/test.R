################################################################################

# Percentage/number of people who maxed out even though they were parole eligible

# Obtained from NCRP releases (ncrp_releases)

################################################################################

# Flag whether people maxed out and the type (i.e., Maxed out and was Parole Eligible Prior to Release Year)
# Remove rows where mand_prisrel_year, parelig_year, and relyr are NA
# Filter to people in prison for a new crime
ncrp_releases_maxout_2020 <- ncrp_releases_2020 %>%
  filter(admtype == "New court commitment") %>%
  filter(!is.na(mand_prisrel_year) &
           !is.na(parelig_year) &
           !is.na(relyr)) %>%
  mutate(maxout = ifelse(mand_prisrel_year == relyr, 1, 0),

         release_timing_type = case_when(
           mand_prisrel_year == relyr ~ "Released on Mandatory Release Year",
           mand_prisrel_year > relyr  ~ "Released Prior to Mandatory Release Year",
           mand_prisrel_year < relyr  ~ "Released After Mandatory Release Year",
           TRUE ~ "Other"),

         maxout_type = case_when(
           mand_prisrel_year == relyr & parelig_year < relyr  ~ "Maxed out and was Parole Eligible Prior to Release Year",
           mand_prisrel_year == relyr & parelig_year > relyr  ~ "Maxed out and was Parole Eligible After Release Year",
           mand_prisrel_year == relyr & parelig_year == relyr ~ "Maxed out and was Parole Eligible During Release Year",
           TRUE ~ "Other")
  ) %>%
  select(mand_prisrel_year, parelig_year, relyr, maxout, release_timing_type, maxout_type, everything())
  #filter(release_timing_type != "Released After Mandatory Release Year") #############################################????????

# get number and proportion of people who maxed out even though they were parole eligible
releases_maxout_2020 <- ncrp_releases_maxout_2020 %>%
  group_by(state) %>%
  count(release_timing_type, maxout_type) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         chart_label = paste0(release_timing_type, " <b>", prop_label, "</b>")) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "<b>",
                  release_timing_type,
                  "</b><br><br>",
                  "Number of People: <b>",
                  scales::comma(n),
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))

data1 <- data.frame(
  id = c("Root", "A", "B", "C", "A1", "A2", "B1", "B2", "C1"),
  parent = c("", "Root", "Root", "Root", "A", "A", "B", "B", "C"),
  value = c(100, 40, 30, 30, 20, 20, 10, 20, 30)
)

data <- ncrp_releases_maxout_2020 %>%
  ungroup() %>%
  filter(state == "Georgia") %>%
  droplevels() %>%
  rename(parent = release_timing_type)

root <- data %>%
  summarise(value = n()) %>%
  mutate(parent = "",
         id = "Released from Prison")

A <- data %>%
  filter(parent == "Released Prior to Mandatory Release Year") %>%
  summarise(value = n()) %>%
  mutate(id = "Released Prior to Mandatory Release Year",
         parent = "Released from Prison")

B <- data %>%
  filter(parent == "Released on Mandatory Release Year") %>%
  summarise(value = n()) %>%
  mutate(id = "Released on Mandatory Release Year",
         parent = "Released from Prison")

C <- data %>%
  filter(parent == "Released After Mandatory Release Year") %>%
  summarise(value = n()) %>%
  mutate(id = "Released After Mandatory Release Year",
         parent = "Released from Prison")

ABC <- rbind(root, A, B, C)
ABC <- ABC %>% select(id, parent, value)

B123 <- data %>%
  filter(parent == "Released on Mandatory Release Year") %>%
  count(maxout_type) %>%
  mutate(id = maxout_type,
         parent = "Released on Mandatory Release Year") %>%
  select(id, parent, value = n)

ABC_all <- rbind(ABC, B123)

hchart(ABC_all, "sunburst", hcaes(id = id, parent = parent, value = value))








maxout_ratio_by_state_2020 <- ncrp_releases_maxout_2020 %>%
  group_by(state) %>%
  summarize(
    total_cases = n(),
    maxout_pe_prior_cases = sum(release_timing_type == "Maxed out and was Parole Eligible Prior to Release Year"),
    ratio = maxout_pe_prior_cases / total_cases,
    representation = 1 / ratio  # Calculating the "1 in X" representation
  )









