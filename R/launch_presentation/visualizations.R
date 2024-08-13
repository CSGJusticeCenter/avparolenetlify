


# Combine the data into a single dataframe
avg_daily_pop <- rbind(
  data.frame(year = avg_daily_jail_pop$fy, population = avg_daily_jail_pop$average_daily_pop, type = "Jail"),
  data.frame(year = avg_daily_doc_pop$fy, population = avg_daily_doc_pop$average_daily_pop, type = "DOC")
)

# Find the maximum y value for adjusting the y-axis
max_y_value <- max(avg_daily_pop$population) * 1.1

# LINEPLOT
gg_avg_daily_pop <- ggplot(avg_daily_pop, aes(x = year, y = population, color = type, label = scales::comma(population))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  shadowtext::geom_shadowtext(vjust = -0.5, hjust = 0.5, size = 5, fontface = "bold", bg.color = "white", show.legend = FALSE) +
  scale_color_manual(values = c("Jail" = orange, "DOC" = blue)) +
  scale_y_continuous(labels = scales::comma, limits = c(4000, max_y_value)) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal(base_family = "Franklin Gothic Book") +
  theme(
    text = element_text(family = "Franklin Gothic Book", size = 14),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    legend.key = element_rect(linetype = 0), legend.key.size = unit(1.2,
                                                                    "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(1)),
    legend.text.align = NULL,
    legend.title = element_blank(),
    legend.title.align = NULL,
    legend.position = "top",
    legend.direction = NULL,
    legend.justification = "left",
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgray"),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

gg_avg_daily_pop




# Create the area chart
hc_prion_adm_pp_rev <- highchart() |>
  hc_chart(type = "area") |>
  hc_xAxis(categories = doc_commitments$year) |>
  hc_tooltip(shared = TRUE, crosshairs = TRUE) |>
  hc_add_series(name = "Parole Technical Revocation",
                data = doc_commitments$pvt_parolee_tech_rev,
                color = darkorange) |>
  hc_add_series(name = "Probation Technical Revocation",
                data = doc_commitments$probation_violator_tech,
                color = orange) |>
  hc_plotOptions(
    area = list(
      stacking = 'normal',
      marker = list(enabled = FALSE),
      accessibility = list(
        description = "This chart shows the number of prison admissions by people
        who were revoked on parolee and probation over the years 2018 to 2023.",
        enabled = TRUE
      )
    )
  ) |>
  hc_add_theme(base_hc_theme) |>
  hc_exporting(enabled = TRUE) |>
  hc_yAxis(max = 1300)
hc_prion_adm_pp_rev




# List of variables to exclude
exclude_vars <- c("home_confinement_revoked", "regular", "confinement_parole_revoked",
                  "escapees_returned_to_custody", "returned_by_judge_as_fit", "community_corr_rev",
                  "sex_offenders_released_revocation", "mandatory_release_revoked",
                  "conditional", "non_violent", "acc", "diagnostic")

data <- doc_commitments %>%
  filter(year == 2023) %>%
  select(-all_of(exclude_vars)) %>%
  mutate(
    other = total - (pvt_parolee_tech_rev + pvf_parolee_new_fel + prp_parolee_pending + probation_violator_felony + probation_violator_tech),
    across(c(pvt_parolee_tech_rev, pvf_parolee_new_fel, prp_parolee_pending, probation_violator_felony, probation_violator_tech, other), ~ .x / total, .names = "{col}_prop")
  ) %>%
  pivot_longer(
    cols = -c(year, total),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    type = if_else(str_detect(variable, "_prop$"), "prop", "n"),
    variable = str_remove(variable, "_prop$")
  ) %>%
  pivot_wider(
    names_from = type,
    values_from = value
  ) %>%
  filter(variable != "total") %>%
  group_by(variable = case_when(
    variable %in% c("pvf_parolee_new_fel", "probation_violator_felony", "prp_parolee_pending") ~ "Supervision New Offense Revocation",
    variable %in% c("pvt_parolee_tech_rev", "probation_violator_tech") ~ "Supervision Technical Revocation",
    variable == "other" ~ "New Offense/Other Admission",
    TRUE ~ variable
  ), year) %>%
  summarise(
    n = sum(n, na.rm = TRUE),
    prop = round(sum(prop, na.rm = TRUE) * 100, 0)
  ) %>%
  ungroup() %>%
  mutate(
    prop = round(prop / sum(prop) * 100, 0),
    variable = factor(variable, levels = c(
      "Supervision New Offense Revocation",
      "Supervision Technical Revocation",
      "New Offense/Other Admission"))
  ) |>
  arrange(match(variable, c(
    "Supervision Technical Revocation",
    "Supervision New Offense Revocation",
    "New Offense/Other Admission"
  )))

# Define colors for the groups
colors_list <- c(orange, darkblue, lightgray)

hc_prison_adm_types <- highchart() |>
  hc_chart(type = "item", marginLeft = 150, marginRight = 150, marginBottom = 0) |>
  hc_title(text = "") |>
  hc_xAxis(categories = levels(data$variable)) %>%
  hc_yAxis(title = list(text = "Percentage"), max = 100) |>
  hc_series(
    list(
      name = "Percentage",
      data = lapply(1:nrow(data), function(i) {
        list(
          y = data$prop[i],
          name = data$variable[i],
          color = colors_list[i],
          # marker = list(symbol = sprintf("url(data:image/svg+xml;base64,%s)", encode_icon(colors_list[i])))
          marker = list(symbol = 'square'),
          n = data$n[i]  # Include number of admissions
        )
      }),
      type = "item",
      size = '100%',
      itemMargin = 1,
      rows = 10
    )
  ) |>
  hc_tooltip(
    formatter = JS("function() {
        return '<b>' + this.point.name + ':</b> ' + this.y + '%<br/><b>Number of Admissions:</b> ' + this.point.n.toLocaleString();
      }")
  ) |>
  hc_add_theme(base_hc_theme) |>
  hc_exporting(enabled = TRUE)

hc_prison_adm_types





# Jail Capacity Bar Chart
# Create the stacked bar chart
hc_jail_capacity <- highchart() |>
  hc_chart(type = "column") |>
  hc_title(text = "5 Example Jails That Are Overcapacity") |>
  hc_xAxis(categories = jail_capacity$jail) |>
  hc_yAxis(title = list(text = "")) |>
  hc_series(
    list(
      name = "Overcapacity",
      data = jail_capacity$overcapacity
    ),
    list(
      name = "Capacity",
      data = jail_capacity$capacity
    )) |>
  hc_plotOptions(series = list(stacking = "normal")) |>
  hc_tooltip(shared = TRUE, crosshairs = TRUE) |>
  hc_add_theme(base_hc_theme) |>
  hc_exporting(enabled = TRUE) |>
  hc_colors(c(red, blue))








#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){
  save(hc_prion_adm_pp_rev, file = file.path(folder, "hc_prion_adm_pp_rev.rds"))
  save(hc_prison_adm_types, file = file.path(folder, "hc_prison_adm_types.rds"))
  save(hc_jail_capacity, file = file.path(folder, "hc_jail_capacity.rds"))

}





# # BUBBLE CHART
# # Find the maximum y value for adjusting the y-axis
# max_y_value <- max(avg_daily_pop$population) * 1.1
#
# # Define the custom theme
# custom_theme <- function() {
#   theme_minimal(base_family = "Franklin Gothic Book") +
#     theme(
#       text = element_text(family = "Franklin Gothic Book", size = 14),
#       plot.title = element_text(size = 16),
#       axis.title = element_text(size = 14),
#       axis.text.x = element_text(size = 12),
#       axis.text.y = element_text(size = 12),
#       axis.ticks.x = element_line(color = "black"),
#       axis.ticks.y = element_blank(),
#       axis.line.x = element_line(color = "black"),
#       axis.line.y = element_blank(),
#       legend.position = "none",  # Remove the legend
#
#       # legend.key = element_rect(linetype = 0),
#       # legend.key.size = unit(1.2, "lines"),
#       # legend.text = element_text(size = 14),
#       # legend.position = c(0.1, 1),  # Adjust this value to move the legend slightly to the left
#       # legend.direction = "horizontal",
#       # legend.justification = c("left", "top"),
#       # legend.title = element_blank(),
#       #panel.grid.minor.y = element_blank(),
#       panel.grid.minor.x = element_blank(),
#       #panel.grid.major.y = element_blank(),
#       #panel.grid.major.x = element_blank(),
#       panel.background = element_rect(fill = "white", color = NA),
#       plot.background = element_rect(fill = "white", color = NA)
#     )
# }
#
# # Plot the data
# p <- ggplot(avg_daily_pop, aes(x = year, y = population, color = type, size = population, label = scales::comma(population))) +
#   geom_point(alpha = 0.8) +
#   shadowtext::geom_shadowtext(vjust = 0.5, hjust = 0.5, size = 5, fontface = "bold", bg.color = "white", show.legend = FALSE) +
#   scale_color_manual(values = c("Jail" = orange, "DOC" = blue)) +
#   scale_size_area(max_size = 30) +
#   #scale_size(range = c(10, 30), guide = "none") +
#   scale_y_continuous(labels = scales::comma, limits = c(4000, max_y_value)) +
#   scale_x_continuous(expand = expansion(mult = c(0.15, 0.15))) +  # Add space on the left
#   labs(title = "",
#        x = "",
#        y = "") +
#   custom_theme()
#
# p
