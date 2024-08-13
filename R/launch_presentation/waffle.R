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

# Define the function to encode the SVG icon
encode_icon <- function(color) {
  iconSVG <- sprintf(
    "<svg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24'>
      <path d='M12 12c2.21 0 4-1.79 4-4s-1.79-4-4-4-4 1.79-4 4 1.79 4 4 4zm0 2c-2.67 0-8 1.34-8 4v2h16v-2c0-2.66-5.33-4-8-4z' fill='%s'/>
    </svg>",
    color
  )
  base64encode(charToRaw(iconSVG))
}

hc_prison_adm_types <- highchart() |>
  hc_chart(type = "item"
  ) |>
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
          marker = list(symbol = 'square')
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
        return '<b>' + this.point.name + ':</b> ' + this.y + '%';
      }")
  ) |>
  hc_add_theme(base_hc_theme)

hc_prison_adm_types






#
#
# # Convert the waffle chart to a plotly object
# waffle_plotly <- plot_ly() |>
#   add_trace(
#     type = 'scatter3d',
#     mode = 'markers',
#     x = rep(1:10, each = 10),
#     y = rep(1:10, times = 10),
#     z = rep(0, 100),
#     marker = list(
#       size = 8,
#       color = as.factor(c(rep("Category A", 50), rep("Category B", 30), rep("Category C", 20))),
#       colorscale = list(c(0, "#FF9999"), c(0.5, "#99CC99"), c(1, "#9999FF"))
#     )
#   ) |>
#   layout(
#     scene = list(
#       xaxis = list(title = 'X-axis'),
#       yaxis = list(title = 'Y-axis'),
#       zaxis = list(title = 'Z-axis')
#     ),
#     title = '3D Waffle Chart'
#   )
#
# # Print the 3D waffle chart
# print(waffle_plotly)
