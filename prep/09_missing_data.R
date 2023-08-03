#######################################
# Project: AV Parole
# File: missing_data.R
# Authors: Mari Roberts
# Date last updated: August 3, 2023 (MAR)
# Description:
#    Understanding missing parole eligibility data
#    Is data missing, does it not exist, or are people ineligible for parole?
#######################################

# load custom functions
source("prep/00_library.R")
source("prep/01_functions.R")

################################################################################

# Prepare data to analyze missingness by state

# Obtained from NCRP year end population

################################################################################

# get data by state
cross_tab_data <- ncrp_yearendpop %>%
  filter(!is.na(sentlgth) &
           !is.na(offgeneral)) %>%
  mutate(offgeneral = case_when(offgeneral == "Other/unspecified" ~ "Other or unspecified",
                                TRUE ~ offgeneral),
         offdetail  = case_when(offdetail == " Other/unspecified" ~ "Other or unspecified",
                                TRUE ~ offdetail),
         release_id = row_number(),
         sentlgth =
           factor(sentlgth,
                  levels = c("< 1 year",
                             "1-1.9 years",
                             "2-4.9 years",
                             "5-9.9 years",
                             "10-24.9 years",
                             ">=25 years",
                             "Life, LWOP, Life plus additional years, Death"),
                  ordered = TRUE),
         offgeneral =
           factor(offgeneral,
                  levels = c("Drugs",
                             "Public order",
                             "Property",
                             "Other or unspecified",
                             "Violent",
                             "All Offenses"),
                  ordered = TRUE)) %>%
  filter(rptyear == 2020)








################################################################################

# Missingness by sentence length and parole eligibility status

# Obtained from NCRP year end population

################################################################################

# cross tab by state
all_cross_tab <- cross_tab_data %>%
  group_by(state) %>%
  count(sentlgth, parelig_status) %>%
  pivot_wider(names_from = parelig_status, values_from = n, values_fill = 0) %>%
  clean_names() %>%
  mutate(total =
           missing +
           current +
           future_1_5_years +
           future_6_years) %>%
  mutate_at(vars(missing:future_6_years), list(percent = ~ (. / total))) %>%
  mutate(state = as.factor(state),
         sentlgth = as.factor(sentlgth))


# reactable table
all_cross_tab_sentlgth <-
  reactable(all_cross_tab,
            elementId = "filter-select",
            style = list(
              fontFamily = "Graphik, sans-serif",
              fontSize = "0.8rem",
              color = neutralBlackText
            ),
            theme = hc_reactable_theme,
            defaultColDef = colDef(format = colFormat(separators = TRUE),
                                   align = "right",
                                   minWidth = 70),
            groupBy = "state",
            # pagination = FALSE,
            pageSize = Inf,
            compact = TRUE,
            filterable = TRUE,
            defaultExpanded = TRUE,
            defaultPageSize = 3,
            columnGroups = list(
              colGroup(name = "Number of People",
                       columns = c("current",
                                   "future_1_5_years",
                                   "future_6_years",
                                   "missing")),
              colGroup(name = "Percentage of the Prison Population",
                       columns = c("current_percent",
                                   "future_1_5_years_percent",
                                   "future_6_years_percent",
                                   "missing_percent"))),
            columns = list(
              state         = colDef(name = "State",
                                     minWidth = 120,
                                     align = "left",
                                     style = list(fontWeight = "bold"),
                                     filterInput = function(values, name) {
                                       tags$select(
                                         onchange = sprintf("Reactable.setFilter('filter-select', '%s', event.target.value || undefined)", name),
                                         tags$option(value = "", "All"),
                                         lapply(unique(values), tags$option),
                                         "aria-label" = sprintf("Filter %s", name),
                                         style = "width: 100%; height: 28px;"
                                       )
                                     }),
              sentlgth      = colDef(name = "Sentence Length",
                                       #style = fnc_highlight_cell
                                       style = list(fontWeight = "bold"),
                                       minWidth = 120,
                                       align = "left",
                                     filterInput = function(values, name) {
                                       tags$select(
                                         onchange = sprintf("Reactable.setFilter('filter-select', '%s', event.target.value || undefined)", name),
                                         tags$option(value = "", "All"),
                                         lapply(unique(values), tags$option),
                                         "aria-label" = sprintf("Filter %s", name),
                                         style = "width: 100%; height: 28px;"
                                       )
                                     }),

              current          = colDef(name = "Current"),
              future_1_5_years = colDef(name = "Future 1-5 Years"),
              future_6_years   = colDef(name = "Future 6+ Years"),
              missing          = colDef(name = "Missing"),

              current_percent          = colDef(name = "Current",
                                                format = colFormat(percent = TRUE, digits = 0)),
              future_1_5_years_percent = colDef(name = "Future 1-5 Years",
                                                format = colFormat(percent = TRUE, digits = 0)),
              future_6_years_percent   = colDef(name = "Future 6+ Years",
                                                format = colFormat(percent = TRUE, digits = 0)),
              missing_percent          = colDef(name = "Missing",
                                                style = list(fontWeight = "bold"),
                                                format = colFormat(percent = TRUE, digits = 0)),

              total           = colDef(name = "Total",
                                       style = list(borderRight = "1px solid #d3d3d3"))
            )
  )







################################################################################

# Missingness by offense type and parole eligibility status

# Obtained from NCRP year end population

################################################################################

# cross tab by state
all_cross_tab <- cross_tab_data %>%
  group_by(state) %>%
  count(offgeneral, parelig_status) %>%
  pivot_wider(names_from = parelig_status, values_from = n, values_fill = 0) %>%
  clean_names() %>%
  mutate(total =
           missing +
           current +
           future_1_5_years +
           future_6_years) %>%
  mutate_at(vars(missing:future_6_years), list(percent = ~ (. / total))) %>%
  mutate(state = as.factor(state),
         offgeneral = as.factor(offgeneral))


# reactable table
all_cross_tab_offgeneral <-
  reactable(all_cross_tab,
            elementId = "filter-select",
            style = list(
              fontFamily = "Graphik, sans-serif",
              fontSize = "0.8rem",
              color = neutralBlackText
            ),
            theme = hc_reactable_theme,
            defaultColDef = colDef(format = colFormat(separators = TRUE),
                                   align = "right",
                                   minWidth = 70),
            groupBy = "state",
            # pagination = FALSE,
            pageSize = Inf,
            compact = TRUE,
            filterable = TRUE,
            defaultExpanded = TRUE,
            defaultPageSize = 3,
            columnGroups = list(
              colGroup(name = "Number of People",
                       columns = c("current",
                                   "future_1_5_years",
                                   "future_6_years",
                                   "missing")),
              colGroup(name = "Percentage of the Prison Population",
                       columns = c("current_percent",
                                   "future_1_5_years_percent",
                                   "future_6_years_percent",
                                   "missing_percent"))),
            columns = list(
              state         = colDef(name = "State",
                                     minWidth = 120,
                                     align = "left",
                                     style = list(fontWeight = "bold"),
                                     filterInput = function(values, name) {
                                       tags$select(
                                         onchange = sprintf("Reactable.setFilter('filter-select', '%s', event.target.value || undefined)", name),
                                         tags$option(value = "", "All"),
                                         lapply(unique(values), tags$option),
                                         "aria-label" = sprintf("Filter %s", name),
                                         style = "width: 100%; height: 28px;"
                                       )
                                     }),
              offgeneral      = colDef(name = "Offense Type",
                                       #style = fnc_highlight_cell
                                       style = list(fontWeight = "bold"),
                                       minWidth = 120,
                                       align = "left",
                                       filterInput = function(values, name) {
                                         tags$select(
                                           onchange = sprintf("Reactable.setFilter('filter-select', '%s', event.target.value || undefined)", name),
                                           tags$option(value = "", "All"),
                                           lapply(unique(values), tags$option),
                                           "aria-label" = sprintf("Filter %s", name),
                                           style = "width: 100%; height: 28px;"
                                         )
                                       }),

              current          = colDef(name = "Current"),
              future_1_5_years = colDef(name = "Future 1-5 Years"),
              future_6_years   = colDef(name = "Future 6+ Years"),
              missing          = colDef(name = "Missing"),

              current_percent          = colDef(name = "Current",
                                                format = colFormat(percent = TRUE, digits = 0)),
              future_1_5_years_percent = colDef(name = "Future 1-5 Years",
                                                format = colFormat(percent = TRUE, digits = 0)),
              future_6_years_percent   = colDef(name = "Future 6+ Years",
                                                format = colFormat(percent = TRUE, digits = 0)),
              missing_percent          = colDef(name = "Missing",
                                                style = list(fontWeight = "bold"),
                                                format = colFormat(percent = TRUE, digits = 0)),

              total           = colDef(name = "Total",
                                       style = list(borderRight = "1px solid #d3d3d3"))
            )
  )








################################################################################

# Missingness by offense type and parole eligibility status

# Obtained from NCRP year end population

################################################################################

# cross tab by state
all_cross_tab <- cross_tab_data %>%
  group_by(state) %>%
  count(offdetail, parelig_status) %>%
  pivot_wider(names_from = parelig_status, values_from = n, values_fill = 0) %>%
  clean_names() %>%
  mutate(total =
           missing +
           current +
           future_1_5_years +
           future_6_years) %>%
  mutate_at(vars(missing:future_6_years), list(percent = ~ (. / total))) %>%
  mutate(state = as.factor(state),
         offdetail = as.factor(offdetail))


# reactable table
all_cross_tab_offdetail <-
  reactable(all_cross_tab,
            elementId = "filter-select",
            style = list(
              fontFamily = "Graphik, sans-serif",
              fontSize = "0.8rem",
              color = neutralBlackText
            ),
            theme = hc_reactable_theme,
            defaultColDef = colDef(format = colFormat(separators = TRUE),
                                   align = "right",
                                   minWidth = 70),
            groupBy = "state",
            # pagination = FALSE,
            pageSize = Inf,
            compact = TRUE,
            filterable = TRUE,
            defaultExpanded = TRUE,
            defaultPageSize = 3,
            columnGroups = list(
              colGroup(name = "Number of People",
                       columns = c("current",
                                   "future_1_5_years",
                                   "future_6_years",
                                   "missing")),
              colGroup(name = "Percentage of the Prison Population",
                       columns = c("current_percent",
                                   "future_1_5_years_percent",
                                   "future_6_years_percent",
                                   "missing_percent"))),
            columns = list(
              state         = colDef(name = "State",
                                     minWidth = 120,
                                     align = "left",
                                     style = list(fontWeight = "bold"),
                                     filterInput = function(values, name) {
                                       tags$select(
                                         onchange = sprintf("Reactable.setFilter('filter-select', '%s', event.target.value || undefined)", name),
                                         tags$option(value = "", "All"),
                                         lapply(unique(values), tags$option),
                                         "aria-label" = sprintf("Filter %s", name),
                                         style = "width: 100%; height: 28px;"
                                       )
                                     }),
              offdetail      = colDef(name = "Offense Detail",
                                       #style = fnc_highlight_cell
                                       style = list(fontWeight = "bold"),
                                       minWidth = 120,
                                       align = "left",
                                       filterInput = function(values, name) {
                                         tags$select(
                                           onchange = sprintf("Reactable.setFilter('filter-select', '%s', event.target.value || undefined)", name),
                                           tags$option(value = "", "All"),
                                           lapply(unique(values), tags$option),
                                           "aria-label" = sprintf("Filter %s", name),
                                           style = "width: 100%; height: 28px;"
                                         )
                                       }),

              current          = colDef(name = "Current"),
              future_1_5_years = colDef(name = "Future 1-5 Years"),
              future_6_years   = colDef(name = "Future 6+ Years"),
              missing          = colDef(name = "Missing"),

              current_percent          = colDef(name = "Current",
                                                format = colFormat(percent = TRUE, digits = 0)),
              future_1_5_years_percent = colDef(name = "Future 1-5 Years",
                                                format = colFormat(percent = TRUE, digits = 0)),
              future_6_years_percent   = colDef(name = "Future 6+ Years",
                                                format = colFormat(percent = TRUE, digits = 0)),
              missing_percent          = colDef(name = "Missing",
                                                style = list(fontWeight = "bold"),
                                                format = colFormat(percent = TRUE, digits = 0)),

              total           = colDef(name = "Total",
                                       style = list(borderRight = "1px solid #d3d3d3"))
            )
  )






################################################################################

# Missingness by sentence length and offense type (general)

# Obtained from NCRP year end population (missing data only)

################################################################################

# cross tab by state
all_cross_tab_state <- cross_tab_data %>%
  filter(parelig_status == "Missing") %>%
  group_by(state) %>%
  count(sentlgth, offgeneral) %>%
  pivot_wider(names_from = sentlgth, values_from = n, values_fill = 0) %>%
  clean_names() %>%
  mutate(total =
           x1_year +
           x1_1_9_years +
           x2_4_9_years +
           x5_9_9_years +
           x10_24_9_years +
           x25_years +
           life_lwop_life_plus_additional_years_death) %>%
  mutate_at(vars(x1_year:life_lwop_life_plus_additional_years_death), list(percent = ~ (. / total)))

# cross tab for all offenses by state
all_cross_tab_all <- cross_tab_data %>%
  group_by(state) %>%
  count(sentlgth) %>%
  pivot_wider(names_from = sentlgth, values_from = n, values_fill = 0) %>%
  clean_names() %>%
  mutate(offgeneral = "All Offenses",
         total =
           x1_year +
           x1_1_9_years +
           x2_4_9_years +
           x5_9_9_years +
           x10_24_9_years +
           x25_years +
           life_lwop_life_plus_additional_years_death) %>%
  mutate_at(vars(x1_year:life_lwop_life_plus_additional_years_death), list(percent = ~ (. / total)))

# add data together
all_cross_tab <- rbind(all_cross_tab_state, all_cross_tab_all)
all_cross_tab <- all_cross_tab %>% mutate(state = as.factor(state),
                                          offgeneral = as.factor(offgeneral))

# highlight cells
fnc_highlight_cell <-
  function(value, index) {
    if (all_cross_tab$offgeneral[index] ==
        "All Offenses") {
      color <- orange
    } else {
      color <- neutralBlackText
    }
    list(color = color,
         position = "sticky",
         borderRight = "1px solid #d3d3d3")
  }


# reactable table
all_cross_tab_sentlgth_offgeneral <- reactable(
          all_cross_tab,
            elementId = "filter-select",
          style = list(
            fontFamily = "Graphik, sans-serif",
            fontSize = "0.8rem",
            color = neutralBlackText
          ),
          theme = hc_reactable_theme,
          defaultColDef = colDef(format = colFormat(separators = TRUE),
                                 align = "right",
                                 minWidth = 55),
          groupBy = "state",
          # pagination = FALSE,
          pageSize = Inf,
          compact = TRUE,
          filterable = TRUE,
          defaultExpanded = TRUE,
          defaultPageSize = 3,
          columnGroups = list(
            colGroup(name = "< 1 year",
                     columns = c("x1_year", "x1_year_percent")),
            colGroup(name = "1-1.9 years",
                     columns = c("x1_1_9_years", "x1_1_9_years_percent")),
            colGroup(name = "2-4.9 years",
                     columns = c("x2_4_9_years", "x2_4_9_years_percent")),
            colGroup(name = "5-9.9 years",
                     columns = c("x5_9_9_years", "x5_9_9_years_percent")),
            colGroup(name = "10-24.9 years",
                     columns = c("x10_24_9_years", "x10_24_9_years_percent")),
            colGroup(name = ">=25 years",
                     columns = c("x25_years", "x25_years_percent")),
            colGroup(name = "Life, LWOP, Life plus additional years, Death",
                     columns = c("life_lwop_life_plus_additional_years_death", "life_lwop_life_plus_additional_years_death_percent"))
          ),
          columns = list(
            state         = colDef(name = "State",
                                   minWidth = 120,
                                   align = "left",
                                   style = list(fontWeight = "bold"),
                                   filterInput = function(values, name) {
                                     tags$select(
                                       onchange = sprintf("Reactable.setFilter('filter-select', '%s', event.target.value || undefined)", name),
                                       tags$option(value = "", "All"),
                                       lapply(unique(values), tags$option),
                                       "aria-label" = sprintf("Filter %s", name),
                                       style = "width: 100%; height: 28px;"
                                     )
                                   }
            ),
            offgeneral      = colDef(name = "Offense with Missing PED",
                                     #style = fnc_highlight_cell
                                     style = list(fontWeight = "bold"),
                                     minWidth = 120,
                                     align = "left",
                                     filterInput = function(values, name) {
                                       tags$select(
                                         onchange = sprintf("Reactable.setFilter('filter-select', '%s', event.target.value || undefined)", name),
                                         tags$option(value = "", "All"),
                                         lapply(unique(values), tags$option),
                                         "aria-label" = sprintf("Filter %s", name),
                                         style = "width: 100%; height: 28px;"
                                       )
                                     }),

            x1_year         = colDef(name = "N"),
            x1_year_percent = colDef(name = "%",
                                     style = list(borderRight = "1px solid #d3d3d3"),
                                     format = colFormat(percent = TRUE, digits = 0)),

            x1_1_9_years         = colDef(name = "N"),
            x1_1_9_years_percent = colDef(name = "%",
                                          style = list(borderRight = "1px solid #d3d3d3",
                                                       fontWeight = "bold"),
                                          format = colFormat(percent = TRUE, digits = 0)),

            x2_4_9_years         = colDef(name = "N"),
            x2_4_9_years_percent = colDef(name = "%",
                                          style = list(borderRight = "1px solid #d3d3d3",
                                                       fontWeight = "bold"),
                                          format = colFormat(percent = TRUE, digits = 0)),

            x5_9_9_years  = colDef(name = "N"),
            x5_9_9_years_percent = colDef(name = "%",
                                          style = list(borderRight = "1px solid #d3d3d3",
                                                       fontWeight = "bold"),
                                          format = colFormat(percent = TRUE, digits = 0)),

            x10_24_9_years         = colDef(name = "N"),
            x10_24_9_years_percent = colDef(name = "%",
                                            style = list(borderRight = "1px solid #d3d3d3",
                                                         fontWeight = "bold"),
                                            format = colFormat(percent = TRUE, digits = 0)),

            x25_years         = colDef(name = "N"),
            x25_years_percent = colDef(name = "%",
                                       #style = fnc_highlight_cell,
                                       style = list(borderRight = "1px solid #d3d3d3",
                                                    fontWeight = "bold"),
                                       format = colFormat(percent = TRUE, digits = 0)),

            life_lwop_life_plus_additional_years_death = colDef(name = "N"),
            life_lwop_life_plus_additional_years_death_percent = colDef(name = "%",
                                                                        style = list(borderRight = "1px solid #d3d3d3",
                                                                                     fontWeight = "bold"),
                                                                        format = colFormat(percent = TRUE,
                                                                                           digits = 0)),

            total           = colDef(name = "Total",
                                     minWidth = 100)
          )
)





























# # cross tab by state
# all_cross_tab <- cross_tab_data %>%
#   group_by(state) %>%
#   count(sentlgth, offgeneral) %>%
#   pivot_wider(names_from = offgeneral, values_from = n, values_fill = 0) %>%
#   clean_names() %>%
#   mutate(total = drugs + other_unspecified + property + public_order + violent) %>%
#   mutate_at(vars(drugs:violent), list(percent = ~ (. / total)))
#
# # reactable table
# reactable(all_cross_tab,
#           style = hc_reactable_style,
#           theme = hc_reactable_theme,
#           defaultColDef = colDef(format = colFormat(separators = TRUE),
#                                  align = "right",
#                                  minWidth = 75),
#           compact = TRUE,
#           filterable = TRUE,
#           fullWidth = FALSE,
#           defaultExpanded = TRUE,
#           defaultPageSize = 100,
#           groupBy = "state",
#                     columnGroups = list(
#                       colGroup(name = "Drugs", columns = c("drugs", "drugs_percent")),
#                       colGroup(name = "Other", columns = c("other_unspecified", "other_unspecified_percent")),
#                       colGroup(name = "Property", columns = c("property", "property_percent")),
#                       colGroup(name = "Public Order", columns = c("public_order", "public_order_percent")),
#                       colGroup(name = "Violent", columns = c("violent", "violent_percent"))
#           ),
#           columns = list(
#             state         = colDef(name = "State",
#                                    minWidth = 130,
#                                    align = "left",
#                                    style = list(fontWeight = "bold")
#                                    ),
#             sentlgth      = colDef(name = "Sentence Length",
#                                    minWidth = 140,
#                                    style = function(value, index) {
#                                      if (all_cross_tab$sentlgth[index] ==
#                                          "Life, LWOP, Life plus additional years, Death") {
#                                        color <- orange
#                                      } else {
#                                        color <- neutralBlackText
#                                      }
#                                      list(color = color)
#                                    }),
#
#             drugs         = colDef(name = "N"),
#             drugs_percent = colDef(name = "%",
#                                    format = colFormat(percent = TRUE,
#                                                       digits = 0)),
#
#             other_unspecified         = colDef(name = "N"),
#             other_unspecified_percent = colDef(name = "%",
#                                                format = colFormat(percent = TRUE,
#                                                                   digits = 0)),
#
#             property         = colDef(name = "N"),
#             property_percent = colDef(name = "%",
#                                       format = colFormat(percent = TRUE,
#                                                          digits = 0)),
#
#             public_order  = colDef(name = "N"),
#             public_order_percent = colDef(name = "%",
#                                    format = colFormat(percent = TRUE,
#                                                       digits = 0),
#                                    style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
#
#             violent         = colDef(name = "N"),
#             violent_percent = colDef(name = "%",
#                                style = function(value, index) {
#                                  if (all_cross_tab$sentlgth[index] ==
#                                      "Life, LWOP, Life plus additional years, Death") {
#                                    color <- orange
#                                  } else {
#                                    color <- neutralBlackText
#                                  }
#                                  list(color = color,
#                                       position = "sticky",
#                                       borderRight = "1px solid #d3d3d3")
#                                },
#                                format = colFormat(percent = TRUE,
#                                                   digits = 0)),
#
#             total           = colDef(name = "Total")
#           )
# )

##########
# Save data
##########

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_cross_tab_sentlgth, file=file.path(folder, "all_cross_tab_sentlgth.rds"))
  save(all_cross_tab_offgeneral, file=file.path(folder, "all_cross_tab_offgeneral.rds"))
  save(all_cross_tab_offdetail, file=file.path(folder, "all_cross_tab_offdetail.rds"))
  save(all_cross_tab_sentlgth_offgeneral, file=file.path(folder, "all_cross_tab_sentlgth_offgeneral.rds"))

}
