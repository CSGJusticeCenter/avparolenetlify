# Load required libraries
library(reactable)
library(scales)

# Create a sample data frame
sample_data <- data.frame(
  Name = c("Alice", "Bob", "Charlie", "David", "Eve"),
  Score = c(.9, .8, .7, NA, NA)
)

# Function to create data bars
data_bars_custom <- function(value) {
  if (is.na(value)) {
    return("")
  }
  percentage <- percent(value)
  bar <- div(style = list(
    background = "teal",
    width = percentage,
    height = "10px"
  ))
  div(style = list(display = "flex", alignItems = "center"),
      bar, div(style = list(marginLeft = "8px"), percentage))
}

# Create the reactable
reactable(
  sample_data,
  pagination = FALSE,
  columns = list(
    Score = colDef(
      cell = data_bars_custom,
      align = "left",
      width = 150
    )
  )
)



style_cells <- function(value, color) {
  tags$span(style = paste("color:", color, ";"), value)
}

parole_eligibility_table$filtered_total_pop <- as.numeric(parole_eligibility_table$filtered_total_pop)

# https://kcuilla.github.io/reactablefmtr/articles/data_bars.html
reactable(
  parole_eligibility_table,
  pagination = FALSE,
  searchable = TRUE,
  defaultSortOrder = "desc",
  defaultSorted = "current_perc",
  defaultColDef = colDef(
    headerStyle = list(fontWeight = "normal"),
    sortable = TRUE,
    na = "-"
  ),
  columns = list(
    state = colDef(name = "State",
                   align = "left",
                   width = 175,
                   cell = function(value) {
                     url <- paste0("https://avparoleproject.netlify.app/state_report_", tolower(gsub(" ", "_", value)))
                     tags$a(href = url, target = "_blank", style = "color:black;", value)
                   }),
    current_perc = colDef(
      name = "Pct. of People in Prison",
      align = "left",
      width = 150,
      cell = data_bars(
        data = parole_eligibility_table,
        text_position = "outside-end",
        number_fmt = scales::percent,
        fill_color = colors$green2,
        background = "transparent"
      )
    ),
    current_count = colDef(name = "Number of People",
                           align = "right",
                           format = colFormat(separators = TRUE)),
    filtered_total_pop = colDef(name = "Prison Population*",
                                align = "right",
                                format = colFormat(separators = TRUE)                           ),
    abolished_discretionary_parole = colDef(name = "Abolished Discretionary Parole",
                                            align = "right"
    ),
    parole_board_members = colDef(name = "Parole Board Members",
                                  align = "right"
    ),
    ratio = colDef(show = F, name = "Ratio",
                   align = "center")
  ),
  columnGroups = list(
    colGroup(name = "In Prison Past Their Parole Eligibility Date", columns = c("current_perc", "current_count"))
  )
)
