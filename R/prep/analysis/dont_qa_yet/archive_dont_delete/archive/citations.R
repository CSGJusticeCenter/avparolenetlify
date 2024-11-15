fnc_format_citations_column <- function(citations) {
  library(stringr)

  formatted_citations <- sapply(citations, function(citation) {
    # Italicize any report titles within quotes
    formatted_citation <- str_replace_all(
      citation,
      "\"([^\"]+)\"",
      function(x) paste0("*", str_replace_all(x, "\"", ""), "*")
    )

    # Convert URLs to markdown hyperlinks and ensure the period is outside the link
    formatted_citation <- str_replace_all(
      formatted_citation,
      "(https?://[^\\s,]+)\\.?\\,?",  # Match the URL pattern followed by a period or comma
      function(x) {
        url <- str_remove_all(x, "[\\.,]$")  # Remove the trailing period/comma from the URL
        paste0("[", url, "](", url, ")")  # Format URL as markdown link
      }
    )

    return(formatted_citation)
  })

  return(formatted_citations)
}

# Function to apply formatting to multiple columns of a data frame
format_citation_columns <- function(df, columns) {
  for (col in columns) {
    df[[col]] <- fnc_format_citations_column(df[[col]])
  }
  return(df)
}

# Example usage
df_citations <- data.frame(
  col1 = c(
    '² US Bureau of Justice Statistics., "National Corrections Reporting Program NCRP Series" (Inter-university Consortium for Political and Social Research, 2022), accessed October 2, 2024, https://www.icpsr.umich.edu/web/NACJD/series/38.',
    '³ Kevin Reitz et al., American Prison-Release Systems: Indeterminacy in Sentencing and the Control of Prison Population Size, Final Report (Minneapolis, Minnesota: Robina Institute, University of Minnesota, 2022), accessed October 1, 2024, https://robinainstitute.umn.edu/sites/robinainstitute.umn.edu/files/2022-05/american_prison-release_systems.pdf, 36-43.'
  ),
  col2 = c(
    '⁴ US Bureau of Justice Statistics, "National Prisoner Statistics, 1978-2022" (Inter-university Consortium for Political and Social Research), accessed January 10, 2024, https://doi.org/10.3886/ICPSR38871.v1.Connecticut Department of Correction Research Unit, Average Confined Inmate Population and Legal Status (Wethersfield, CT: Connecticut Department of Correction, 2024), accessed October 1, 2024, https://portal.ct.gov/-/media/doc/pdf/monthlystat/stat01012024.pdf, 1.',
    'Some other citation here without a URL.'
  )
)

# Columns to format
columns_to_format <- c("col1", "col2")

# Apply the formatting function
df_formatted <- format_citation_columns(df_citations, columns_to_format)

# View formatted result
df_formatted
