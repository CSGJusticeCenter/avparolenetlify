


#-------------------------------------------------------------------------------
# DATA PREPARATION
#-------------------------------------------------------------------------------

#' Filter prison population data by admtype and sentence length
#'
#' This function filters the prison population data to include only those with
#' a specific admission type (e.g., "New court commitment") and sentence lengths
#' between 1-25 years.
#'
#' @param data A dataframe containing the prison population data.
#' @param admtype_filter A string indicating the admission type to filter by (default is "New court commitment").
#' @param sentence_lengths A vector of strings indicating the sentence lengths to include (default includes "1-1.9 years", "2-4.9 years", "5-9.9 years", "10-24.9 years").
#' @return A filtered dataframe with prison population data based on the given admission type and sentence lengths.
#' @export
#' @examples
#' ncrp_filtered <- filter_population_criteria(ncrp_yearendpop)
filter_population_criteria <- function(data,
                               admtype_filter = "New court commitment",
                               sentence_lengths = c("1-1.9 years",
                                                    "2-4.9 years",
                                                    "5-9.9 years",
                                                    "10-24.9 years")) {
  filtered_data <- data |>
    filter(admtype == admtype_filter) |>
    filter(sentlgth %in% sentence_lengths)

  return(filtered_data)
}



#-------------------------------------------------------------------------------
# VISUALIZATIONS
#-------------------------------------------------------------------------------
