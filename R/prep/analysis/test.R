fnc_generate_columnchart_sentence <- function(state_var, df, x_var, type) {

  df1 <- df |>
    filter(state == state_var) |>
    arrange(-prop)

  year <- unique(df1$rptyear)

  # If there's not enough data, return a missing data message
  if (nrow(df1) < 1 || is.na(df1$prop[1])) {
    return(paste0("Data for ", state_var, " is missing or incomplete."))
  }

  # Check if x_var is "sex" to format to lowercase
  if (x_var == "sex") {
    df1[[x_var]] <- tolower(df1[[x_var]])
  }

  # Special handling for "fbi_index"
  if (x_var == "fbi_index") {
    # Get the top categories based on the highest proportion
    max_prop <- max(round(df1$prop, 0))
    top_categories <- df1 |>
      filter(round(prop, 0) == max_prop) |>
      arrange(desc(prop))

    # Construct sentences for each top category
    fbi_sentences <- top_categories |>
      mutate(fbi_sentence = paste0(tolower(fbi_index), " (", round(prop, 0), " percent)")) |>
      pull(fbi_sentence)

    # Use commas and "and" to format the final sentence correctly
    fbi_sentence_final <- if (length(fbi_sentences) > 1) {
      paste(paste(fbi_sentences[-length(fbi_sentences)], collapse = ", "),
            ", and ", fbi_sentences[length(fbi_sentences)], sep = "")
    } else {
      fbi_sentences
    }

    # Return the full sentence for fbi_index
    sentences <- paste0("In ", year, ", most people ", type,
                        " were incarcerated for ", fbi_sentence_final, " offenses.")

  } else if (x_var == "ageyrend" | x_var == "agerlse") {
    # Handle age-related variables
    age_range <- strsplit(as.character(df1[[x_var]][1]), "-")[[1]]
    sentences <- paste0("In ", year, ", ", round(df1$prop[1], 0),
                        " percent of people ", type, " were between the ages of ",
                        age_range[1], " and ", age_range[2], " old.")
  } else if (x_var == "sentlgth") {
    # Handle sentence length variables
    sent_range <- strsplit(as.character(df1[[x_var]][1]), "-")[[1]]
    sentences <- paste0("In ", year, ", ", round(df1$prop[1], 0),
                        " percent of people ", type, " had sentence lengths between ",
                        sent_range[1], " and ", sent_range[2], ".")
  } else {
    # General case for other variables
    sentences <- paste0("In ", year, ", ", round(df1$prop[1], 0),
                        " percent of people ", type, " were ",
                        df1[[x_var]][1], ".")
  }

  return(sentences)
}
