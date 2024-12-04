# Import state-specific notes about parole eligibility and number of parole board members
state_notes_raw <- read.csv(file.path(config$sp_data_path, "data/raw/Carl State Notes/av_parole_state_notes_v1.csv")) |>
  clean_names() |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(
    state = str_replace_all(state, "\\*", ""),
    citation = sapply(citation, fnc_format_citation)) # format citations

# Import state-specific imputation methodology
state_methodology <- read_dta(file.path(config$sp_data_path, "data/analysis/ncrp_results/state_notes_2020.dta"))

# Combine these together
state_methodology_v1 <- state_notes_raw |>
  left_join(state_methodology, by = "state") |>
  # add period to matching note.
  mutate(matching_note = paste0(matching_note, "."),
         # add superscript 1 to release systems
         release_systems = paste0(release_systems, "\u00B9"),
         citation        = paste("\u00B9", citation, sep = " "),
         # Increase superscripts to account for 1^ above
         # Superscript 1: \u00B9
         # Superscript 2: \u00B2
         # Superscript 3: \u00B3
         # Superscript 4: \u2074
         # Superscript 5: \u2075
         # Superscript 6: \u2076
         estimation_note = gsub("\u00B9", "\u00B2", estimation_note),
         rules_note      = gsub("\u00B2", "\u00B3", rules_note),
         projection_note = gsub("\u00B3", "\u2074", projection_note),


         source_note1    = gsub("\u00B9", "\u00B2", source_note1),
         source_note2    = gsub("\u00B9", "\u00B3", source_note2),
         source_note3    = gsub("\u00B3", "\u2074", source_note3),
         # combine methodology info and citations
         methodology_notes = paste(estimation_note, matching_note, rules_note, projection_note, sep = "<br><br>"),
         citation = paste(citation, source_note1, source_note2, source_note3, sep = "<br><br>")
  ) |>
  select(state, methodology_notes, citation)

# Save as spreadsheet
write.csv(state_methodology_v1, "state_methodology_v1.csv")
