# Adjust the path to your Downloads folder
doc_path <- "Release Systems by State.docx"

# Read the Word document
doc <- read_docx(doc_path)

# Extract all elements of the Word document
doc_elements <- docx_summary(doc)

# Filter out only the tables
doc_tables <- doc_elements %>% filter(content_type == "table cell")

# Group the data by row_id to combine the entries by rows
reshaped_data <- doc_tables %>%
  group_by(row_id) %>%
  summarise(text = paste(text, collapse = " | ")) %>%
  ungroup()

# Display the table data
print(doc_tables)
