data <- ncrp_term_records %>% filter(state == "Georgia") %>%
  filter(!is.na(releaseyr) & !is.na(abt_inmate_id))

# # Sort the data by inmate ID and release year
# sorted_data <- data[order(data$abt_inmate_id, data$releaseyr), ]
# # Create a flag indicating whether an individual recidivated within 2 years
# sorted_data$recidivated <- FALSE
# for (i in 1:(nrow(sorted_data) - 1)) {
#   if (sorted_data$abt_inmate_id[i] == sorted_data$abt_inmate_id[i + 1]) {
#     if (sorted_data$releaseyr[i + 1] - sorted_data$releaseyr[i] <= 2) {
#       sorted_data$recidivated[i] <- TRUE
#       sorted_data$recidivated[i + 1] <- TRUE
#     }
#   }
# }
#
# # View the updated data
# sorted_data

# Sort the data by inmate ID and release year
sorted_data <- data %>%
  arrange(abt_inmate_id, releaseyr)

# Create a flag indicating whether an individual recidivated within 2 years
sorted_data <- sorted_data %>%
  group_by(abt_inmate_id) %>%
  mutate(recidivated = (releaseyr - lag(releaseyr, default = first(releaseyr))) >= 2) %>%
  ungroup()

# Flag individuals who recidivated at least once within 2 years
recidivated_flag <- sorted_data %>%
  group_by(abt_inmate_id) %>%
  summarise(recidivated = any(recidivated)) %>%
  ungroup()

# Merge the recidivated flag back to the original dataset
final_data <- merge(data, recidivated_flag, by = "abt_inmate_id", all.x = TRUE)

# # View the updated data
# data <- final_data %>%
#   filter(!is.na(offgeneral)) %>%
#   group_by(offgeneral) %>%
#   count(recidivated)

data <- final_data %>%
  select(abt_inmate_id, offgeneral, recidivated, admtype) %>%
  slice(1:100) %>%
  mutate(recidivated = case_when(
    recidivated == TRUE ~ "Returned to Prison",
    recidivated == FALSE ~ "Did not Return to Prison"
  ))












































# create a table of frequencies
freq_table <- data %>% group_by(admtype, recidivated) %>%
  summarise(n = n())

# create a nodes data frame
nodes <- data.frame(name = unique(c(as.character(freq_table$admtype),
                                    as.character(freq_table$recidivated))))

# create links dataframe
links <- data.frame(source = match(freq_table$admtype, nodes$name) - 1,
                    target = match(freq_table$recidivated, nodes$name) - 1,
                    value = freq_table$n,
                    stringsAsFactors = FALSE)

# Calculate incoming flow counts for "New Court Commitment" and "Parole Return/Revocation" to "Returned to Prison"
incoming_new_court <- filter(freq_table, recidivated == "Returned to Prison" &
                               admtype == "New court commitment")$n

incoming_parole_return <- filter(freq_table, recidivated == "Returned to Prison" &
                                   admtype == "Parole return/revocation")$n

# Add incoming flow counts to "Returned to Prison" label
returned_label <- paste0("<b>Returned to Prison</b>\n(Total: ",
                         incoming_new_court + incoming_parole_return, ")")
returned_label <- paste0(returned_label,
                         "\nNew Court Commitment: ", incoming_new_court,
                         "\nParole return/revocation: ", incoming_parole_return)

# Calculate incoming flow counts for "New Court Commitment" and "Parole Return/Revocation" to "Did not Return to Prison"
incoming_new_court <- filter(freq_table, recidivated == "Did not Return to Prison" &
                               admtype == "New court commitment")$n

incoming_parole_return <- filter(freq_table, recidivated == "Did not Return to Prison" &
                                   admtype == "Parole return/revocation")$n

# Add incoming flow counts to "Returned to Prison" label
didnt_returned_label <- paste0("<b>Did not Return Prison</b>\n(Total: ",
                         incoming_new_court + incoming_parole_return, ")")
didnt_returned_label <- paste0(didnt_returned_label,
                         "\nNew Court Commitment: ", incoming_new_court,
                         "\nParole return/revocation: ", incoming_parole_return)

# Update the label in the nodes data frame
nodes$name[nodes$name == "Returned to Prison"] <- returned_label
nodes$name[nodes$name == "Did not Return to Prison"] <- didnt_returned_label

plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    pad = 15,
    thickness = 20,
    line = list(color = neutralBlackText,
                width = 0.5),
    textfont = list(size = 16) ,
    label = nodes$name
  ),
  link = list(
    source = links$source,
    target = links$target,
    value = links$value,
    hoverinfo = "none"
  ),
  textfont = list(family = "Graphik",
                  size = 14,
                  color = neutralBlackText),
  width = 720,
  height = 480
) %>%
  layout(
    title = "Recidivism",
    # font = list(size = 14),
    margin = list(t = 40, l = 10, r = 10, b = 10)
  )



# # create a table of frequencies
# freq_table <- data %>% group_by(offgeneral, admtype, recidivated) %>%
#   summarise(n = n())
#
# # create a nodes data frame
# nodes <- data.frame(name = unique(c(as.character(freq_table$offgeneral),
#                                     as.character(freq_table$admtype),
#                                     as.character(freq_table$recidivated))))
#
# # create links dataframe
# links <- data.frame(source = match(freq_table$offgeneral, nodes$name) - 1,
#                     target = match(freq_table$admtype, nodes$name) - 1,
#                     value = freq_table$n,
#                     stringsAsFactors = FALSE)
#
# links <- rbind(links,
#                data.frame(source = match(freq_table$admtype, nodes$name) - 1,
#                           target = match(freq_table$recidivated, nodes$name) - 1,
#                           value = freq_table$n,
#                           stringsAsFactors = FALSE))
#
# # Make Sankey diagram
# plot_ly(
#   type = "sankey",
#   orientation = "h",
#   node = list(pad = 15,
#               thickness = 20,
#               line = list(color = "black", width = 0.5),
#               label = nodes$name),
#   link = list(source = links$source,
#               target = links$target,
#               value = links$value),
#   textfont = list(size = 10),
#   width = 720,
#   height = 480
# ) %>%
#   layout(title = "Sankey Diagram: offgeneral, admtype, and recidivated",
#          font = list(size = 14),
#          margin = list(t = 40, l = 10, r = 10, b = 10))






# library(tidyverse)
# library(gganimate)
#
# sigmoid <- function(x_from, x_to, y_from, y_to, scale = 5, n = 100) {
#   x <- seq(-scale, scale, length = n)
#   y <- exp(x) / (exp(x) + 1)
#   tibble(x = (x + scale) / (scale * 2) * (x_to - x_from) + x_from,
#          y = y * (y_to - y_from) + y_from)
# }
#
# n_points <- 400
# data1 <- tibble(from = rep(4, n_points),
#                to = sample(1:4, n_points, TRUE),
#                color = sample(c("A", "B"), n_points, TRUE))
#
# p <- map_df(seq_len(nrow(data1)),
#             ~ sigmoid(0, 1, as.numeric(data1[.x, 1]), as.numeric(data1[.x,
#                                                                      2])) %>%
#               mutate(time = row_number() + .x,
#                      y = y + runif(1, -0.25, 0.25))) %>%
#   ggplot(aes(x, y, frame = time)) +
#   geom_point()+transition_manual(time)
#
# animate(p, nframes = 499)



















# library(networkD3)
# # Create Nodes data frame
# Nodes <- data.frame(name = unique(c(data$offgeneral, data$recidivated)))
#
# # Compute number of people at each node
# node_counts <- aggregate(n ~ offgeneral, data, sum)
# Nodes$n <- ifelse(Nodes$name %in% node_counts$offgeneral, node_counts$n[match(Nodes$name, node_counts$offgeneral)], 0)
#
# # Compute proportion of people in each node
# total_count <- sum(data$n)
# Nodes$prop <- Nodes$n / total_count
# Nodes$prop_label <- paste0(formatC(Nodes$prop * 100, digits = 2, format = "f"), "%")
#
# # Create a data frame for links
# Links <- data.frame(source = match(data$offgeneral, Nodes$name) - 1,
#                     target = match(data$recidivated, Nodes$name) - 1,
#                     value = data$n)
#
# # Create a Sankey plot
# sankeyPlot <- sankeyNetwork(
#   Links = Links,
#   Nodes = Nodes,
#   Source = "source",
#   Target = "target",
#   Value = "value",
#   NodeID = "name",
#   fontSize = 16,
#   nodeWidth = 30
# )
#
# # Display the Sankey plot
# sankeyPlot
#
