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
  select(abt_inmate_id, recidivated) %>% mutate(color = "red") %>%
  slice(1:20)



library(tidyverse)
library(gganimate)

sigmoid <- function(x_from, x_to, y_from, y_to, scale = 5, n = 100) {
  x <- seq(-scale, scale, length = n)
  y <- exp(x) / (exp(x) + 1)
  tibble(x = (x + scale) / (scale * 2) * (x_to - x_from) + x_from,
         y = y * (y_to - y_from) + y_from)
}

n_points <- 400
data <- tibble(from = rep(4, n_points),
               to = sample(1:4, n_points, TRUE),
               color = sample(c("A", "B"), n_points, TRUE))

p <- map_df(seq_len(nrow(data)),
            ~ sigmoid(0, 1, as.numeric(data[.x, 1]), as.numeric(data[.x,
                                                                     2])) %>%
              mutate(time = row_number() + .x,
                     y = y + runif(1, -0.25, 0.25))) %>%
  ggplot(aes(x, y, frame = time)) +
  geom_point()+transition_manual(time)

animate(p, nframes = 499)





















# library(networkD3) - this works but I want something different
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

