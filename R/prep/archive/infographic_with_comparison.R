# Updated function to ensure the blue person and group are the same size
create_infographic_with_comparison_same_size <- function(rri_raw) {

  # Create the blue person (for comparison)
  comparison_icon <- create_icons(
    rri_raw = 1,             # One full blue person
    infogs = 1,               # Only one icon for comparison
    infogs_ncol = 1,          # Single column
    fillcolor = "#1f77b4",    # Blue color for comparison
    partialcolor = "#1f77b4", # Blue color for partial (though not used here)
    emptyhumans = FALSE,      # No empty humans
    emptycolor = "white",     # White background for empty spaces
    fillHoriz = FALSE         # Fill vertically (same as before)
  )

  # Create the group of people on the right
  ggtemp_justpeople <- create_icons(
    rri_raw = rri_raw,
    infogs = default_ncols,
    infogs_ncol = default_ncols,
    fillcolor = mclc_dk_blue,
    partialcolor = mclc_lt_blue,
    emptyhumans = TRUE,
    emptycolor = "white",
    fillHoriz = FALSE
  )

  # Ensure same size for both sets of icons by applying a fixed aspect ratio
  comparison_icon <- comparison_icon + theme(aspect.ratio = 1)
  ggtemp_justpeople <- ggtemp_justpeople + theme(aspect.ratio = 1)

  # Combine the blue person and the group of people in one plot
  combined_plot <- plot_grid(comparison_icon, ggtemp_justpeople, nrow = 1, rel_widths = c(1, 4))

  # Display the combined plot
  print(combined_plot)
}

# Call the updated function
create_infographic_with_comparison_same_size(3.5)

# Save the high resolution version
ggsave("high_res_infographic_with_comparison_same_size.png", plot = last_plot(), width = 8, height = 8, dpi = 300)
