
# ###############################
#
# # Georgia
#
# ###############################
#
# # create table about parole eligibility
# pe_length <- c("1/3", "6", "9", "7", "7 or 1/3")
# pe_unit   <- c("Term", "Months", "Months", "Years", "Years or Term")
# pe_sentence <- c("Generally when parole consideration occurs",
#                  "Must be served for misdemeanor sentences",
#                  "Must be served for felony sentences",
#                  "Must be served for a sentence of 21 years or more (excluding life sentences)",
#                  "Must be served for convictions of serious crimes such as aggravated assault, incest, or drug trafficking")
# pe_criteria <- data.frame(pe_length, pe_unit, pe_sentence)
#
# pe1 <- c("1/3 Term", "Generally when parole consideration occurs")
# pe2 <- c("6 Months", "Must be served for misdemeanor sentences")
# pe3 <- c("9 Months", "Must be served for felony sentences")
# pe4 <- c("7 Years", "Must be served for a sentence of 21 years or more (excluding life sentences)")
# pe5 <- c("7 Years or 1/3 Term", "Must be served for convictions of serious crimes such as aggravated assault, incest, or drug trafficking")
#
# # Reshape the data into a long format
# df1 <- data.frame(pe1, pe2, pe3)
# df2 <- data.frame(pe4, pe5)
#
# pe_criteria_table1 <- reactable(
#   data = df1,
#   sortable = FALSE,
#   style = hc_reactable_style,
#   theme = reactableTheme(borderColor = "#fff",
#                          stripedColor = "#fff",
#                          cellStyle = list(display = "flex",
#                                           flexDirection = "column",
#                                           justifyContent = "center")),
#   columns = list(
#     pe1 = colDef(align = "center", name = "", minWidth = 80),
#     pe2 = colDef(align = "center", name = "", minWidth = 80),
#     pe3 = colDef(align = "center", name = "", minWidth = 80)
#   ),
#   rowStyle = function(index) {
#     if (index %in% c(1)) {
#       list(fontWeight = "bold",
#            fontSize = "20px",
#            marginBottom = "-10px")
#     } else if (index %in% c(2)) {
#       list(fontSize = "14px")
#     }
#   }
# )
#
# pe_criteria_table2 <- reactable(
#   data = df2,
#   sortable = FALSE,
#   style = hc_reactable_style,
#   theme = reactableTheme(borderColor = "#fff",
#                          stripedColor = "#fff",
#                          cellStyle = list(display = "flex",
#                                           flexDirection = "column",
#                                           justifyContent = "center")),
#   columns = list(
#     pe4 = colDef(align = "center", name = "", minWidth = 80),
#     pe5 = colDef(align = "center", name = "", minWidth = 80)
#   ),
#   rowStyle = function(index) {
#     if (index %in% c(1)) {
#       list(fontWeight = "bold",
#            fontSize = "20px",
#            marginBottom = "-10px")
#     } else if (index %in% c(2)) {
#       list(fontSize = "14px")
#     }
#   }
# )

c("While the grant of parole is discretionary, consideration for parole is often not; most people in prison are entitled to automatic consideration for parole. In general, this occurs after serving 1/3 of the total sentence. However, there are minimums depending on the sentence.")

##########
# Save data
##########

# theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))
#
# for (folder in theseFOLDERS){
#
#   save(all_parole_eligibility_criteria, file=file.path(folder, "all_parole_eligibility_criteria.rds"))
#
# }
