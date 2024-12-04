# Format sources that will go under each visualization
ncrp_csg_source_year <- "National Corrections Reporting Program and CSG Justice Center estimates, 2019"
ncrp_source_year     <- "National Corrections Reporting Program, 2019"
bjs_source_year      <- "BJS Prisoners in the United States, 2019"



fnc_get_source_year <- function(df, state_name){
  state_year <- df |>
    filter(state == state_name) |>
    pull(unique(rptyear))
  return(state_year)
}
