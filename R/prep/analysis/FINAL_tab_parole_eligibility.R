#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: November 14, 2024 (MAR)
# Description:

#######################################

# ---------------------------------------------------------------------------- #
# Pie charts of the prison population by parole eligibility status
# ---------------------------------------------------------------------------- #

# Function that filters the population data to include only people in prison for new crimes
# with sentence lengths 1+ years except life
# Only includes states with parole systems and without high missingness
# Includes states don't need to be filtered by admission type or sentence length
# These states are in states_nofilter
ncrp_yearendpop_filtered <- fnc_filter_pe_population_criteria(data = ncrp_yearendpop_consolidated,
                                                              exclude = states_to_exclude,
                                                              dont_filter = states_nofilter)



