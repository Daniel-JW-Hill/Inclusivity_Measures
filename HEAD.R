
## Script for Measurement of extensive and intensive inclusion 
## within smallholder orientated high value markets. 

# Daniel Hill

# This is the head script for analysis of inclusion measurement methodologies
# For coffee selling households in Kapchorwa. 

rm(list = ls())

#Set WD
library(here)
here()

#Load raw analysis data. 
library(readxl)
data = readxl::read_xlsx(here('Data',"Complete_data_final.xlsx"))

###########################
#### Data preparation #####
###########################

# Initial data file restructuring and edits to IDs and other important information.
source(here('Utilities',"initial_data_clean.R"))
data = initial_data_clean(data)

# GPS coordinates are collected inconsistently depending on enumerator and day
# Run script to clean up GPS data for each household to derive altitude and 
# Other spatial information. 
source(here('Utilities',"clean_GPS_variables.R"))
data = clean_GPS_variables(data)

# # Retrieve spatial data for each observation based on village GPS coords. 
source(here('Utilities',"get_spatial_data.R"))
data = get_spatial_data(data)

# Retrieve proxy asset valuations - indiv, others, and total
source(here('Utilities',"get_asset_values.R"))
data = get_asset_values(data)

# Clean and retrieve outcome participation measures
source(here('Utilities',"clean_coffee_marketing.R"))
source(here('Utilities',"get_participation_outcomes.R"))
data = clean_coffee_marketing(data)
data = get_participation_outcomes(data)

# Tidy up some of the Household and individual characteristics
source(here('Utilities', "clean_characteristics.R"))
data = clean_characteristics(data)

# Tidy up coffee tree and tenure information 
source(here('Utilities', "clean_land.R"))
data = clean_land(data)

#Tidy up some variable names and variable classes
source(here('Utilities', "clean_choice_exp_framing.R"))
source(here('Utilities', "clean_semantic_differential.R"))
source(here('Utilities', "clean_preference_ranks.R"))
data = clean_choice_exp_framing(data)
data = clean_semantic_differential(data)
data = clean_preference_ranks(data)

#Retrieve preferences
source(here('Utilities', "get_preferences_new.R"))
preference_outputs = get_preferences(data)
data = preference_outputs[[1]]
preference_rankings = preference_outputs[[2]]
individualRankLogit = preference_outputs[[3]]

# Save results from preference analysis
write.csv(preference_rankings,file = here("Results", "preference_rankings.csv"))

#Retrieve relative decision making for coffee. 
source(here('Utilities', "get_relative_decision_making.R"))
data = get_relative_decision_making(data)

# Retrieve relative price information
source(here('Utilities', "get_relative_prices.R"))
data = get_relative_prices(data)

# Package data for hh model and individual model
source(here('Utilities', "get_hh_model_data.R"))
source(here('Utilities', "get_indiv_model_data.R"))
hh_data = get_hh_model_data(data)
indiv_data = get_indiv_model_data(data)

###############################
#### Save data and reload #####
###############################

write.csv(data, file = here("Data", "Data_clean.csv"))
write.csv(hh_data, file = here("Data","hh_data.csv"))
write.csv(indiv_data, file = here("Data","indiv_data.csv"))

hh_data = read.csv(here("Data","hh_data.csv"))
indiv_data = read.csv(here("Data","indiv_data.csv"))

###########################
#### Model Estimation #####
###########################

# Set up models for estimation
Outcome_hh = c("CONTINUOUS_PARTICIPATION")
C_hh = c("AGE_HEAD", "EDUCATION_HEAD" , "DEPENDENCY_RATIO" , "SOCIAL_PARTICIPATION" , "COFFEE_TREES" ,
         "PROPORTION_LAND_RENTED" , "ASSETS" , "ALTITUDE", "DISTANCE_PAVED_ROAD" , "DECISION_DISAGREEMENTS") 
P_hh   = c("PRICE_PREFERENCES" , "INPUT_PREFERENCES")
Z_hh = c("RELATIVE_PRICE_DIFFERENTIAL")
P_hh_omitted = c("EASE_MARKETING_PREFERENCES")

Outcome_indiv = c("BINARY_PARTICIPATION")
C_indiv = c("FEMALE",  "AGE", "EDUCATION" , "DEPENDENCY_RATIO" , "SOCIAL_PARTICIPATION" , "COFFEE_TREES" ,
            "PROPORTION_LAND_RENTED" , "ASSETS" , "ALTITUDE", "DISTANCE_PAVED_ROAD" , "DECISION_DISAGREEMENTS")
P_indiv   = c("PRICE_PREFERENCES" , "INPUT_PREFERENCES")
Z_indiv = c("RELATIVE_PRICE_DIFFERENTIAL")
P_indiv_omitted = c("EASE_MARKETING_PREFERENCES")

# Retrieve summary stats tables from model tables
source(here('Utilities', "get_summary_stats.R"))
get_summary_stats(hh_data, indiv_data)

# Estimate first stage regressions to retrieve orthogonal residuals of preferences 
source(here('Utilities', "get_first_stage_regressions.R"))
first_stage_outputs = get_first_stage_regressions(hh_data, indiv_Data,  
                                                  C_hh, P_hh, Z_hh, P_hh_omitted,
                                                  C_indiv, P_indiv, Z_indiv, P_indiv_omitted)
hh_data = first_stage_outputs[[1]]
indiv_data = first_stage_outputs[[2]]
rm(first_stage_outputs)

V_hh = c("PRICE_PREFERENCES_RESIDUALS" , "INPUT_PREFERENCES_RESIDUALS", "RELATIVE_PRICE_DIFFERENTIAL_RESIDUALS")
V_indiv = c("PRICE_PREFERENCES_RESIDUALS" , "INPUT_PREFERENCES_RESIDUALS", "RELATIVE_PRICE_DIFFERENTIAL_RESIDUALS")

# Firstly perform direct and reduced form models for individual extensive HVM participation 
# This script returns fitted values for HVM participation opportunity, and saves model results 
# in the results folder. 

source(here('Utilities', "get_indiv_models.R"))
fitted_indiv = get_indiv_models(indiv_data,
                                Outcome_indiv,
                                C_indiv,
                                P_indiv,
                                Z_indiv, 
                                V_indiv)

# Now perform the same for the household mixture distribution 
source(here('Utilities', "get_hh_models.R"))
fitted_hh  = get_hh_models(hh_data, 
                           Outcome_hh,
                           C_hh, 
                           P_hh,
                           Z_hh,
                           V_hh)

###############################################
#### Inequality in opportunity estimation #####
###############################################

# With these results, now estimate inequality measures and ranges. 
source(here('Utilities', "get_inequality_measures.R"))

#Individual model - extensive opportunity to participate
get_inequality_measures(fitted_indiv$Direct_models, 
                        fitted_indiv$Reduced_models, 
                        title = "Binary individual model ineq results (fitted)",
                        relative_to = "fitted")

#HH model - extensive, intensive, and total.
get_inequality_measures(fitted_hh$direct$zprob, 
                        fitted_hh$reduced$zprob, 
                        title = "Continuous household model ineq results (zprob fitted)", 
                        relative_to = "fitted")

get_inequality_measures(fitted_hh$direct$conditional, 
                        fitted_hh$reduced$conditional,  
                        title = "Continuous household model ineq results (conditional fitted)", 
                        relative_to = "fitted")

get_inequality_measures(fitted_hh$direct$response, 
                        fitted_hh$reduced$response, 
                        title = "Continuous household model ineq results (response fitted)", 
                        relative_to = "fitted")

########################
#### DECOMPOSITIONS ####
########################  

# Now we have calculated the central estimates
# We can move onto understand the contribution of each variable
# through a decomposition analysis

# This is achieved by eliminating  the variation of a single variable at a time
# and re-estimating the inequality in opportunity. 

#Individual model - extensive only
source(here("Utilities", "get_decomposition_indiv.R"))
get_decomposition_indiv(indiv_data,
                        Outcome_indiv,
                        C_indiv,
                        P_indiv,
                        Z_indiv,
                        V_indiv)

# Household model - extensive, intensive and total
source(here("Utilities", "get_decomposition_hh.R"))
get_decomposition_hh(hh_data, 
                     Outcome_hh,
                     C_hh,
                     P_hh,
                     Z_hh,
                     V_hh)

# Plot contributions, drawing on csv files saved in results folder. 
source(here("Utilities", "get_contribution_plots.R"))
get_contribution_plots(contributions_of = "PSY")

########################
#### Bias scenarios ####
########################  

# Now we perform the estimations with artificially introduced bias. 
# This will give us the ranges in which we may expect a causal interpretation
# of the inequality in opportunity measures. 

# Individual model
source(here("Utilities", "get_bias_ci_indiv_binary.R"))
get_bias_ci_indiv_binary(indiv_data, Outcome_indiv, C_indiv, P_indiv, Z_indiv, V_indiv)

# Household model
source(here("Utilities", "get_bias_ci_continuous.R"))
get_bias_ci_continuous(hh_data, Outcome_hh, C_hh, P_hh, Z_hh, V_hh)

########################
#### End of script #####
########################  




