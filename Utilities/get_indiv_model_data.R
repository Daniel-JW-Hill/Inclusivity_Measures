
#This function constructs a dataframe per individual

get_indiv_model_data = function(data){
  
  indivs = data$HHID
  female = data$FEMALE
  
  indiv_data = data.frame("HHID" = indivs,
                       "FEMALE" = female,
                       "SINGLE" = rep(0, length(indivs)),
                       "FEMALE_HEAD" = rep(0, length(indivs)),
                       "AGE" = rep(0, length(indivs)),
                       "EDUCATION" = rep(0, length(indivs)),
                       "DEPENDENCY_RATIO" = rep(0, length(indivs)),
                       "SOCIAL_PARTICIPATION" = rep(0, length(indivs)),
                       "COFFEE_TREES" = rep(0, length(indivs)),
                       "COFFEE_TREES_SQR" =   rep(0, length(indivs)),
                       "PROPORTION_LAND_RENTED" =   rep(0, length(indivs)),
                       "ASSETS" =   rep(0, length(indivs)),
                       "ALTITUDE" =   rep(0, length(indivs)),
                       "DISTANCE_PAVED_ROAD" = rep(0, length(indivs)),
                       "DECISION_DISAGREEMENTS" = rep(0, length(indivs)),
                       "INPUT_PREFERENCES" = rep(0, length(indivs)),
                       "EASE_MARKETING_PREFERENCES" = rep(0, length(indivs)),
                       "PRICE_PREFERENCES" =  rep(0, length(indivs)),
                       "RELATIVE_PRICE_DIFFERENTIAL" = rep(0, length(indivs)),
                       "BINARY_PARTICIPATION" = rep(0, length(indivs))
                       )
  
  for (i in 1:nrow(indiv_data)){
    hh_idx = indiv_data$HHID[i]
    female_idx = indiv_data$FEMALE[i]
    data_subset  = data[data$HHID == hh_idx,]
    indiv =  which(data_subset$FEMALE == female_idx)
    indiv_data$SINGLE[i] = data_subset$SINGLE[indiv]
    indiv_data$FEMALE_HEAD[i] = ifelse(length(data_subset$HOUSEHOLD_HEAD[data_subset$FEMALE == 1 & data_subset$HOUSEHOLD_HEAD == 1]) > 0 , 1,  
                                       ifelse(length(data_subset$HOUSEHOLD_HEAD[data_subset$FEMALE == 1]) != 0 & nrow(data_subset) == 1, 1, 0))
    indiv_data$AGE[i] = data_subset$AGE[indiv]
    indiv_data$EDUCATION[i] = data_subset$EDUCATION[indiv]
    indiv_data$DEPENDENCY_RATIO[i] = data_subset$DEPENDENCY_RATIO[indiv]
    indiv_data$SOCIAL_PARTICIPATION[i] = data_subset$SOCIAL_PARTICIPATION_OVERALL[indiv]
    indiv_data$COFFEE_TREES[i] = data_subset$COFFEE_TREES_N[indiv]
    indiv_data$COFFEE_TREES_SQR[i] = data_subset$COFFEE_TREES_N_SQ[indiv]
    indiv_data$PROPORTION_LAND_RENTED[i] = data_subset$RENTED_PROPORTION[indiv]
    indiv_data$ASSETS[i] = data_subset$value_assets_indiv[indiv]
    indiv_data$ALTITUDE[i] = data_subset$altitude[indiv]/1000
    indiv_data$DISTANCE_PAVED_ROAD[i] = data_subset$distance_road[indiv]/1000
    indiv_data$DECISION_DISAGREEMENTS[i] = data_subset$DECISION_DISAGREEMENTS_COFFEE_MAJOR[indiv]
    indiv_data$INPUT_PREFERENCES[i] = data_subset$INPUT_PREFERENCES[indiv]
    indiv_data$EASE_MARKETING_PREFERENCES[i] = data_subset$EASE_PREFERENCES[indiv]
    indiv_data$PRICE_PREFERENCES[i] = data_subset$PRICE_PREFERENCES[indiv]
    indiv_data$RELATIVE_PRICE_DIFFERENTIAL[i] = data_subset$num_buyers_option[indiv]
    indiv_data$BINARY_PARTICIPATION[i] = data_subset$binary_participation_indiv[indiv]
      
  }
  
  return(indiv_data)
  
}