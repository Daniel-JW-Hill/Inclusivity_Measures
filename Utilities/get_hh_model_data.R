
#This function constructs a dataframe per household for the household models. 

get_hh_model_data = function(data){
  
  hhs = unique(data$HHID)
  
  hh_data = data.frame("HHID" = hhs,
                       "SINGLE" = rep(0, length(hhs)),
                       "FEMALE_HEAD" = rep(0, length(hhs)),
                       "AGE_HEAD" = rep(0, length(hhs)),
                       "EDUCATION_HEAD" = rep(0, length(hhs)),
                       "DEPENDENCY_RATIO" = rep(0, length(hhs)),
                       "SOCIAL_PARTICIPATION" = rep(0, length(hhs)),
                       "COFFEE_TREES" = rep(0, length(hhs)),
                       "COFFEE_TREES_SQR" =   rep(0, length(hhs)),
                       "PROPORTION_LAND_RENTED" =   rep(0, length(hhs)),
                       "ASSETS" =   rep(0, length(hhs)),
                       "ALTITUDE" =   rep(0, length(hhs)),
                       "DISTANCE_PAVED_ROAD" = rep(0, length(hhs)),
                       "DECISION_DISAGREEMENTS" = rep(0, length(hhs)),
                       "INPUT_PREFERENCES" = rep(0, length(hhs)),
                       "EASE_MARKETING_PREFERENCES" = rep(0, length(hhs)),
                       "PRICE_PREFERENCES" = rep(0, length(hhs)),
                       "RELATIVE_PRICE_DIFFERENTIAL" = rep(0, length(hhs)),
                       "BINARY_PARTICIPATION" = rep(0, length(hhs)),
                       "CONTINUOUS_PARTICIPATION" = rep(0, length(hhs))
                       )
  
  for (hhs in 1:nrow(hh_data)){
     hh_idx = hh_data$HHID[hhs]
     data_subset  = data[data$HHID == hh_idx,]
    
    if (nrow(data_subset) == 1){ # if single headed household. 
      hh_data$SINGLE[hhs] = data_subset$SINGLE[1]
      hh_data$FEMALE_HEAD[hhs] = data_subset$FEMALE[1]
      hh_data$AGE_HEAD[hhs] = data_subset$AGE[1]
      hh_data$EDUCATION_HEAD[hhs] = data_subset$EDUCATION[1]
      hh_data$DEPENDENCY_RATIO[hhs] = data_subset$DEPENDENCY_RATIO[1]
      hh_data$SOCIAL_PARTICIPATION[hhs] = data_subset$SOCIAL_PARTICIPATION_OVERALL[1]
      hh_data$COFFEE_TREES[hhs] = data_subset$COFFEE_TREES_N[1]
      hh_data$COFFEE_TREES_SQR[hhs] = data_subset$COFFEE_TREES_N_SQ[1]
      hh_data$PROPORTION_LAND_RENTED[hhs] = data_subset$RENTED_PROPORTION[1]
      hh_data$ASSETS[hhs] = data_subset$value_assets_HH[1]
      hh_data$ALTITUDE[hhs] = data_subset$altitude[1]/1000
      hh_data$DISTANCE_PAVED_ROAD[hhs] = data_subset$distance_road[1]/1000
      hh_data$DECISION_DISAGREEMENTS[hhs] = data_subset$DECISION_DISAGREEMENTS_COFFEE_MAJOR[1]
      hh_data$INPUT_PREFERENCES[hhs] = data_subset$INPUT_PREFERENCES[1]
      hh_data$EASE_MARKETING_PREFERENCES[hhs] = data_subset$EASE_PREFERENCES[1]
      hh_data$PRICE_PREFERENCES[hhs] = data_subset$PRICE_PREFERENCES[1]
      hh_data$RELATIVE_PRICE_DIFFERENTIAL[hhs] = data_subset$RELATIVE_PRICE_DIFFERENTIAL[1]
      hh_data$BINARY_PARTICIPATION[hhs] = data_subset$binary_participation_hh[1]
      hh_data$CONTINUOUS_PARTICIPATION[hhs]= data_subset$continuous_participation_average[1]
      
    } else { # for household head and spouse. 
      
      head = which(data_subset$HOUSEHOLD_HEAD == 1)
     
      hh_data$SINGLE[hhs] = 0
      hh_data$FEMALE_HEAD[hhs] = data_subset$FEMALE[head]
      hh_data$AGE_HEAD[hhs] = data_subset$AGE[head]
      hh_data$EDUCATION_HEAD[hhs] = data_subset$EDUCATION[head]
      hh_data$DEPENDENCY_RATIO[hhs] = data_subset$DEPENDENCY_RATIO[head]
      hh_data$SOCIAL_PARTICIPATION[hhs]= data_subset$SOCIAL_PARTICIPATION_OVERALL[head]
      hh_data$COFFEE_TREES[hhs] = data_subset$COFFEE_TREES_N[head]
      hh_data$COFFEE_TREES_SQR[hhs] = data_subset$COFFEE_TREES_N_SQ[head]
      hh_data$PROPORTION_LAND_RENTED[hhs] = data_subset$RENTED_PROPORTION[head]
      hh_data$ASSETS[hhs] = data_subset$value_assets_HH[head]
      hh_data$ALTITUDE[hhs] = data_subset$altitude[head]/1000
      hh_data$DISTANCE_PAVED_ROAD[hhs] = data_subset$distance_road[head]/1000
      hh_data$DECISION_DISAGREEMENTS[hhs] = data_subset$DECISION_DISAGREEMENTS_COFFEE_MAJOR[head]
      hh_data$INPUT_PREFERENCES[hhs] = data_subset$INPUT_PREFERENCES[head]
      hh_data$EASE_MARKETING_PREFERENCES[hhs] = data_subset$EASE_PREFERENCES[head]
      hh_data$PRICE_PREFERENCES[hhs] = data_subset$PRICE_PREFERENCES[head]
      hh_data$RELATIVE_PRICE_DIFFERENTIAL[hhs] = data_subset$RELATIVE_PRICE_DIFFERENTIAL[head]
      hh_data$BINARY_PARTICIPATION[hhs] = data_subset$binary_participation_hh[head]
      hh_data$CONTINUOUS_PARTICIPATION[hhs] = data_subset$continuous_participation_average[head]
    }
    
  }
  
  # Make minor adjustment for beta distributions estimated for continuous data
  # Models must be bounded (0,1). This is assumed for the zeros mechanically for the zero inflated model
  # But for ones it is not. 
  # For the one observation with continuous participation == 1, change to 0.999
  hh_data$CONTINUOUS_PARTICIPATION[which(hh_data$CONTINUOUS_PARTICIPATION == 1)] = 0.999
  
  return(hh_data)
  
}