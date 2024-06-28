
# This function performs the decomposition for the continuous hh  models. 
# 

get_contributions_continuous = function(hh_data, 
                                        Outcome_hh,
                                        C_hh,
                                        P_hh,
                                        Z_hh,
                                        V_hh) {
  
 
  library(glmmTMB)
  library(performance)
  
  #####################
  ##### FULL MODEL ####
  #####################
  
  #Estimate central model
  eq_hh_full =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(P_hh, collapse = " + "), Z_hh, sep = " + ")) 
  eq_hh_full = as.formula(eq_hh_full)
  model_hh_full = tobit(eq_hh_full, left = 0, right = 1, data = hh_data)
  model_summary_hh_full = summary(model_hh_full)
  
  #Retrieve coefficients and central fitted values
  coefficients = coef(model_hh_full)
  
  #Opportunity fitted values
  coef_opportunity = C_hh
  coefficients_opportunity = coefficients[coef_opportunity]
  model_matrix_opportunity = model.matrix(as.formula(eq_hh_full), data = hh_data)[, coef_opportunity]
  fitted_opportunity = as.matrix(model_matrix_opportunity) %*% as.matrix(coefficients_opportunity)

  #Preference fitted values
  coef_preferences = c(P_hh, Z_hh)
  coefficients_preferences = coefficients[coef_preferences]
  model_matrix_preferences = model.matrix(as.formula(eq_hh_full), data = hh_data)[, coef_preferences]
  fitted_preferences = as.matrix(model_matrix_preferences) %*% as.matrix(coefficients_preferences)
  
  #Fitted outcomes (with error term)
  fitted_outcomes = fitted(model_hh_full) + residuals(model_hh_full)
  
  # #Add scalar constant to fitted values to ensure all are positive 
  min_fitted_opportunity = min(fitted_opportunity)
  min_fitted_preferences = min(fitted_preferences)
  min_fitted_outcomes = min(fitted_outcomes)
  
  if (min_fitted_opportunity <0){
    fitted_opportunity =   fitted_opportunity + abs(min_fitted_opportunity)
  }
  if (min_fitted_preferences <0){
    fitted_preferences =  fitted_preferences + abs(min_fitted_preferences)
  }
  if (min_fitted_outcomes <0){
    fitted_outcomes = fitted_outcomes + abs(min_fitted_outcomes)
  }
  

  GINI = c(gini.wtd(fitted_opportunity),
           gini.wtd(fitted_preferences),
           gini.wtd(fitted_outcomes))
  THEIL = c(theil.wtd(fitted_opportunity),
            theil.wtd(fitted_preferences),
            theil.wtd(fitted_outcomes))
  MLD = c(mld.wtd(fitted_opportunity),
          mld.wtd(fitted_preferences),
          mld.wtd(fitted_outcomes))
              
  
  # Now retrieve contributions for opportunities      
  opportunity_gini_contributions = numeric(length(C_hh)+2)
  opportunity_theil_contributions = numeric(length(C_hh)+2)
  opportunity_mld_contributions = numeric(length(C_hh)+2)
  
  for (i in 1:length(C_hh)) {
    model_matrix_combination = model_matrix_opportunity 
    if (length(unique(model_matrix_combination[,i])) < 3){
      model_matrix_combination[,i] = 1
    } else {
     model_matrix_combination[,i] = mean(model_matrix_combination[,i])
    }
    fitted_values_combination = as.matrix(model_matrix_combination) %*% as.matrix(coefficients_opportunity)
    for (r in 1:length(fitted_values_combination)){
      fitted_values_combination[r] = ifelse(fitted_values_combination[r]<0, fitted_values_combination[r] + abs(min(fitted_values_combination)), fitted_values_combination[r])
    }
    gini_c = gini.wtd(fitted_values_combination)
    theil_c = theil.wtd(fitted_values_combination)
    mld_c = mld.wtd(fitted_values_combination)

    opportunity_gini_contributions[i] = ((gini_c - GINI[1])/GINI[1])
    opportunity_theil_contributions[i] = ((theil_c - THEIL[1])/THEIL[1])
    opportunity_mld_contributions[i] = ((mld_c - MLD[1])/MLD[1])
  }
  
  # Coffee tree decomposition 
  model_matrix_combination = as.data.frame(model_matrix_opportunity)
  model_matrix_combination$COFFEE_TREES = mean(model_matrix_combination$COFFEE_TREES, na.rm = TRUE)
  model_matrix_combination$COFFEE_TREES_SQR = mean(model_matrix_combination$COFFEE_TREES_SQR, na.rm = TRUE)
  fitted_values_combination = as.matrix(model_matrix_combination) %*% as.matrix(coefficients_opportunity)
  for (r in 1:length(fitted_values_combination)){
    fitted_values_combination[r] = ifelse(fitted_values_combination[r]<0, fitted_values_combination[r] + abs(min(fitted_values_combination)), fitted_values_combination[r])
  }
  gini_c = gini.wtd(fitted_values_combination)
  theil_c = theil.wtd(fitted_values_combination)
  mld_c = mld.wtd(fitted_values_combination)
  idx = length(opportunity_gini_contributions)-1
  opportunity_gini_contributions[idx] = ((gini_c - GINI[1])/GINI[1])
  opportunity_theil_contributions[idx] = ((theil_c - THEIL[1])/THEIL[1])
  opportunity_mld_contributions[idx] = ((mld_c - MLD[1])/MLD[1])
  
  #Spatial decomposition
  model_matrix_combination = as.data.frame(model_matrix_opportunity)
  model_matrix_combination$DISTANCE_PAVED_ROAD = mean(model_matrix_combination$DISTANCE_PAVED_ROAD, na.rm = TRUE)
  model_matrix_combination$ALTITUDE = mean(model_matrix_combination$ALTITUDE, na.rm = TRUE)
  fitted_values_combination = as.matrix(model_matrix_combination) %*% as.matrix(coefficients_opportunity)
  for (r in 1:length(fitted_values_combination)){
    fitted_values_combination[r] = ifelse(fitted_values_combination[r]<0, fitted_values_combination[r] + abs(min(fitted_values_combination)), fitted_values_combination[r])
  }
  gini_c = gini.wtd(fitted_values_combination)
  theil_c = theil.wtd(fitted_values_combination)
  mld_c = mld.wtd(fitted_values_combination)
  idx = length(opportunity_gini_contributions )
  opportunity_gini_contributions[idx] = ((gini_c - GINI[1])/GINI[1])
  opportunity_theil_contributions[idx] = ((theil_c - THEIL[1])/THEIL[1])
  opportunity_mld_contributions[idx] = ((mld_c - MLD[1])/MLD[1])
  
  # Now retrieve contributions for preferences  
  preference_vec = c(P_hh, Z_hh)
  preference_gini_contributions = numeric(length(preference_vec))
  preference_theil_contributions = numeric(length(preference_vec))
  preference_mld_contributions = numeric(length(preference_vec))
  
  for (i in 1:length(preference_vec)) {
    model_matrix_combination = model_matrix_preferences
    if (length(unique(model_matrix_combination[,i])) < 3){
      model_matrix_combination[,i] = 1
    } else {
      model_matrix_combination[,i] = mean(model_matrix_combination[,i])
    }
    fitted_values_combination = as.matrix(model_matrix_combination) %*% as.matrix(coefficients_preferences)
    for (r in 1:length(fitted_values_combination)){
     fitted_values_combination[r] = ifelse(fitted_values_combination[r]<0, fitted_values_combination[r] + abs(min(fitted_values_combination)), fitted_values_combination[r])
    }
    gini_c = gini.wtd(fitted_values_combination)
    theil_c = theil.wtd(fitted_values_combination)
    mld_c = mld.wtd(fitted_values_combination)
    
    preference_gini_contributions[i] = ((gini_c - GINI[2])/GINI[2])
    preference_theil_contributions[i] = ((theil_c - THEIL[2])/THEIL[2])
    preference_mld_contributions[i] = ((mld_c - MLD[2])/MLD[2])
  }
  
  # Now retrieve contributions for outcomes  
  all_vec = c(C_hh, P_hh, Z_hh)
  all_gini_contributions = numeric(length(all_vec)+2)
  all_theil_contributions = numeric(length(all_vec)+2)
  all_mld_contributions = numeric(length(all_vec)+2)
  model_matrix_all = model.matrix(as.formula(eq_hh_full), data = hh_data)
  coefficients_all = coef(model_hh_full)
  
  for (i in 1:length(all_vec)) {
    model_matrix_combination = model_matrix_all
    if (length(unique(model_matrix_combination[,i])) < 3){
      model_matrix_combination[,i] = 1
    } else {
      model_matrix_combination[,i] = mean(model_matrix_combination[,i])
    }
    fitted_values_combination = as.matrix(model_matrix_combination) %*% as.matrix(coefficients_all)
    
    for (r in 1:length(fitted_values_combination)){
      fitted_values_combination[r] = ifelse(fitted_values_combination[r]<0, fitted_values_combination[r] + abs(min(fitted_values_combination)), fitted_values_combination[r])
    }
    
    gini_c = gini.wtd(fitted_values_combination)
    theil_c = theil.wtd(fitted_values_combination)
    mld_c = mld.wtd(fitted_values_combination)
    
    all_gini_contributions[i] = ((gini_c - GINI[3])/GINI[3])
    all_theil_contributions[i] = ((theil_c - THEIL[3])/THEIL[3])
    all_mld_contributions[i] = ((mld_c - MLD[3])/MLD[3])
  }
  
  # Coffee tree decomposition 
  model_matrix_combination = as.data.frame(model_matrix_all)
  model_matrix_combination$COFFEE_TREES = mean(model_matrix_combination$COFFEE_TREES, na.rm = TRUE)
  model_matrix_combination$COFFEE_TREES_SQR = mean(model_matrix_combination$COFFEE_TREES_SQR, na.rm = TRUE)
  fitted_values_combination = as.matrix(model_matrix_combination) %*% as.matrix(coefficients_all)
  for (r in 1:length(fitted_values_combination)){
    fitted_values_combination[r] = ifelse(fitted_values_combination[r]<0, fitted_values_combination[r] + abs(min(fitted_values_combination)), fitted_values_combination[r])
  }
  gini_c = gini.wtd(fitted_values_combination)
  theil_c = theil.wtd(fitted_values_combination)
  mld_c = mld.wtd(fitted_values_combination)
  idx = length(opportunity_gini_contributions)-1
  all_gini_contributions[idx] = ((gini_c - GINI[3])/GINI[3])
  all_theil_contributions[idx] = ((theil_c - THEIL[3])/THEIL[3])
  all_mld_contributions[idx] = ((mld_c - MLD[3])/MLD[3])
  
  #Spatial decomposition
  model_matrix_combination = as.data.frame(model_matrix_all)
  model_matrix_combination$DISTANCE_PAVED_ROAD = mean(model_matrix_combination$DISTANCE_PAVED_ROAD, na.rm = TRUE)
  model_matrix_combination$ALTITUDE = mean(model_matrix_combination$ALTITUDE, na.rm = TRUE)
  fitted_values_combination = as.matrix(model_matrix_combination) %*% as.matrix(coefficients_all)
  for (r in 1:length(fitted_values_combination)){
    fitted_values_combination[r] = ifelse(fitted_values_combination[r]<0, fitted_values_combination[r] + abs(min(fitted_values_combination)), fitted_values_combination[r])
  }
  gini_c = gini.wtd(fitted_values_combination)
  theil_c = theil.wtd(fitted_values_combination)
  mld_c = mld.wtd(fitted_values_combination)
  idx = length(opportunity_gini_contributions )
  all_gini_contributions[idx] = ((gini_c - GINI[3])/GINI[3])
  all_theil_contributions[idx] = ((theil_c - THEIL[3])/THEIL[3])
  all_mld_contributions[idx] = ((mld_c - MLD[3])/MLD[3])
  
  # Save results of decomposition. 
  opportunity_decomp_hh = data.frame("GINI" = c(GINI[1], opportunity_gini_contributions),
                                     "Theil" = c(THEIL[1], opportunity_theil_contributions),
                                     "mld" = c(MLD[1], opportunity_mld_contributions))
  rownames(opportunity_decomp_hh) = c("overall", C_hh, "coffee_trees_all", "spatial")
  
  preference_decomp_hh = data.frame("GINI" = c(GINI[2], preference_gini_contributions),
                                    "Theil" = c(THEIL[2], preference_theil_contributions),
                                    "mld" = c(MLD[2], preference_mld_contributions))
  rownames(preference_decomp_hh) = c("overall", P_hh, Z_hh)
  
  all_decomp_hh = data.frame("GINI" = c(GINI[3], all_gini_contributions),
                             "Theil" = c(THEIL[3],all_theil_contributions),
                             "mld" = c(MLD[3],all_mld_contributions))
  rownames(all_decomp_hh) = c("overall", C_hh, P_hh, Z_hh, "coffee_trees_all", "spatial")
  
  write.csv(opportunity_decomp_hh, file = here("Results", "Opportunity decomposition continuous full model.csv"))
  write.csv(preference_decomp_hh, file = here("Results", "Preference decomposition continuous full model.csv"))
  write.csv(all_decomp_hh, file = here("Results", "Outcome decomposition continuous full model.csv"))
  
  #############################
  ##### REDUCED FORM MODEL ####
  ############################
  
  #Estimate central model
  eq_hh_reduced =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(V_hh, collapse = " + "), sep = " + ")) 
  eq_hh_reduced = as.formula(eq_hh_reduced)
  model_hh_reduced = tobit(eq_hh_reduced, left = 0, right = 1, data = hh_data )
  model_summary_hh_reduced = summary(model_hh_reduced)
  
  #Retrieve coefficients and central fitted values
  coefficients = coef(model_hh_reduced)
  
  #Opportunity fitted values
  coef_opportunity = C_hh
  coefficients_opportunity = coefficients[coef_opportunity]
  model_matrix_opportunity = model.matrix(as.formula(eq_hh_reduced), data = hh_data)[, coef_opportunity]
  fitted_opportunity = as.matrix(model_matrix_opportunity) %*% as.matrix(coefficients_opportunity)
  
  #Preference fitted values
  coef_preferences = V_hh
  coefficients_preferences = coefficients[coef_preferences]
  model_matrix_preferences = model.matrix(as.formula(eq_hh_reduced), data = hh_data)[, coef_preferences]
  fitted_preferences = as.matrix(model_matrix_preferences) %*% as.matrix(coefficients_preferences)
  
  #Fitted outcomes (with error term)
  fitted_outcomes = fitted(model_hh_reduced) + residuals(model_hh_reduced)
  
  # #Add scalar constant to fitted values to ensure all are positive 
  min_fitted_opportunity = min(fitted_opportunity)
  min_fitted_preferences = min(fitted_preferences)
  min_fitted_outcomes = min(fitted_outcomes)
  
  if (min_fitted_opportunity <0){
    fitted_opportunity = fitted_opportunity + abs(min_fitted_opportunity)
  }
  if (min_fitted_preferences <0){
    fitted_preferences =  fitted_preferences + abs(min_fitted_preferences)
  }
  if (min_fitted_outcomes <0){
    fitted_outcomes = fitted_outcomes + abs(min_fitted_outcomes)
  }
  
  GINI = c(gini.wtd(fitted_opportunity),
           gini.wtd(fitted_preferences),
           gini.wtd(fitted_outcomes))
  THEIL = c(theil.wtd(fitted_opportunity),
            theil.wtd(fitted_preferences),
            theil.wtd(fitted_outcomes))
  MLD = c(mld.wtd(fitted_opportunity),
          mld.wtd(fitted_preferences),
          mld.wtd(fitted_outcomes))
  
  
  # Now retrieve contributions for opportunities      
  opportunity_gini_contributions = numeric(length(C_hh)+2)
  opportunity_theil_contributions = numeric(length(C_hh)+2)
  opportunity_mld_contributions = numeric(length(C_hh)+2)
  
  for (i in 1:length(C_hh)) {
    model_matrix_combination = model_matrix_opportunity 
    if (length(unique(model_matrix_combination[,i])) < 3){
      model_matrix_combination[,i] = 1
    } else {
      model_matrix_combination[,i] = mean(model_matrix_combination[,i])
    }
    fitted_values_combination = as.matrix(model_matrix_combination) %*% as.matrix(coefficients_opportunity)
    for (r in 1:length(fitted_values_combination)){
      fitted_values_combination[r] = ifelse(fitted_values_combination[r]<0, fitted_values_combination[r] + abs(min(fitted_values_combination)), fitted_values_combination[r])
    }
    gini_c = gini.wtd(fitted_values_combination)
    theil_c = theil.wtd(fitted_values_combination)
    mld_c = mld.wtd(fitted_values_combination)
    
    opportunity_gini_contributions[i] = ((gini_c - GINI[1])/GINI[1])
    opportunity_theil_contributions[i] = ((theil_c - THEIL[1])/THEIL[1])
    opportunity_mld_contributions[i] = ((mld_c - MLD[1])/MLD[1])
  }
  
  # Coffee tree decomposition 
  model_matrix_combination = as.data.frame(model_matrix_opportunity)
  model_matrix_combination$COFFEE_TREES = mean(model_matrix_combination$COFFEE_TREES, na.rm = TRUE)
  model_matrix_combination$COFFEE_TREES_SQR = mean(model_matrix_combination$COFFEE_TREES_SQR, na.rm = TRUE)
  fitted_values_combination = as.matrix(model_matrix_combination) %*% as.matrix(coefficients_opportunity)
  for (r in 1:length(fitted_values_combination)){
    fitted_values_combination[r] = ifelse(fitted_values_combination[r]<0, fitted_values_combination[r] + abs(min(fitted_values_combination)), fitted_values_combination[r])
  }
  gini_c = gini.wtd(fitted_values_combination)
  theil_c = theil.wtd(fitted_values_combination)
  mld_c = mld.wtd(fitted_values_combination)
  idx = length(opportunity_gini_contributions)-1
  opportunity_gini_contributions[idx] = ((gini_c - GINI[1])/GINI[1])
  opportunity_theil_contributions[idx] = ((theil_c - THEIL[1])/THEIL[1])
  opportunity_mld_contributions[idx] = ((mld_c - MLD[1])/MLD[1])
  
  #Spatial decomposition
  model_matrix_combination = as.data.frame(model_matrix_opportunity)
  model_matrix_combination$DISTANCE_PAVED_ROAD = mean(model_matrix_combination$DISTANCE_PAVED_ROAD, na.rm = TRUE)
  model_matrix_combination$ALTITUDE = mean(model_matrix_combination$ALTITUDE, na.rm = TRUE)
  fitted_values_combination = as.matrix(model_matrix_combination) %*% as.matrix(coefficients_opportunity)
  for (r in 1:length(fitted_values_combination)){
    fitted_values_combination[r] = ifelse(fitted_values_combination[r]<0, fitted_values_combination[r] + abs(min(fitted_values_combination)), fitted_values_combination[r])
  }
  gini_c = gini.wtd(fitted_values_combination)
  theil_c = theil.wtd(fitted_values_combination)
  mld_c = mld.wtd(fitted_values_combination)
  idx = length(opportunity_gini_contributions )
  opportunity_gini_contributions[idx] = ((gini_c - GINI[1])/GINI[1])
  opportunity_theil_contributions[idx] = ((theil_c - THEIL[1])/THEIL[1])
  opportunity_mld_contributions[idx] = ((mld_c - MLD[1])/MLD[1])
  
  # Now retrieve contributions for preferences  
  preference_vec = V_hh
  preference_gini_contributions = numeric(length(preference_vec))
  preference_theil_contributions = numeric(length(preference_vec))
  preference_mld_contributions = numeric(length(preference_vec))
  
  for (i in 1:length(preference_vec)) {
    model_matrix_combination = model_matrix_preferences
    if (length(unique(model_matrix_combination[,i])) < 3){
      model_matrix_combination[,i] = 1
    } else {
      model_matrix_combination[,i] = mean(model_matrix_combination[,i])
    }
    fitted_values_combination = as.matrix(model_matrix_combination) %*% as.matrix(coefficients_preferences)
   
    for (r in 1:length(fitted_values_combination)){
      fitted_values_combination[r] = ifelse(fitted_values_combination[r]<0, fitted_values_combination[r] + abs(min(fitted_values_combination)), fitted_values_combination[r])
    }
    gini_c = gini.wtd(fitted_values_combination)
    theil_c = theil.wtd(fitted_values_combination)
    mld_c = mld.wtd(fitted_values_combination)
    
    preference_gini_contributions[i] = ((gini_c - GINI[2])/GINI[2])
    preference_theil_contributions[i] = ((theil_c - THEIL[2])/THEIL[2])
    preference_mld_contributions[i] = ((mld_c - MLD[2])/MLD[2])
  }
  
  # Now retrieve contributions for outcomes  
  all_vec = c(C_hh, V_hh)
  all_gini_contributions = numeric(length(all_vec)+2)
  all_theil_contributions = numeric(length(all_vec)+2)
  all_mld_contributions = numeric(length(all_vec)+2)
  model_matrix_all = model.matrix(as.formula(eq_hh_reduced), data = hh_data)
  coefficients_all = coef(model_hh_reduced)
  
  
  for (i in 1:length(all_vec)) {
    model_matrix_combination = model_matrix_all
    if (length(unique(model_matrix_combination[,i])) < 3){
      model_matrix_combination[,i] = 1
    } else {
      model_matrix_combination[,i] = mean(model_matrix_combination[,i])
    }
    fitted_values_combination = as.matrix(model_matrix_combination) %*% as.matrix(coefficients_all)

    for (r in 1:length(fitted_values_combination)){
      fitted_values_combination[r] = ifelse(fitted_values_combination[r]<0, fitted_values_combination[r] + abs(min(fitted_values_combination)), fitted_values_combination[r])
    }
    gini_c = gini.wtd(fitted_values_combination)
    theil_c = theil.wtd(fitted_values_combination)
    mld_c = mld.wtd(fitted_values_combination)
    
    all_gini_contributions[i] = ((gini_c - GINI[3])/GINI[3])
    all_theil_contributions[i] = ((theil_c - THEIL[3])/THEIL[3])
    all_mld_contributions[i] = ((mld_c - MLD[3])/MLD[3])
  }
  
  # Coffee tree decomposition 
  model_matrix_combination = as.data.frame(model_matrix_all)
  model_matrix_combination$COFFEE_TREES = mean(model_matrix_combination$COFFEE_TREES, na.rm = TRUE)
  model_matrix_combination$COFFEE_TREES_SQR = mean(model_matrix_combination$COFFEE_TREES_SQR, na.rm = TRUE)
  fitted_values_combination = as.matrix(model_matrix_combination) %*% as.matrix(coefficients_all)
  for (r in 1:length(fitted_values_combination)){
    fitted_values_combination[r] = ifelse(fitted_values_combination[r]<0, fitted_values_combination[r] + abs(min(fitted_values_combination)), fitted_values_combination[r])
  }
  gini_c = gini.wtd(fitted_values_combination)
  theil_c = theil.wtd(fitted_values_combination)
  mld_c = mld.wtd(fitted_values_combination)
  idx = length(opportunity_gini_contributions)-1
  all_gini_contributions[idx] = ((gini_c - GINI[3])/GINI[3])
  all_theil_contributions[idx] = ((theil_c - THEIL[3])/THEIL[3])
  all_mld_contributions[idx] = ((mld_c - MLD[3])/MLD[3])
  
  #Spatial decomposition
  model_matrix_combination = as.data.frame(model_matrix_all)
  model_matrix_combination$DISTANCE_PAVED_ROAD = mean(model_matrix_combination$DISTANCE_PAVED_ROAD, na.rm = TRUE)
  model_matrix_combination$ALTITUDE = mean(model_matrix_combination$ALTITUDE, na.rm = TRUE)
  fitted_values_combination = as.matrix(model_matrix_combination) %*% as.matrix(coefficients_all)
  for (r in 1:length(fitted_values_combination)){
    fitted_values_combination[r] = ifelse(fitted_values_combination[r]<0, fitted_values_combination[r] + abs(min(fitted_values_combination)), fitted_values_combination[r])
  }
  gini_c = gini.wtd(fitted_values_combination)
  theil_c = theil.wtd(fitted_values_combination)
  mld_c = mld.wtd(fitted_values_combination)
  idx = length(opportunity_gini_contributions )
  all_gini_contributions[idx] = ((gini_c - GINI[3])/GINI[3])
  all_theil_contributions[idx] = ((theil_c - THEIL[3])/THEIL[3])
  all_mld_contributions[idx] = ((mld_c - MLD[3])/MLD[3])
  
  # Save results of decomposition. 
  opportunity_decomp_hh = data.frame("GINI" = c(GINI[1], opportunity_gini_contributions),
                                     "Theil" = c(THEIL[1], opportunity_theil_contributions),
                                     "mld" = c(MLD[1], opportunity_mld_contributions))
  rownames(opportunity_decomp_hh) = c("overall", C_hh, "coffee_trees_all", "spatial")
  
  preference_decomp_hh = data.frame("GINI" = c(GINI[2], preference_gini_contributions),
                                    "Theil" = c(THEIL[2], preference_theil_contributions),
                                    "mld" = c(MLD[2], preference_mld_contributions))
  rownames(preference_decomp_hh) = c("overall", V_hh)
  
  all_decomp_hh = data.frame("GINI" = c(GINI[3], all_gini_contributions),
                             "Theil" = c(THEIL[3],all_theil_contributions),
                             "mld" = c(MLD[3],all_mld_contributions))
  rownames(all_decomp_hh) = c("overall", C_hh, V_hh, "coffee_trees_all", "spatial")
  
  write.csv(opportunity_decomp_hh, file = here("Results", "Opportunity decomposition continuous reduced model.csv"))
  write.csv(preference_decomp_hh, file = here("Results", "Preference decomposition continuous reduced model.csv"))
  write.csv(all_decomp_hh, file = here("Results", "Outcome decomposition continuous reduced model.csv"))
  
}
# End of function





  