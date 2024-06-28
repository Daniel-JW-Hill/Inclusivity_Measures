
# This function performs simple logit models to retrieve the fitted opportunity and outcome
# for the direct model (i.e. inclusive of preferences) 
# 

get_fitted_binary_full = function(hh_data, 
                                  indiv_data,
                                  Outcome_hh,
                                  C_hh,
                                  P_hh,
                                  Z_hh,
                                  Outcome_indiv,
                                  C_indiv,
                                  P_indiv,
                                  Z_indiv) {
  
  ###################
  ##### HH MODEL ####
  ###################
  
  #Install Firth's bias reduction method for logistic regression
  library(logistf)
  
  #Equation
  eq_hh_full =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(P_hh, collapse = " + "), Z_hh, sep = " + "))

  #Estimate model
  model_hh_full = logistf(eq_hh_full, data = hh_data)
  
  #Summary of model
  model_summary_hh_full = summary(model_hh_full)

  #Perform tests on model and save table
  log_likelihood = model_summary_hh_full$loglik
  McFaddenR2  =  1 - (model_summary_hh_full$loglik[1]/model_summary_hh_full$loglik[2])
  predicted_probs = model_hh_full$predict
  predicted_classes = ifelse(predicted_probs > 0.5, 1, 0)
  correct_predictions = sum(predicted_classes == hh_data[,names(hh_data) == Outcome_hh])
  percent_correct = (correct_predictions / nrow(hh_data)) * 100
  
  tests_df = data.frame(
    log_likelihood = log_likelihood,
    McfaddenR2 = McFaddenR2,
    percent_correct = percent_correct
  )
  
  write.csv(tests_df, here("results", "Binary  Household model - full - tests.csv"))
  
  results_hh_model_full = data.frame("coefs" = model_hh_full$coefficients,
                                     "odds" = exp(model_hh_full$coefficients),
                                     "standard_errors" = sqrt(diag(vcov(model_hh_full))),
                                     "probs" = model_hh_full$prob)
  
  write.csv(results_hh_model_full, here("results", "Binary  Household model - full - results.csv"))

  # Equalise variation for estimating fitted values
  hh_data_cbar = hh_data_pbar =hh_data
  hh_data_cbar[C_hh] = lapply(hh_data_cbar[C_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[P_hh] = lapply(hh_data_pbar[P_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[Z_hh] = lapply(hh_data_pbar[Z_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  
  #Opportunity fitted values
  predicted_probs_opportunity =  predict(model_hh_full, newdata = hh_data_pbar, type = 'response')
  
  #Preference fitted values
  predicted_probs_preferences = predict(model_hh_full, newdata = hh_data_cbar, type = 'response')
  
  #Fitted outcomes (without error term)
  predicted_probs_total = predict(model_hh_full, data = hh_data, type = 'response')
  
  #Combine into single output
  fitted_values_hh_model = cbind(predicted_probs_opportunity, predicted_probs_preferences, predicted_probs_total)
  colnames(fitted_values_hh_model) = c("fitted_opportunity", "fitted_preferences", "fitted")
  
  #Draw sample of coefficients to calculate inequality distributions
  b = 1000
  
  fitted_opportunity_hh_model_dist = matrix(NA, nrow = nrow(fitted_values_hh_model), ncol = b)
  fitted_preference_hh_model_dist = matrix(NA, nrow = nrow(fitted_values_hh_model), ncol = b)
  fitted_hh_model_dist = matrix(NA, nrow = nrow(fitted_values_hh_model), ncol = b)
  model_matrix_all =  model.matrix(as.formula(eq_hh_full), data = hh_data)
  
  for (bb in 1:b){
    #Draw sample of coefficients from coef and sd 
    for (r in 1:nrow(results_hh_model_full)){
      results_hh_model_full$new_coefs[r] = rnorm(1, mean = results_hh_model_full$coefs[r], sd = results_hh_model_full$standard_errors[r])
    }
    coefficients_bb = results_hh_model_full$new_coefs[which(rownames(results_hh_model_full) %in% coef_opportunity)]
    linear_predictor_opportunity_bb = as.matrix(model_matrix_opportunity) %*% as.matrix(coefficients_bb)
    predicted_probs_opportunity_bb = 1 / (1 + exp(-linear_predictor_opportunity_bb))
    fitted_opportunity_hh_model_dist[,bb] = predicted_probs_opportunity_bb
    
    coefficients_preferences_bb = results_hh_model_full$new_coefs[which(rownames(results_hh_model_full) %in% coef_preferences)]
    linear_predictor_preferences_bb  = as.matrix(model_matrix_preferences ) %*% as.matrix(coefficients_preferences_bb )
    fitted_preference_hh_model_dist[,bb]  = 1 / (1 + exp(-linear_predictor_preferences_bb ))
    
    coefficients_all = results_hh_model_full$new_coefs
    linear_predictor_all_bb = as.matrix(model_matrix_all) %*% as.matrix(coefficients_all)
    predicted_probs_all_bb = 1 / (1 + exp(-linear_predictor_all_bb))
    fitted_hh_model_dist[,bb] = predicted_probs_all_bb
  }
  
  ######################
  ##### INDIV MODEL ####
  ######################
  
  #Equation
  eq_indiv_full =  paste(Outcome_indiv, sep = " ~ ", paste(paste(C_indiv, collapse = " + "), paste(P_indiv, collapse = " + "), Z_indiv, sep = " + "))
  
  #Estimate model
  model_indiv_full = logistf(eq_indiv_full, data = indiv_data)
  
  #Summary of model
  model_summary_indiv_full = summary(model_indiv_full)
  
  #Perform tests on model and save table
  log_likelihood = model_summary_indiv_full$loglik
  McFaddenR2  =  1 - (model_summary_indiv_full$loglik[1]/model_summary_indiv_full$loglik[2])
  predicted_probs = model_indiv_full$predict
  predicted_classes = ifelse(predicted_probs > 0.5, 1, 0)
  correct_predictions = sum(predicted_classes == indiv_data[,names(indiv_data) == Outcome_indiv])
  percent_correct = (correct_predictions / nrow(indiv_data)) * 100
  
  tests_df = data.frame(
    log_likelihood = log_likelihood,
    McfaddenR2 = McFaddenR2,
    percent_correct = percent_correct
  )
  
  write.csv(tests_df, here("results", "Binary individual model - full - tests.csv"))
  
  results_indiv_model_full = data.frame("coefs" = model_indiv_full$coefficients,
                                     "odds" = exp(model_indiv_full$coefficients),
                                     "standard_errors" = sqrt(diag(vcov(model_indiv_full))),
                                     "probs" = model_indiv_full$prob)
  
  write.csv(results_indiv_model_full, here("results", "Binary individual model - full - results.csv"))
  
  #Retrieve coefficients and residuals
  coefficients = model_indiv_full$coefficients
  
  #Opportunity fitted values
  coef_opportunity = C_indiv
  coefficients_opportunity = coefficients[coef_opportunity]
  model_matrix_opportunity = model.matrix(as.formula(eq_indiv_full), data = indiv_data)[, coef_opportunity]
  linear_predictor_opportunity = as.matrix(model_matrix_opportunity) %*% as.matrix(coefficients_opportunity)
  predicted_probs_opportunity = 1 / (1 + exp(-linear_predictor_opportunity))
  
  #Preference fitted values
  coef_preferences = c(P_indiv, Z_indiv)
  coefficients_preferences = coefficients[coef_preferences]
  model_matrix_preferences = model.matrix(as.formula(eq_indiv_full), data = indiv_data)[, coef_preferences]
  linear_predictor_preferences = as.matrix(model_matrix_preferences) %*% as.matrix(coefficients_preferences)
  predicted_probs_preferences = 1 / (1 + exp(-linear_predictor_preferences))
  
  #Fitted outcomes (without error term)
  model_matrix_all =  model.matrix(as.formula(eq_indiv_full), data = indiv_data)
  linear_predictor = as.matrix(model_matrix_all) %*% as.matrix(coefficients)
  predicted_probs_total = 1 / (1 + exp(-linear_predictor))
  
  #Combine into single output
  fitted_values_indiv_model = cbind(predicted_probs_opportunity, predicted_probs_preferences, predicted_probs_total)
  colnames(fitted_values_indiv_model) = c("fitted_opportunity", "fitted_preferences", "fitted")
  
  #Draw sample of coefficients to calculate inequality distributions
  b = 1000
  
  fitted_opportunity_indiv_model_dist = matrix(NA, nrow = nrow(fitted_values_indiv_model), ncol = b)
  fitted_preference_indiv_model_dist = matrix(NA, nrow = nrow(fitted_values_indiv_model), ncol = b)
  fitted_indiv_model_dist = matrix(NA, nrow = nrow(fitted_values_indiv_model), ncol = b)
  model_matrix_all =  model.matrix(as.formula(eq_indiv_full), data = indiv_data)
  
  for (bb in 1:b){
    #Draw sample of coefficients from coef and sd 
    for (r in 1:nrow(results_hh_model_full)){
      results_indiv_model_full$new_coefs[r] = rnorm(1, mean = results_indiv_model_full$coefs[r], sd = results_indiv_model_full$standard_errors[r])
    }
    coefficients_bb = results_indiv_model_full$new_coefs[which(rownames(results_indiv_model_full) %in% coef_opportunity)]
    linear_predictor_opportunity_bb = as.matrix(model_matrix_opportunity) %*% as.matrix(coefficients_bb)
    predicted_probs_opportunity_bb = 1 / (1 + exp(-linear_predictor_opportunity_bb))
    fitted_opportunity_indiv_model_dist[,bb] = predicted_probs_opportunity_bb
    
    coefficients_preferences_bb = results_indiv_model_full$new_coefs[which(rownames(results_indiv_model_full) %in% coef_preferences)]
    linear_predictor_preferences_bb  = as.matrix(model_matrix_preferences ) %*% as.matrix(coefficients_preferences_bb )
    fitted_preference_indiv_model_dist[,bb]  = 1 / (1 + exp(-linear_predictor_preferences_bb ))
    
    coefficients_all = results_indiv_model_full$new_coefs
    linear_predictor_all_bb = as.matrix(model_matrix_all) %*% as.matrix(coefficients_all)
    predicted_probs_all_bb = 1 / (1 + exp(-linear_predictor_all_bb))
    fitted_indiv_model_dist[,bb] = predicted_probs_all_bb
  }
  
  fitted_values_hh = list(fitted_values_hh_model, 
                          fitted_opportunity_hh_model_dist,
                          fitted_preference_hh_model_dist,
                          fitted_hh_model_dist)
  names(fitted_values_hh) = c("Central_fitted","Dist_fitted_opportunity", "Dist_fitted_preferences","Dist_fitted_all" )
  
  fitted_values_indiv = list(fitted_values_indiv_model, 
                          fitted_opportunity_indiv_model_dist, 
                          fitted_preference_indiv_model_dist,
                          fitted_indiv_model_dist)
  names(fitted_values_indiv) = c("Central_fitted","Dist_fitted_opportunity", "Dist_fitted_preferences","Dist_fitted_all" )
  
  fitted_binary_full = list(fitted_values_hh,
                           fitted_values_indiv)
  names(fitted_binary_full) = c("HH_models", "Individual_models")
  
  return(fitted_binary_full)
         
} 
         
         