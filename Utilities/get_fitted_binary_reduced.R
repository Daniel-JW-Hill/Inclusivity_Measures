
# This function performs simple logit models to retrieve the fitted opportunity and outcome
# values for the reduced form model. 
# 

get_fitted_binary_reduced = function(hh_data, 
                                  indiv_data,
                                  Outcome_hh,
                                  C_hh,
                                  V_hh,
                                  Outcome_indiv,
                                  C_indiv,
                                  V_indiv) {
  
  ###################
  ##### HH MODEL ####
  ###################
  
  library(logistf)
  
  #Equation
  eq_hh_reduced =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(V_hh, collapse = " + "), sep = " + "))
  
  #Estimate model
  model_hh_reduced =  logistf(eq_hh_reduced, data = hh_data)
  
  #Summary of model
  model_summary_hh_reduced = summary(model_hh_reduced)
  
  #Perform tests on model and save table
  log_likelihood = model_summary_hh_reduced$loglik
  McFaddenR2  =  1 - (model_summary_hh_reduced$loglik[1]/model_summary_hh_reduced$loglik[2])
  McFaddenR2_adj = 1 - ((model_summary_hh_reduced$loglik[1] - length(model_summary_hh_reduced$terms))/model_summary_hh_reduced$loglik[2])
  predicted_probs = model_hh_reduced$predict
  predicted_classes = ifelse(predicted_probs > 0.5, 1, 0)
  correct_predictions = sum(predicted_classes == hh_data[,names(hh_data) == Outcome_hh])
  percent_correct = (correct_predictions / nrow(hh_data)) * 100
  model_null = logistf(BINARY_PARTICIPATION ~ 1, data = hh_data)
  model_null_summary = summary(model_null)
  log_likelihood_null = model_null_summary$loglik
  LRtestStat = -2*(log_likelihood_null[1] - log_likelihood[1])
  
  tests_df = data.frame(
    log_likelihood = log_likelihood[1],
    McfaddenR2 = McFaddenR2,
    McFaddenR2_adj = McFaddenR2_adj,
    percent_correct = percent_correct,
    LRtestStat = LRtestStat
  )
  
  write.csv(tests_df, here("results", "Binary  Household model - reduced - tests.csv"))
  
  results_hh_model_reduced = data.frame("coefs" = model_hh_reduced$coefficients,
                                     "odds" = exp(model_hh_reduced$coefficients),
                                     "standard_errors" = sqrt(diag(vcov(model_hh_reduced))),
                                     "probs" = model_hh_reduced$prob)
  
  write.csv(results_hh_model_reduced, here("results", "Binary  Household model - reduced - results.csv"))
  
  # Equalise variation for estimating fitted values
  hh_data_cbar = hh_data_pbar =hh_data
  hh_data_cbar[C_hh] = lapply(hh_data_cbar[C_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[P_hh] = lapply(hh_data_pbar[P_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[Z_hh] = lapply(hh_data_pbar[Z_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  
  #Opportunity fitted values
  predicted_probs_opportunity =  predict(model_hh_reduced, newdata = hh_data_pbar, type = 'response' , se.fit = TRUE)
  
  #Preference fitted values
  predicted_probs_preferences = predict(model_hh_reduced, newdata = hh_data_cbar, type = 'response' , se.fit = TRUE)
  
  #Fitted outcomes (without error term)
  predicted_probs_total = predict(model_hh_reduced, data = hh_data, type = 'response', se.fit = TRUE)
  
  #Combine into single output
  fitted_values_hh_model = cbind(predicted_probs_opportunity$fit, predicted_probs_preferences$fit, predicted_probs_total$fit)
  colnames(fitted_values_hh_model) = c("fitted_opportunity", "fitted_preferences", "fitted")
  
  #Draw sample of coefficients to calculate inequality distributions
  b = 500
  min_outcomes = 15
  
  fitted_opportunity_hh_model_dist = matrix(NA, nrow = nrow(fitted_values_hh_model), ncol = b)
  fitted_opportunity_hh_model_dist_onlysig = matrix(NA, nrow = nrow(fitted_values_hh_model), ncol = b)
  fitted_preference_hh_model_dist = matrix(NA, nrow = nrow(fitted_values_hh_model), ncol = b)
  fitted_hh_model_dist = matrix(NA, nrow = nrow(fitted_values_hh_model), ncol = b)
  
  for (bb in 1:b){
    # Bootstrap  sample and re-estimate models, with min outcomes required
    min_outcomes_condition  = FALSE
    while (min_outcomes_condition == FALSE){
      bootstrap_indices = sample(1:nrow(hh_data), size = nrow(hh_data), replace = TRUE)
      bootstrap_sample = hh_data[bootstrap_indices, ]
      if (length(bootstrap_sample$BINARY_PARTICIPATION[bootstrap_sample$BINARY_PARTICIPATION > 0]) > min_outcomes) {
        # If condition is met, set the flag to True to exit the loop
        min_outcomes_condition = TRUE
      }
    }
    
    #re-estimate model
    model_hh_reduced_boot =  logistf(eq_hh_reduced, data = bootstrap_sample)
    
    #Check if converged (i.e converged before max iterations), if no add central scenario
    if ( model_hh_reduced_boot$iter[1] ==0){
      fitted_opportunity_hh_model_dist[,bb] = predicted_probs_opportunity$fit 
      fitted_preferences_hh_model_dist[,bb] = predicted_probs_preferences$fit 
      fitted_hh_model_dist[,bb] = predicted_probs_total$fit 
    }
    
    #Fitted values - use original sample but results from bootstrap
    fitted_opportunity_hh_model_dist[,bb] = predict(model_hh_reduced_boot, newdata = hh_data_pbar, type = 'response')
    fitted_preference_hh_model_dist[,bb] = predict(model_hh_reduced_boot, newdata = hh_data_cbar, type = 'response')
    fitted_hh_model_dist[,bb] = predict(model_hh_reduced_boot, newdata = hh_data, type = 'response')
    
    # Finally retrieve central estimate only for the significant coefficients. 
    for (coef in 1:length(model_hh_reduced_boot$coefficients)){
      if (model_hh_reduced_boot$prob[coef]>0.1){
        model_hh_reduced_boot$coefficients[coef] = 0
      }
    }
    fitted_opportunity_hh_model_dist_onlysig[,bb] = predict(model_hh_reduced_boot, newdata = hh_data_pbar, type = 'response')
    
  }
  

  ######################
  ##### INDIV MODEL ####
  ######################
  
  #Equation
  eq_indiv_reduced =  paste(Outcome_indiv, sep = " ~ ", paste(paste(C_indiv, collapse = " + "), paste(V_indiv, collapse = " + "),  sep = " + "))
  
  #Estimate model
  model_indiv_reduced = logistf(eq_indiv_reduced, data = indiv_data)
  
  #Summary of model
  model_summary_indiv_reduced = summary(model_indiv_reduced)
  
  #Perform tests on model and save table
  log_likelihood = model_summary_indiv_reduced$loglik
  McFaddenR2  =  1 - (model_summary_indiv_reduced$loglik[1]/model_summary_indiv_reduced$loglik[2])
  McFaddenR2_adj = 1 - ((model_summary_indiv_reduced$loglik[1] - length(model_summary_indiv_reduced$terms))/model_summary_indiv_reduced$loglik[2])
  predicted_probs = model_indiv_reduced$predict
  predicted_classes = ifelse(predicted_probs > 0.5, 1, 0)
  correct_predictions = sum(predicted_classes == indiv_data[,names(indiv_data) == Outcome_indiv])
  percent_correct = (correct_predictions / nrow(indiv_data)) * 100
  model_null = logistf(BINARY_PARTICIPATION ~ 1, data = indiv_data)
  model_null_summary = summary(model_null)
  log_likelihood_null = model_null_summary$loglik
  LRtestStat = -2*(log_likelihood_null[1] - log_likelihood[1])
  
  tests_df = data.frame(
    log_likelihood = log_likelihood[1],
    McfaddenR2 = McFaddenR2,
    McFaddenR2_adj = McFaddenR2_adj,
    percent_correct = percent_correct,
    LRtestStat = LRtestStat
  )
  
  write.csv(tests_df, here("results", "Binary individual model - reduced - tests.csv"))
  
  results_indiv_model_reduced = data.frame("coefs" = model_indiv_reduced$coefficients,
                                        "odds" = exp(model_indiv_reduced$coefficients),
                                        "standard_errors" = sqrt(diag(vcov(model_indiv_reduced))),
                                        "probs" = model_indiv_reduced$prob)
  
  write.csv(results_indiv_model_reduced, here("results", "Binary individual model - reduced - results.csv"))
  
  # Equalise variation for estimating fitted values
  indiv_data_cbar = indiv_data_pbar = indiv_data
  indiv_data_cbar[C_indiv] = lapply(indiv_data_cbar[C_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  indiv_data_pbar[P_indiv] = lapply(indiv_data_pbar[P_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  indiv_data_pbar[Z_indiv] = lapply(indiv_data_pbar[Z_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  
  #Opportunity fitted values
  predicted_probs_opportunity =  predict(model_indiv_reduced, newdata = indiv_data_pbar, type = 'response' , se.fit = TRUE)
  
  #Preference fitted values
  predicted_probs_preferences = predict(model_indiv_reduced, newdata = indiv_data_cbar, type = 'response' , se.fit = TRUE)
  
  #Fitted outcomes (without error term)
  predicted_probs_total = predict(model_indiv_reduced, data = indiv_data, type = 'response', se.fit = TRUE)
  
  #Combine into single output
  fitted_values_indiv_model = cbind(predicted_probs_opportunity$fit, predicted_probs_preferences$fit, predicted_probs_total$fit)
  colnames(fitted_values_indiv_model) = c("fitted_opportunity", "fitted_preferences", "fitted")
  
  #Draw sample of coefficients to calculate inequality distributions
  b = 500
  min_outcomes = 15
  
  fitted_opportunity_indiv_model_dist = matrix(NA, nrow = nrow(fitted_values_indiv_model), ncol = b)
  fitted_opportunity_indiv_model_dist_onlysig = matrix(NA, nrow = nrow(fitted_values_indiv_model), ncol = b)
  fitted_preference_indiv_model_dist = matrix(NA, nrow = nrow(fitted_values_indiv_model), ncol = b)
  fitted_indiv_model_dist = matrix(NA, nrow = nrow(fitted_values_indiv_model), ncol = b)
  
  
  for (bb in 1:b){
    # Bootstrap  sample and re-estimate models, with min outcomes required
    min_outcomes_condition  = FALSE
    while (min_outcomes_condition == FALSE){
      bootstrap_indices = sample(1:nrow(indiv_data), size = nrow(indiv_data), replace = TRUE)
      bootstrap_sample = indiv_data[bootstrap_indices, ]
      if (length(bootstrap_sample$BINARY_PARTICIPATION[bootstrap_sample$BINARY_PARTICIPATION > 0]) > min_outcomes) {
        # If condition is met, set the flag to True to exit the loop
        min_outcomes_condition = TRUE
      }
    }
    
    #re-estimate model
    model_indiv_reduced_boot =  logistf(eq_indiv_reduced, data = bootstrap_sample)
    
    #Check if converged (i.e converged before max iterations), if no add central scenario
    if ( model_indiv_reduced_boot$iter[1] ==0){
      fitted_opportunity_indiv_model_dist[,bb] = predicted_probs_opportunity$fit 
      fitted_preferences_indiv_model_dist[,bb] = predicted_probs_preferences$fit 
      fitted_indiv_model_dist[,bb] = predicted_probs_total$fit 
    }
    
    #Fitted values - use original sample but results from bootstrap
    fitted_opportunity_indiv_model_dist[,bb] = predict(model_indiv_reduced_boot, newdata = indiv_data_pbar, type = 'response')
    fitted_preference_indiv_model_dist[,bb] = predict(model_indiv_reduced_boot, newdata = indiv_data_cbar, type = 'response')
    fitted_indiv_model_dist[,bb] = predict(model_indiv_reduced_boot, newdata = indiv_data, type = 'response')
    
    # Finally retrieve central estimate only for the significant coefficients. 
    for (coef in 1:length(model_indiv_reduced_boot$coefficients)){
      if (model_indiv_reduced_boot$prob[coef]>0.1){
        model_indiv_reduced_boot$coefficients[coef] = 0
      }
    }
    fitted_opportunity_indiv_model_dist_onlysig[,bb] = predict(model_indiv_reduced_boot, newdata = indiv_data_pbar, type = 'response')
    
  }
  
 # Save results
  fitted_values_hh = list(fitted_values_hh_model, 
                          fitted_opportunity_hh_model_dist, 
                          fitted_preference_hh_model_dist,
                          fitted_hh_model_dist,
                          fitted_opportunity_hh_model_dist_onlysig,
                          model_hh_reduced)
  
  names(fitted_values_hh) = c("Central_fitted","Dist_fitted_opportunity", "Dist_fitted_preferences","Dist_fitted_all", "Central_onlysig", "model_hh_reduced")
  
  fitted_values_indiv = list(fitted_values_indiv_model, 
                             fitted_opportunity_indiv_model_dist,
                             fitted_preference_indiv_model_dist,
                             fitted_indiv_model_dist,
                             fitted_opportunity_indiv_model_dist_onlysig,
                             model_indiv_reduced)
  names(fitted_values_indiv) = c("Central_fitted","Dist_fitted_opportunity", "Dist_fitted_preferences","Dist_fitted_all", "Central_onlysig", "model_indiv_reduced")
  
  fitted_binary_full = list(fitted_values_hh,
                            fitted_values_indiv)
  names(fitted_binary_full) = c("HH_models", "Individual_models")
  
  return(fitted_binary_full)
  
}
