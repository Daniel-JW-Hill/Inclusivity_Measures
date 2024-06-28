
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
  McFaddenR2_adj = 1 - ((model_summary_hh_full$loglik[1] - length(model_summary_hh_full$terms))/model_summary_hh_full$loglik[2])
  predicted_probs = model_hh_full$predict
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
  predicted_probs_opportunity =  predict(model_hh_full, newdata = hh_data_pbar, type = 'response' , se.fit = TRUE)
  
  #Preference fitted values
  predicted_probs_preferences = predict(model_hh_full, newdata = hh_data_cbar, type = 'response' , se.fit = TRUE)
  
  #Fitted outcomes (without error term)
  predicted_probs_total = predict(model_hh_full, data = hh_data, type = 'response', se.fit = TRUE)
  
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
    model_hh_full_boot =  logistf(eq_hh_full, data = bootstrap_sample)
    
    #Check if converged (i.e converged before max iterations), if no add central scenario
    if ( model_hh_full_boot$iter[1] ==0){
      fitted_opportunity_hh_model_dist[,bb] = predicted_probs_opportunity$fit 
      fitted_preferences_hh_model_dist[,bb] = predicted_probs_preferences$fit 
      fitted_hh_model_dist[,bb] = predicted_probs_total$fit 
    }
    
    #Fitted values - use original sample but results from bootstrap
    fitted_opportunity_hh_model_dist[,bb] = predict(model_hh_full_boot, newdata = hh_data_pbar, type = 'response')
    fitted_preference_hh_model_dist[,bb] = predict(model_hh_full_boot, newdata = hh_data_cbar, type = 'response')
    fitted_hh_model_dist[,bb] = predict(model_hh_full_boot, newdata = hh_data, type = 'response')
    
    # Finally retrieve central estimate only for the significant coefficients. 
    for (coef in 1:length(model_hh_full_boot$coefficients)){
      if (model_hh_full_boot$prob[coef]>0.1){
        model_hh_full_boot$coefficients[coef] = 0
      }
    }
    fitted_opportunity_hh_model_dist_onlysig[,bb]= predict(model_hh_full_boot, newdata = hh_data_pbar, type = 'response')
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
  McFaddenR2_adj = 1 - ((model_summary_indiv_full$loglik[1] - length(model_summary_indiv_full$terms))/model_summary_indiv_full$loglik[2])
  predicted_probs = model_indiv_full$predict
  predicted_classes = ifelse(predicted_probs > 0.5, 1, 0)
  correct_predictions = sum(predicted_classes == indiv_data[,names(indiv_data) == Outcome_indiv])
  percent_correct = (correct_predictions / nrow(indiv_data)) * 100
  model_null = logistf(BINARY_PARTICIPATION ~ 1, data = indiv_data)
  model_null_summary = summary(model_null)
  log_likelihood_null = model_null_summary$loglik
  LRtestStat = -2*(log_likelihood_null[1] - log_likelihood[1])
  
  tests_df = data.frame(
    log_likelihood = log_likelihood,
    McfaddenR2 = McFaddenR2,
    McFaddenR2_adj = McFaddenR2_adj,
    percent_correct = percent_correct,
    LRtestStat = LRtestStat
  )
  
  write.csv(tests_df, here("results", "Binary individual model - full - tests.csv"))
  
  results_indiv_model_full = data.frame("coefs" = model_indiv_full$coefficients,
                                     "odds" = exp(model_indiv_full$coefficients),
                                     "standard_errors" = sqrt(diag(vcov(model_indiv_full))),
                                     "probs" = model_indiv_full$prob)
  
  write.csv(results_indiv_model_full, here("results", "Binary individual model - full - results.csv"))
  
  # Equalise variation for estimating fitted values
  indiv_data_cbar = indiv_data_pbar = indiv_data
  indiv_data_cbar[C_hh] = lapply(indiv_data_cbar[C_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  indiv_data_pbar[P_hh] = lapply(indiv_data_pbar[P_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  indiv_data_pbar[Z_hh] = lapply(indiv_data_pbar[Z_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  
  #Opportunity fitted values
  predicted_probs_opportunity =  predict(model_indiv_full, newdata = indiv_data_pbar, type = 'response' , se.fit = TRUE)
  
  #Preference fitted values
  predicted_probs_preferences = predict(model_indiv_full, newdata = indiv_data_cbar, type = 'response' , se.fit = TRUE)
  
  #Fitted outcomes (without error term)
  predicted_probs_total = predict(model_indiv_full, data = indiv_data, type = 'response', se.fit = TRUE)
  
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
    model_indiv_full_boot =  logistf(eq_indiv_full, data = bootstrap_sample)
    
    #Check if converged (i.e converged before max iterations), if no add central scenario
    if ( model_indiv_full_boot$iter[1] ==0){
      fitted_opportunity_indiv_model_dist[,bb] = predicted_probs_opportunity$fit 
      fitted_preferences_indiv_model_dist[,bb] = predicted_probs_preferences$fit 
      fitted_indiv_model_dist[,bb] = predicted_probs_total$fit 
    }
    
    #Fitted values - use original sample but results from bootstrap
    fitted_opportunity_indiv_model_dist[,bb] = predict(model_indiv_full_boot, newdata = indiv_data_pbar, type = 'response')
    fitted_preference_indiv_model_dist[,bb] = predict(model_indiv_full_boot, newdata = indiv_data_cbar, type = 'response')
    fitted_indiv_model_dist[,bb] = predict(model_indiv_full_boot, newdata = indiv_data, type = 'response')
    
    # Finally retrieve central estimate only for the significant coefficients. 
    for (coef in 1:length(model_indiv_full_boot$coefficients)){
      if (model_indiv_full_boot$prob[coef]>0.1){
        model_indiv_full_boot$coefficients[coef] = 0
      }
    }
    fitted_opportunity_indiv_model_dist_onlysig[,bb] = predict(model_indiv_full_boot, newdata = indiv_data_pbar, type = 'response')
  }
  
  #Save all results
  fitted_values_hh = list(fitted_values_hh_model, 
                          fitted_opportunity_hh_model_dist,
                          fitted_preference_hh_model_dist,
                          fitted_hh_model_dist, 
                          fitted_opportunity_hh_model_dist_onlysig)
  names(fitted_values_hh) = c("Central_fitted","Dist_fitted_opportunity", "Dist_fitted_preferences","Dist_fitted_all", "Central_onlysig" )
  
  fitted_values_indiv = list(fitted_values_indiv_model, 
                          fitted_opportunity_indiv_model_dist, 
                          fitted_preference_indiv_model_dist,
                          fitted_indiv_model_dist,
                          fitted_opportunity_indiv_model_dist_onlysig)
  names(fitted_values_indiv) = c("Central_fitted","Dist_fitted_opportunity", "Dist_fitted_preferences","Dist_fitted_all", "Central_onlysig"  )
  
  fitted_binary_full = list(fitted_values_hh,
                           fitted_values_indiv)
  names(fitted_binary_full) = c("HH_models", "Individual_models")
  
  return(fitted_binary_full)
         
} 
         
         