
# This function performs the continuous measure estimation for the full model
# Only for the household model. 

get_fitted_continuous_full = function(hh_data, 
                                  Outcome_hh,
                                  C_hh,
                                  P_hh,
                                  Z_hh) {
  
  
  library(glmmTMB)
  library(performance)
  
  #Equation
  eq_hh_full =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(P_hh, collapse = " + "), Z_hh, sep = " + "))
  eq_hh_zero = paste(" ~ ", paste(paste(C_hh, collapse = " + "), paste(P_hh, collapse = " + "), Z_hh, sep = " + "), sep = "")
  
  eq_hh_full = as.formula(eq_hh_full)
  eq_hh_zero = as.formula(eq_hh_zero)
  
  #Estimate model
  model_hh_full = glmmTMB(eq_hh_full, data = hh_data, family = beta_family(link = "logit"), ziformula = eq_hh_zero, dispformula = ~ 1)

  #Summary of model
  model_summary_hh_full = summary(model_hh_full)
  
  # Calculate pseudo R-squared Nakagawa R2 (2017)
  log_likelihood = model_summary_hh_full$logLik[1]
  nakagawa_R_squared = performance::r2(model_hh_full) # adjusted 
  # Null model for LR test
  eq_null = as.formula("CONTINUOUS_PARTICIPATION ~ 1")
  model_null = glmmTMB(eq_null, data = hh_data, family = beta_family(link = "logit"), ziformula = eq_null, dispformula = ~ 1)
  model_null_summary = summary(model_null)
  LRtestStat = -2*(model_summary_hh_full$logLik - model_null_summary$logLik)
  
  tests_df = data.frame(
    "log_likelihood" = log_likelihood,
    "Nagakawa_R_squared" = nakagawa_R_squared[1],
    "Nagakawa_R_squared_adj" = nakagawa_R_squared[2],
    "LRteststat" = LRtestStat 
  )
  
  write.csv(tests_df, here("results", "Continuous Household model - full - tests.csv"))
  
  results_hh_model_full = data.frame("coefs" = c(model_summary_hh_full$coefficients[[1]][,1], model_summary_hh_full$coefficients[[2]][,1]),
                                     "standard_errors" = c(model_summary_hh_full$coefficients[[1]][,2], model_summary_hh_full$coefficients[[2]][,2]),
                                     "probs" = c(model_summary_hh_full$coefficients[[1]][,4], model_summary_hh_full$coefficients[[2]][,4]))
                                     
  write.csv(results_hh_model_full, here("results", "Continuous Household model - full - results.csv"))
  
  ### Create new data frames equalising preferences, and equalising circumstances. 
  hh_data_cbar = hh_data_pbar =hh_data
  hh_data_cbar[C_hh] = lapply(hh_data_cbar[C_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[P_hh] = lapply(hh_data_pbar[P_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[Z_hh] = lapply(hh_data_pbar[Z_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  
  #Opportunity fitted values
  pred_opportunity_zprob = predict(model_hh_full, newdata = hh_data_pbar, type = 'zprob', se.fit = TRUE)
  pred_opportunity_response = predict(model_hh_full, newdata = hh_data_pbar, type = 'response', se.fit = TRUE)
  pred_opportunity_conditional = predict(model_hh_full, newdata = hh_data_pbar, type = 'conditional', se.fit = TRUE)
  
  #Preference fitted values
  pred_preferences_zprob = predict(model_hh_full, newdata = hh_data_cbar, type = 'zprob', se.fit = TRUE)
  pred_preferences_response = predict(model_hh_full, newdata = hh_data_cbar, type = 'response', se.fit = TRUE)
  pred_preferences_conditional = predict(model_hh_full, newdata = hh_data_cbar, type = 'conditional', se.fit = TRUE)
  
  #Fitted outcomes (without error term)
  pred_zprob = predict(model_hh_full, newdata = hh_data, type = 'zprob', se.fit = TRUE)
  pred_response = predict(model_hh_full, newdata = hh_data, type = 'response', se.fit = TRUE)
  pred_conditional = predict(model_hh_full, newdata = hh_data, type = 'conditional', se.fit = TRUE)
  
  #Combine into single output
  fitted_values_hh_model_zprob = cbind(pred_opportunity_zprob$fit , pred_preferences_zprob$fit, pred_zprob$fit)
  colnames(fitted_values_hh_model_zprob) = c("fitted_opportunity", "fitted_preferences", "fitted")
  
  fitted_values_hh_model_response = cbind(pred_opportunity_response$fit , pred_preferences_response$fit, pred_response$fit)
  colnames(fitted_values_hh_model_response ) = c("fitted_opportunity", "fitted_preferences", "fitted")
  
  fitted_values_hh_model_conditional = cbind(pred_opportunity_conditional$fit , pred_preferences_conditional$fit, pred_conditional$fit)
  colnames(fitted_values_hh_model_conditional ) = c("fitted_opportunity", "fitted_preferences", "fitted")
  
  #Extract distribution of coefficients and fitted values for intervals through bootstrap. 
  b = 500
  min_outcomes = 15
  
  fitted_opportunity_hh_model_zprob_dist = matrix(NA, nrow = nrow(fitted_values_hh_model_zprob), ncol = b)
  fitted_opportunity_hh_model_zprob_dist_onlysig = matrix(NA, nrow = nrow(fitted_values_hh_model_zprob), ncol = b)
  fitted_preferences_hh_model_zprob_dist = matrix(NA, nrow = nrow(fitted_values_hh_model_zprob), ncol = b)
  fitted_hh_model_zprob_dist = matrix(NA, nrow = nrow(fitted_values_hh_model_zprob), ncol = b)
  
  fitted_opportunity_hh_model_response_dist = matrix(NA, nrow = nrow(fitted_values_hh_model_response), ncol = b)
  fitted_opportunity_hh_model_response_dist_onlysig = matrix(NA, nrow = nrow(fitted_values_hh_model_response), ncol = b)
  fitted_preferences_hh_model_response_dist = matrix(NA, nrow = nrow(fitted_values_hh_model_response), ncol = b)
  fitted_hh_model_response_dist = matrix(NA, nrow = nrow(fitted_values_hh_model_response), ncol = b)
  
  fitted_opportunity_hh_model_conditional_dist = matrix(NA, nrow = nrow(fitted_values_hh_model_conditional), ncol = b)
  fitted_opportunity_hh_model_conditional_dist_onlysig = matrix(NA, nrow = nrow(fitted_values_hh_model_conditional), ncol = b)
  fitted_preferences_hh_model_conditional_dist = matrix(NA, nrow = nrow(fitted_values_hh_model_conditional), ncol = b)
  fitted_hh_model_conditional_dist = matrix(NA, nrow = nrow(fitted_values_hh_model_conditional), ncol = b)
  
  for (bb in 1:b){
    # Bootstrap  sample and re-estimate models, with min outcomes required
    min_outcomes_condition  = FALSE
    while (min_outcomes_condition == FALSE){
      bootstrap_indices = sample(1:nrow(hh_data), size = nrow(hh_data), replace = TRUE)
      bootstrap_sample = hh_data[bootstrap_indices, ]
      if (length(bootstrap_sample$CONTINUOUS_PARTICIPATION[bootstrap_sample$CONTINUOUS_PARTICIPATION > 0]) > min_outcomes) {
        # If condition is met, set the flag to True to exit the loop
        min_outcomes_condition = TRUE
      }
    }
    
    #re-estimate model
    model_hh_full_boot = glmmTMB(eq_hh_full, data = bootstrap_sample, family = beta_family(link = "logit"), ziformula = eq_hh_zero, dispformula = ~ 1)
    
    #Check if converged, if no add central scenario
    if (any(is.na(summary(model_hh_full_boot)$coefficients$cond[,2]))){
      fitted_opportunity_hh_model_zprob_dist[,bb] = pred_opportunity_zprob$fit 
      fitted_opportunity_hh_model_response_dist[,bb] = pred_opportunity_response$fit 
      fitted_opportunity_hh_model_conditional_dist[,bb] = pred_opportunity_conditional$fit 
      fitted_preferences_hh_model_zprob_dist[,bb] = pred_preferences_zprob$fit 
      fitted_preferences_hh_model_response_dist[,bb] = pred_preferences_response$fit 
      fitted_preferences_hh_model_conditional_dist[,bb] = pred_preferences_conditional$fit 
      fitted_hh_model_zprob_dist[,bb] = pred_zprob$fit 
      fitted_hh_model_response_dist[,bb] = pred_response$fit 
      fitted_hh_model_conditional_dist[,bb] = pred_conditional$fit 
    }
    
    #Fitted values - use original sample but results from bootstrap
    fitted_opportunity_hh_model_zprob_dist[,bb] = predict(model_hh_full_boot, newdata = hh_data_pbar, type = 'zprob')
    fitted_opportunity_hh_model_response_dist[,bb] = predict(model_hh_full_boot, newdata = hh_data_pbar, type = 'response')
    fitted_opportunity_hh_model_conditional_dist[,bb] = predict(model_hh_full_boot, newdata = hh_data_pbar, type = 'conditional')
    fitted_preferences_hh_model_zprob_dist[,bb] = predict(model_hh_full_boot, newdata = hh_data_cbar, type = 'zprob')
    fitted_preferences_hh_model_response_dist[,bb] = predict(model_hh_full_boot, newdata = hh_data_cbar, type = 'response')
    fitted_preferences_hh_model_conditional_dist[,bb] = predict(model_hh_full_boot, newdata = hh_data_cbar, type = 'conditional')
    fitted_hh_model_zprob_dist[,bb] = predict(model_hh_full_boot, newdata = hh_data, type = 'zprob')
    fitted_hh_model_response_dist[,bb] = predict(model_hh_full_boot, newdata = hh_data, type = 'response')
    fitted_hh_model_conditional_dist[,bb] = predict(model_hh_full_boot, newdata = hh_data, type = 'conditional')
    
    # Finally retrieve central estimate only for the significant coefficients. 
    counter = 1
    
    if (any(is.na(summary(model_hh_full)$coefficients$cond[,2]))) {
      for (coef in 1:length(summary(model_hh_full_boot)$coefficients$cond[,1])){
        if (summary(model_hh_full)$coefficients$cond[coef,4] >0.1){
          model_hh_full$fit$par[counter] = 0
          model_hh_full$sdr$par.fixed[counter] = 0
        }
        counter = counter + 1
      }
      for (coef in 1:length(summary(model_hh_full)$coefficients$zi[,1])){
        if (summary(model_hh_full)$coefficients$cond[coef,4] >0.1){
          model_hh_full$fit$par[counter] = 0
          model_hh_full$sdr$par.fixed[counter] = 0
        }
        counter = counter + 1
      }   
      
      fitted_opportunity_hh_model_zprob_dist_onlysig[,bb] = predict(model_hh_full, newdata = hh_data_pbar, type = 'zprob')
      fitted_opportunity_hh_model_response_dist_onlysig[,bb] = predict(model_hh_full, newdata = hh_data_pbar, type = 'response')
      fitted_opportunity_hh_model_conditional_dist_onlysig[,bb] = predict(model_hh_full, newdata = hh_data_pbar, type = 'conditional')
      
    } else {
      
      for (coef in 1:length(summary(model_hh_full_boot)$coefficients$cond[,1])){
        if (summary(model_hh_full_boot)$coefficients$cond[coef,4] >0.1){
          model_hh_full_boot$fit$par[counter] = 0
          model_hh_full_boot$sdr$par.fixed[counter] = 0
        }
        counter = counter + 1
      }
      for (coef in 1:length(summary(model_hh_full_boot)$coefficients$zi[,1])){
        if (summary(model_hh_full_boot)$coefficients$cond[coef,4] >0.1){
          model_hh_full_boot$fit$par[counter] = 0
          model_hh_full_boot$sdr$par.fixed[counter] = 0
        }
        counter = counter + 1
      }   
      fitted_opportunity_hh_model_zprob_dist_onlysig[,bb] = predict(model_hh_full_boot, newdata = hh_data_pbar, type = 'zprob')
      fitted_opportunity_hh_model_response_dist_onlysig[,bb] = predict(model_hh_full_boot, newdata = hh_data_pbar, type = 'response')
      fitted_opportunity_hh_model_conditional_dist_onlysig[,bb] = predict(model_hh_full_boot, newdata = hh_data_pbar, type = 'conditional')
    }
  }
  
  # Save results
  fitted_values_hh_zprob= list(fitted_values_hh_model_zprob, 
                                   fitted_opportunity_hh_model_zprob_dist,
                                   fitted_preferences_hh_model_zprob_dist,
                                   fitted_hh_model_zprob_dist,
                                   fitted_opportunity_hh_model_zprob_dist_onlysig)
  names(fitted_values_hh_zprob) = c("Central_fitted", 
                                    "Dist_fitted_opportunity", 
                                    "Dist_fitted_preferences", 
                                    "Dist_fitted_all", 
                                    "Central_onlysig")
  
  fitted_values_hh_response = list(fitted_values_hh_model_response, 
                                    fitted_opportunity_hh_model_response_dist,
                                    fitted_preferences_hh_model_response_dist,
                                    fitted_hh_model_response_dist,
                                   fitted_opportunity_hh_model_response_dist_onlysig)
  names(fitted_values_hh_response) = c("Central_fitted", 
                                       "Dist_fitted_opportunity", 
                                       "Dist_fitted_preferences", 
                                       "Dist_fitted_all",
                                       "Central_onlysig")
  
  fitted_values_hh_conditional = list( fitted_values_hh_model_conditional, 
                                       fitted_opportunity_hh_model_conditional_dist,
                                       fitted_preferences_hh_model_conditional_dist,
                                       fitted_hh_model_conditional_dist,
                                       fitted_opportunity_hh_model_conditional_dist_onlysig)
  names(fitted_values_hh_conditional) = c("Central_fitted", 
                                           "Dist_fitted_opportunity", 
                                           "Dist_fitted_preferences", 
                                           "Dist_fitted_all",
                                          "Central_onlysig")
  
  fitted_values_hh = list(fitted_values_hh_zprob, fitted_values_hh_response, fitted_values_hh_conditional)
  names(fitted_values_hh) = c("zprob", "response", "conditional")
  
  return(fitted_values_hh)
}

