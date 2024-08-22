
# This function performs the household models
# using a mixture-distribution zero-inflated beta regression.
# Both the direct and reduced form models are estimated
# bootstrapping is performed to retrieve both compare these models (via average marginal effects)
# and to retrieve standard errors for the inequality indices. 

get_hh_models = function(hh_data, 
                         Outcome_hh,
                         C_hh, 
                         P_hh,
                         Z_hh,
                         V_hh) {
  
  
  # Load libraries
  library(glmmTMB) #to run zero inflated beta regression.
  library(performance) #model fit measures
  library(marginaleffects) #for average marginal effects of coefficients. 
  
  #######################
  ##### Direct model ####
  #######################
  
  #Equation - mixture distributions. 
  eq_hh_direct_cond =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(P_hh, collapse = " + "), Z_hh, sep = " + "))
  eq_hh_direct_zero = paste(" ~ ", paste(paste(C_hh, collapse = " + "), paste(P_hh, collapse = " + "), Z_hh, sep = " + "), sep = "")
  
  eq_hh_direct_cond = as.formula(eq_hh_direct_cond)
  eq_hh_direct_zero = as.formula(eq_hh_direct_zero)
  
  #Estimate model
  model_hh_direct = glmmTMB(eq_hh_direct_cond, data = hh_data, family = beta_family(link = "logit"), ziformula = eq_hh_direct_zero, dispformula = ~ 1)

  #Summary of model
  model_summary_hh_direct = summary(model_hh_direct)
  
  # Calculate pseudo R-squared Nakagawa R2 (2017)
  log_likelihood = model_summary_hh_direct$logLik[1]
  nakagawa_R_squared = performance::r2(model_hh_direct) # adjusted 
  # Null model for LR test
  eq_null = as.formula("CONTINUOUS_PARTICIPATION ~ 1")
  model_null = glmmTMB(eq_null, data = hh_data, family = beta_family(link = "logit"), ziformula = eq_null, dispformula = ~ 1)
  model_null_summary = summary(model_null)
  LRtestStat = -2*(model_summary_hh_direct$logLik - model_null_summary$logLik)
  
  tests_df = data.frame(
    "log_likelihood" = log_likelihood,
    "Nagakawa_R_squared" = nakagawa_R_squared[1],
    "Nagakawa_R_squared_adj" = nakagawa_R_squared[2],
    "LRteststat" = LRtestStat 
  )
  
  write.csv(tests_df, here("results", "Continuous Household model - direct - tests.csv"))
  
  # Save central results in CSV table
  # With marginal effects included
  # Save one for zero inflated model and one for the conditional model. 
  avg_margins_hh_direct_zprob = avg_slopes(model_hh_direct, type = "zprob",re.form=NA)
  avg_margins_hh_direct_zprob = avg_margins_hh_direct_zprob[match(names(model_summary_hh_direct$coefficients[[1]][,1]), avg_margins_hh_direct_zprob$term),]
  avg_margins_hh_direct_zprob$term[1] = "Intercept"
  results_hh_model_direct_zprob = data.frame("coefs" = model_summary_hh_direct$coefficients[[1]][,1],
                                          "standard_errors_coefs" = model_summary_hh_direct$coefficients[[1]][,2],
                                          "probs_coefs" = model_summary_hh_direct$coefficients[[1]][,4],
                                          "avg_marginal_effects" = avg_margins_hh_direct_zprob$estimate,
                                          "avg_marginal_effects_se" = avg_margins_hh_direct_zprob$std.error,
                                          "avg_marginal_effects_stat" = avg_margins_hh_direct_zprob$statistic,
                                          "avg_marginal_effects_pval" = avg_margins_hh_direct_zprob$p.value)
  
  avg_margins_hh_direct_cond = avg_slopes(model_hh_direct, type = "conditional",re.form=NA)
  avg_margins_hh_direct_cond = avg_margins_hh_direct_cond[match(names(model_summary_hh_direct$coefficients[[1]][,2]), avg_margins_hh_direct_cond$term),]
  avg_margins_hh_direct_cond$term[1] = "Intercept"
  results_hh_model_direct_cond = data.frame("coefs" = model_summary_hh_direct$coefficients[[1]][,1],
                                                "standard_errors_coefs" = model_summary_hh_direct$coefficients[[1]][,2],
                                                "probs_coefs" = model_summary_hh_direct$coefficients[[1]][,4],
                                                "avg_marginal_effects" = avg_margins_hh_direct_cond$estimate,
                                                "avg_marginal_effects_se" = avg_margins_hh_direct_cond$std.error,
                                                "avg_marginal_effects_stat" = avg_margins_hh_direct_cond$statistic,
                                                "avg_marginal_effects_pval" = avg_margins_hh_direct_cond$p.value)
  
  write.csv(results_hh_model_direct_cond, here("results", "Continuous Household model - direct cond - results.csv"))

  #############################
  ##### Reduced form model ####
  #############################
  
  #Equation - mixture distributions. 
  eq_hh_reduced_cond =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(V_hh, collapse = " + "), sep = " + "))
  eq_hh_reduced_zero = paste(" ~ ", paste(paste(C_hh, collapse = " + "), " + " , paste(V_hh, collapse = " + "),  sep = ""))
  
  eq_hh_reduced_cond = as.formula(eq_hh_reduced_cond)
  eq_hh_reduced_zero = as.formula(eq_hh_reduced_zero)
  
  #Estimate model
  model_hh_reduced = glmmTMB(eq_hh_reduced_cond, data = hh_data, family = beta_family(link = "logit"), ziformula = eq_hh_reduced_zero, dispformula = ~ 1)
  
  #Summary of model
  model_summary_hh_reduced = summary(model_hh_reduced)
  
  # Calculate pseudo R-squared Nakagawa R2 (2017)
  log_likelihood = model_summary_hh_reduced$logLik[1]
  nakagawa_R_squared = performance::r2(model_hh_reduced) # adjusted 
  # Null model for LR test
  eq_null = as.formula("CONTINUOUS_PARTICIPATION ~ 1")
  model_null = glmmTMB(eq_null, data = hh_data, family = beta_family(link = "logit"), ziformula = eq_null, dispformula = ~ 1)
  model_null_summary = summary(model_null)
  LRtestStat = -2*(model_summary_hh_reduced$logLik - model_null_summary$logLik)
  
  tests_df = data.frame(
    "log_likelihood" = log_likelihood,
    "Nagakawa_R_squared" = nakagawa_R_squared[1],
    "Nagakawa_R_squared_adj" = nakagawa_R_squared[2],
    "LRteststat" = LRtestStat 
  )
  
  write.csv(tests_df, here("results", "Continuous Household model - reduced - tests.csv"))
  
  # Save central results in CSV table
  # With marginal effects included
  # Save one for zero inflated model and one for the conditional model. 
  avg_margins_hh_reduced_zprob = avg_slopes(model_hh_reduced, type = "zprob",re.form=NA)
  avg_margins_hh_reduced_zprob = avg_margins_hh_reduced_zprob[match(names(model_summary_hh_reduced$coefficients[[1]][,1]), avg_margins_hh_reduced_zprob$term),]
  avg_margins_hh_reduced_zprob$term[1] = "Intercept"
  results_hh_model_reduced_zprob = data.frame("coefs" = model_summary_hh_reduced$coefficients[[1]][,1],
                                             "standard_errors_coefs" = model_summary_hh_reduced$coefficients[[1]][,2],
                                             "probs_coefs" = model_summary_hh_reduced$coefficients[[1]][,4],
                                             "avg_marginal_effects" = avg_margins_hh_reduced_zprob$estimate,
                                             "avg_marginal_effects_se" = avg_margins_hh_reduced_zprob$std.error,
                                             "avg_marginal_effects_stat" = avg_margins_hh_reduced_zprob$statistic,
                                             "avg_marginal_effects_pval" = avg_margins_hh_reduced_zprob$p.value)
  
  avg_margins_hh_reduced_cond = avg_slopes(model_hh_reduced, type = "conditional",re.form=NA)
  avg_margins_hh_reduced_cond = avg_margins_hh_reduced_cond[match(names(model_summary_hh_reduced$coefficients[[1]][,2]), avg_margins_hh_reduced_cond$term),]
  avg_margins_hh_reduced_cond$term[1] = "Intercept"
  results_hh_model_reduced_cond = data.frame("coefs" = model_summary_hh_reduced$coefficients[[1]][,1],
                                            "standard_errors_coefs" = model_summary_hh_reduced$coefficients[[1]][,2],
                                            "probs_coefs" = model_summary_hh_reduced$coefficients[[1]][,4],
                                            "avg_marginal_effects" = avg_margins_hh_reduced_cond$estimate,
                                            "avg_marginal_effects_se" = avg_margins_hh_reduced_cond$std.error,
                                            "avg_marginal_effects_stat" = avg_margins_hh_reduced_cond$statistic,
                                            "avg_marginal_effects_pval" = avg_margins_hh_reduced_cond$p.value)
  
  write.csv(results_hh_model_reduced_cond, here("results", "Continuous Household model - reduced cond - results.csv"))
  

  #################################
  ##### Retrieve fitted values ####
  #################################
  
  # Equalise variation in data for estimating fitted values
  hh_data_cbar = hh_data_pbar =hh_data
  hh_data_cbar[C_hh] = lapply(hh_data_cbar[C_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[P_hh] = lapply(hh_data_pbar[P_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[Z_hh] = lapply(hh_data_pbar[Z_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[V_hh] = lapply(hh_data_pbar[V_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  
  #Direct model central fitted value estimates. 
  #Opportunity fitted values
  pred_opportunity_direct_zprob = predict(model_hh_direct, newdata = hh_data_pbar, type = 'zprob', se.fit = TRUE)
  pred_opportunity_direct_response = predict(model_hh_direct, newdata = hh_data_pbar, type = 'response', se.fit = TRUE)
  pred_opportunity_direct_conditional = predict(model_hh_direct, newdata = hh_data_pbar, type = 'conditional', se.fit = TRUE)
  
  #Preference fitted values
  pred_preferences_direct_zprob = predict(model_hh_direct, newdata = hh_data_cbar, type = 'zprob', se.fit = TRUE)
  pred_preferences_direct_response = predict(model_hh_direct, newdata = hh_data_cbar, type = 'response', se.fit = TRUE)
  pred_preferences_direct_conditional = predict(model_hh_direct, newdata = hh_data_cbar, type = 'conditional', se.fit = TRUE)
  
  #Fitted outcomes (without error term)
  pred_direct_zprob = predict(model_hh_direct, newdata = hh_data, type = 'zprob', se.fit = TRUE)
  pred_direct_response = predict(model_hh_direct, newdata = hh_data, type = 'response', se.fit = TRUE)
  pred_direct_conditional = predict(model_hh_direct, newdata = hh_data, type = 'conditional', se.fit = TRUE)
  
  #Combine into single output
  fitted_values_hh_direct_zprob = cbind(pred_opportunity_direct_zprob$fit , pred_preferences_direct_zprob$fit, pred_direct_zprob$fit)
  colnames(fitted_values_hh_direct_zprob) = c("fitted_opportunity", "fitted_preferences", "fitted")
  
  fitted_values_hh_direct_response = cbind(pred_opportunity_direct_response$fit , pred_preferences_direct_response$fit, pred_direct_response$fit)
  colnames(fitted_values_hh_direct_response ) = c("fitted_opportunity", "fitted_preferences", "fitted")
  
  fitted_values_hh_direct_conditional = cbind(pred_opportunity_direct_conditional$fit , pred_preferences_direct_conditional$fit, pred_direct_conditional$fit)
  colnames(fitted_values_hh_direct_conditional ) = c("fitted_opportunity", "fitted_preferences", "fitted")
  
  
  # Reduced model central fitted value estimates. 
  #Opportunity fitted values
  pred_opportunity_reduced_zprob = predict(model_hh_reduced, newdata = hh_data_pbar, type = 'zprob', se.fit = TRUE)
  pred_opportunity_reduced_response = predict(model_hh_reduced, newdata = hh_data_pbar, type = 'response', se.fit = TRUE)
  pred_opportunity_reduced_conditional = predict(model_hh_reduced, newdata = hh_data_pbar, type = 'conditional', se.fit = TRUE)
  
  #Preference fitted values
  pred_preferences_reduced_zprob = predict(model_hh_reduced, newdata = hh_data_cbar, type = 'zprob', se.fit = TRUE)
  pred_preferences_reduced_response = predict(model_hh_reduced, newdata = hh_data_cbar, type = 'response', se.fit = TRUE)
  pred_preferences_reduced_conditional = predict(model_hh_reduced, newdata = hh_data_cbar, type = 'conditional', se.fit = TRUE)
  
  #Fitted outcomes (without error term)
  pred_reduced_zprob = predict(model_hh_reduced, newdata = hh_data, type = 'zprob', se.fit = TRUE)
  pred_reduced_response = predict(model_hh_reduced, newdata = hh_data, type = 'response', se.fit = TRUE)
  pred_reduced_conditional = predict(model_hh_reduced, newdata = hh_data, type = 'conditional', se.fit = TRUE)
  
  #Combine into single output
  fitted_values_hh_reduced_zprob = cbind(pred_opportunity_reduced_zprob$fit , pred_preferences_reduced_zprob$fit, pred_reduced_zprob$fit)
  colnames(fitted_values_hh_reduced_zprob) = c("fitted_opportunity", "fitted_preferences", "fitted")
  
  fitted_values_hh_reduced_response = cbind(pred_opportunity_reduced_response$fit , pred_preferences_reduced_response$fit, pred_reduced_response$fit)
  colnames(fitted_values_hh_reduced_response ) = c("fitted_opportunity", "fitted_preferences", "fitted")
  
  fitted_values_hh_reduced_conditional = cbind(pred_opportunity_reduced_conditional$fit , pred_preferences_reduced_conditional$fit, pred_reduced_conditional$fit)
  colnames(fitted_values_hh_reduced_conditional ) = c("fitted_opportunity", "fitted_preferences", "fitted")
  
  ################################
  ##### Bootstrap both models ####
  ################################
  
  # Bootstrapping is used to retrieve the correlation between models for the average marginal effects
  # and for the standard errors for the inequality indices. 
  
  #Establish bootstrap parameters and dataframes
  b = 500 #number of bootstrapped samples
  min_outcomes = 15 # to ensure model fit. 
  
  # Direct model empty dataframes
  fitted_opportunity_hh_direct_zprob_dist = matrix(NA, nrow = nrow(fitted_values_hh_direct_zprob), ncol = b)
  fitted_opportunity_hh_direct_zprob_dist_onlysig = matrix(NA, nrow = nrow(fitted_values_hh_direct_zprob), ncol = b)
  fitted_preferences_hh_direct_zprob_dist = matrix(NA, nrow = nrow(fitted_values_hh_direct_zprob), ncol = b)
  fitted_hh_direct_zprob_dist = matrix(NA, nrow = nrow(fitted_values_hh_direct_zprob), ncol = b)
  
  fitted_opportunity_hh_direct_response_dist = matrix(NA, nrow = nrow(fitted_values_hh_direct_response), ncol = b)
  fitted_opportunity_hh_direct_response_dist_onlysig = matrix(NA, nrow = nrow(fitted_values_hh_direct_response), ncol = b)
  fitted_preferences_hh_direct_response_dist = matrix(NA, nrow = nrow(fitted_values_hh_direct_response), ncol = b)
  fitted_hh_direct_response_dist = matrix(NA, nrow = nrow(fitted_values_hh_direct_response), ncol = b)
  
  fitted_opportunity_hh_direct_conditional_dist = matrix(NA, nrow = nrow(fitted_values_hh_direct_conditional), ncol = b)
  fitted_opportunity_hh_direct_conditional_dist_onlysig = matrix(NA, nrow = nrow(fitted_values_hh_direct_conditional), ncol = b)
  fitted_preferences_hh_direct_conditional_dist = matrix(NA, nrow = nrow(fitted_values_hh_direct_conditional), ncol = b)
  fitted_hh_direct_conditional_dist = matrix(NA, nrow = nrow(fitted_values_hh_direct_conditional), ncol = b)
  
  avg_margins_hh_direct_zprob_dist = as.data.frame(matrix(NA, nrow = nrow(avg_margins_hh_direct_zprob), ncol = b))
  rownames(avg_margins_hh_direct_zprob_dist) = avg_margins_hh_direct_zprob$term
  avg_margins_hh_direct_cond_dist = as.data.frame(matrix(NA, nrow = nrow(avg_margins_hh_direct_cond), ncol = b))
  rownames(avg_margins_hh_direct_cond_dist) = avg_margins_hh_direct_cond$term
  
  #Reduced form model empty dataframes
  fitted_opportunity_hh_reduced_zprob_dist = matrix(NA, nrow = nrow(fitted_values_hh_reduced_zprob), ncol = b)
  fitted_opportunity_hh_reduced_zprob_dist_onlysig = matrix(NA, nrow = nrow(fitted_values_hh_reduced_zprob), ncol = b)
  fitted_preferences_hh_reduced_zprob_dist = matrix(NA, nrow = nrow(fitted_values_hh_reduced_zprob), ncol = b)
  fitted_hh_reduced_zprob_dist = matrix(NA, nrow = nrow(fitted_values_hh_reduced_zprob), ncol = b)
  
  fitted_opportunity_hh_reduced_response_dist = matrix(NA, nrow = nrow(fitted_values_hh_reduced_response), ncol = b)
  fitted_opportunity_hh_reduced_response_dist_onlysig = matrix(NA, nrow = nrow(fitted_values_hh_reduced_response), ncol = b)
  fitted_preferences_hh_reduced_response_dist = matrix(NA, nrow = nrow(fitted_values_hh_reduced_response), ncol = b)
  fitted_hh_reduced_response_dist = matrix(NA, nrow = nrow(fitted_values_hh_reduced_response), ncol = b)
  
  fitted_opportunity_hh_reduced_conditional_dist = matrix(NA, nrow = nrow(fitted_values_hh_reduced_conditional), ncol = b)
  fitted_opportunity_hh_reduced_conditional_dist_onlysig = matrix(NA, nrow = nrow(fitted_values_hh_reduced_conditional), ncol = b)
  fitted_preferences_hh_reduced_conditional_dist = matrix(NA, nrow = nrow(fitted_values_hh_reduced_conditional), ncol = b)
  fitted_hh_reduced_conditional_dist = matrix(NA, nrow = nrow(fitted_values_hh_reduced_conditional), ncol = b)
  
  avg_margins_hh_reduced_zprob_dist = as.data.frame(matrix(NA, nrow = nrow(avg_margins_hh_reduced_zprob), ncol = b))
  rownames(avg_margins_hh_reduced_zprob_dist) = avg_margins_hh_reduced_zprob$term
  avg_margins_hh_reduced_cond_dist = as.data.frame(matrix(NA, nrow = nrow(avg_margins_hh_reduced_cond), ncol = b))
  rownames(avg_margins_hh_reduced_cond_dist) = avg_margins_hh_reduced_cond$term
  
  # Run boostrap. 
  for (bb in 1:b){
    
    print((bb/b)*100)
    
    # Bootstrap sample and check min outcomes required, if not resample
    min_outcomes_condition  = FALSE
    while (min_outcomes_condition == FALSE){
      bootstrap_indices = sample(1:nrow(hh_data), size = nrow(hh_data), replace = TRUE)
      bootstrap_sample = hh_data[bootstrap_indices, ]
      
      if (length(bootstrap_sample$CONTINUOUS_PARTICIPATION[bootstrap_sample$CONTINUOUS_PARTICIPATION > 0]) > min_outcomes) {
        min_outcomes_condition = TRUE # If condition is met, set the flag to True to exit the loop
      }
    }
    
    #re-estimate model
    model_hh_direct_boot = glmmTMB(eq_hh_direct_cond, data = bootstrap_sample, family = beta_family(link = "logit"), ziformula = eq_hh_direct_zero, dispformula = ~ 1)
    model_hh_reduced_boot = glmmTMB(eq_hh_reduced_cond, data = bootstrap_sample, family = beta_family(link = "logit"), ziformula = eq_hh_reduced_zero, dispformula = ~ 1)
    
    #Check if both models converged (i.e no NA values in results). If no, add central scenario for both 
    if (any(is.na(summary(model_hh_direct_boot)$coefficients$cond[,2])) | any(is.na(summary(model_hh_reduced_boot)$coefficients$cond[,2]))) {
      fitted_opportunity_hh_direct_zprob_dist[,bb] = pred_opportunity_direct_zprob$fit 
      fitted_opportunity_hh_direct_response_dist[,bb] = pred_opportunity_direct_response$fit 
      fitted_opportunity_hh_direct_conditional_dist[,bb] = pred_opportunity_direct_conditional$fit 
      fitted_preferences_hh_direct_zprob_dist[,bb] = pred_preferences_direct_zprob$fit 
      fitted_preferences_hh_direct_response_dist[,bb] = pred_preferences_direct_response$fit 
      fitted_preferences_hh_direct_conditional_dist[,bb] = pred_preferences_direct_conditional$fit 
      fitted_hh_direct_zprob_dist[,bb] = pred_direct_zprob$fit 
      fitted_hh_direct_response_dist[,bb] = pred_direct_response$fit 
      fitted_hh_direct_conditional_dist[,bb] = pred_direct_conditional$fit 
    
      fitted_opportunity_hh_reduced_zprob_dist[,bb] = pred_opportunity_reduced_zprob$fit 
      fitted_opportunity_hh_reduced_response_dist[,bb] = pred_opportunity_reduced_response$fit 
      fitted_opportunity_hh_reduced_conditional_dist[,bb] = pred_opportunity_reduced_conditional$fit 
      fitted_preferences_hh_reduced_zprob_dist[,bb] = pred_preferences_reduced_zprob$fit 
      fitted_preferences_hh_reduced_response_dist[,bb] = pred_preferences_reduced_response$fit 
      fitted_preferences_hh_reduced_conditional_dist[,bb] = pred_preferences_reduced_conditional$fit 
      fitted_hh_reduced_zprob_dist[,bb] = pred_reduced_zprob$fit 
      fitted_hh_reduced_response_dist[,bb] = pred_reduced_response$fit 
      fitted_hh_reduced_conditional_dist[,bb] = pred_reduced_conditional$fit 
      
      #Retrieve average marginal effects from the central models
      avg_margins_hh_direct_zprob_dist[,bb] = avg_margins_hh_direct_zprob$estimate
      avg_margins_hh_direct_cond_dist[,bb] = avg_margins_hh_direct_cond$estimate
      avg_margins_hh_reduced_zprob_dist[,bb] = avg_margins_hh_reduced_zprob$estimate
      avg_margins_hh_reduced_cond_dist[,bb] = avg_margins_hh_reduced_cond$estimate
      
      # Finally retrieve central estimate only for the significant coefficients. 
      counter = 1
      for (coef in 1:length(summary(model_hh_direct)$coefficients$cond[,1])){
        if (summary(model_hh_direct)$coefficients$cond[coef,4] >0.1){
          model_hh_direct$fit$par[counter] = 0
          model_hh_direct$sdr$par.fixed[counter] = 0
        }
        counter = counter + 1
      }
      for (coef in 1:length(summary(model_hh_direct)$coefficients$zi[,1])){
        if (summary(model_hh_direct)$coefficients$cond[coef,4] >0.1){
          model_hh_direct$fit$par[counter] = 0
          model_hh_direct$sdr$par.fixed[counter] = 0
        }
        counter = counter + 1
      }   
      
      fitted_opportunity_hh_direct_zprob_dist_onlysig[,bb] = predict(model_hh_direct, newdata = hh_data_pbar, type = 'zprob')
      fitted_opportunity_hh_direct_response_dist_onlysig[,bb] = predict(model_hh_direct, newdata = hh_data_pbar, type = 'response')
      fitted_opportunity_hh_direct_conditional_dist_onlysig[,bb] = predict(model_hh_direct, newdata = hh_data_pbar, type = 'conditional')
      
      counter = 1
      for (coef in 1:length(summary(model_hh_reduced)$coefficients$cond[,1])){
        if (summary(model_hh_reduced)$coefficients$cond[coef,4] >0.1){
          model_hh_reduced$fit$par[counter] = 0
          model_hh_reduced$sdr$par.fixed[counter] = 0
        }
        counter = counter + 1
      }
      for (coef in 1:length(summary(model_hh_reduced)$coefficients$zi[,1])){
        if (summary(model_hh_reduced)$coefficients$cond[coef,4] >0.1){
          model_hh_reduced$fit$par[counter] = 0
          model_hh_reduced$sdr$par.fixed[counter] = 0
        }
        counter = counter + 1
      }   
      
      fitted_opportunity_hh_reduced_zprob_dist_onlysig[,bb] = predict(model_hh_reduced, newdata = hh_data_pbar, type = 'zprob')
      fitted_opportunity_hh_reduced_response_dist_onlysig[,bb] = predict(model_hh_reduced, newdata = hh_data_pbar, type = 'response')
      fitted_opportunity_hh_reduced_conditional_dist_onlysig[,bb] = predict(model_hh_reduced, newdata = hh_data_pbar, type = 'conditional')
      
    } else {
        
      #Else, Fitted values - use original sample but results from bootstrap
      fitted_opportunity_hh_direct_zprob_dist[,bb] = predict(model_hh_direct_boot, newdata = hh_data_pbar, type = 'zprob')
      fitted_opportunity_hh_direct_response_dist[,bb] = predict(model_hh_direct_boot, newdata = hh_data_pbar, type = 'response')
      fitted_opportunity_hh_direct_conditional_dist[,bb] = predict(model_hh_direct_boot, newdata = hh_data_pbar, type = 'conditional')
      fitted_preferences_hh_direct_zprob_dist[,bb] = predict(model_hh_direct_boot, newdata = hh_data_cbar, type = 'zprob')
      fitted_preferences_hh_direct_response_dist[,bb] = predict(model_hh_direct_boot, newdata = hh_data_cbar, type = 'response')
      fitted_preferences_hh_direct_conditional_dist[,bb] = predict(model_hh_direct_boot, newdata = hh_data_cbar, type = 'conditional')
      fitted_hh_direct_zprob_dist[,bb] = predict(model_hh_direct_boot, newdata = hh_data, type = 'zprob')
      fitted_hh_direct_response_dist[,bb] = predict(model_hh_direct_boot, newdata = hh_data, type = 'response')
      fitted_hh_direct_conditional_dist[,bb] = predict(model_hh_direct_boot, newdata = hh_data, type = 'conditional')
      
      fitted_opportunity_hh_reduced_zprob_dist[,bb] = predict(model_hh_reduced_boot, newdata = hh_data_pbar, type = 'zprob')
      fitted_opportunity_hh_reduced_response_dist[,bb] = predict(model_hh_reduced_boot, newdata = hh_data_pbar, type = 'response')
      fitted_opportunity_hh_reduced_conditional_dist[,bb] = predict(model_hh_reduced_boot, newdata = hh_data_pbar, type = 'conditional')
      fitted_preferences_hh_reduced_zprob_dist[,bb] = predict(model_hh_reduced_boot, newdata = hh_data_cbar, type = 'zprob')
      fitted_preferences_hh_reduced_response_dist[,bb] = predict(model_hh_reduced_boot, newdata = hh_data_cbar, type = 'response')
      fitted_preferences_hh_reduced_conditional_dist[,bb] = predict(model_hh_reduced_boot, newdata = hh_data_cbar, type = 'conditional')
      fitted_hh_reduced_zprob_dist[,bb] = predict(model_hh_reduced_boot, newdata = hh_data, type = 'zprob')
      fitted_hh_reduced_response_dist[,bb] = predict(model_hh_reduced_boot, newdata = hh_data, type = 'response')
      fitted_hh_reduced_conditional_dist[,bb] = predict(model_hh_reduced_boot, newdata = hh_data, type = 'conditional')
      
      #Retrieve average marginal effects from the models. 
      avg_margins_hh_direct_zprob_boot = avg_slopes(model_hh_direct_boot,type = "zprob", re.form=NA)
      avg_margins_hh_direct_zprob_boot= avg_margins_hh_direct_zprob_boot[match(rownames(avg_margins_hh_direct_zprob_dist), avg_margins_hh_direct_zprob_boot$term),]
      avg_margins_hh_direct_zprob_dist[,bb] =  avg_margins_hh_direct_zprob_boot$estimate
      
      avg_margins_hh_direct_cond_boot = avg_slopes(model_hh_direct_boot,type = "conditional", re.form=NA)
      avg_margins_hh_direct_cond_boot= avg_margins_hh_direct_cond_boot[match(rownames(avg_margins_hh_direct_cond_dist), avg_margins_hh_direct_cond_boot$term),]
      avg_margins_hh_direct_cond_dist[,bb] =  avg_margins_hh_direct_cond_boot$estimate
      
      avg_margins_hh_reduced_zprob_boot = avg_slopes(model_hh_reduced_boot,type = "zprob", re.form=NA)
      avg_margins_hh_reduced_zprob_boot= avg_margins_hh_reduced_zprob_boot[match(rownames(avg_margins_hh_reduced_zprob_dist), avg_margins_hh_reduced_zprob_boot$term),]
      avg_margins_hh_reduced_zprob_dist[,bb] = avg_margins_hh_reduced_zprob_boot$estimate
    
      avg_margins_hh_reduced_cond_boot = avg_slopes(model_hh_reduced_boot,type = "conditional", re.form=NA)
      avg_margins_hh_reduced_cond_boot= avg_margins_hh_reduced_cond_boot[match(rownames(avg_margins_hh_reduced_cond_dist), avg_margins_hh_reduced_cond_boot$term),]
      avg_margins_hh_reduced_cond_dist[,bb] =  avg_margins_hh_reduced_cond_boot$estimate
      
      # Finally retrieve central estimate only for the significant coefficients. 
      counter = 1
      for (coef in 1:length(summary(model_hh_direct_boot)$coefficients$cond[,1])){
        if (summary(model_hh_direct_boot)$coefficients$cond[coef,4] >0.1){
          model_hh_direct_boot$fit$par[counter] = 0
          model_hh_direct_boot$sdr$par.fixed[counter] = 0
        }
        counter = counter + 1
      }
      for (coef in 1:length(summary(model_hh_direct_boot)$coefficients$zi[,1])){
        if (summary(model_hh_direct_boot)$coefficients$cond[coef,4] >0.1){
          model_hh_direct_boot$fit$par[counter] = 0
          model_hh_direct_boot$sdr$par.fixed[counter] = 0
        }
        counter = counter + 1
      }   
      
      fitted_opportunity_hh_direct_zprob_dist_onlysig[,bb] = predict(model_hh_direct_boot, newdata = hh_data_pbar, type = 'zprob')
      fitted_opportunity_hh_direct_response_dist_onlysig[,bb] = predict(model_hh_direct_boot, newdata = hh_data_pbar, type = 'response')
      fitted_opportunity_hh_direct_conditional_dist_onlysig[,bb] = predict(model_hh_direct_boot, newdata = hh_data_pbar, type = 'conditional')
      
      counter = 1
      for (coef in 1:length(summary(model_hh_reduced_boot)$coefficients$cond[,1])){
        if (summary(model_hh_reduced_boot)$coefficients$cond[coef,4] >0.1){
          model_hh_reduced_boot$fit$par[counter] = 0
          model_hh_reduced_boot$sdr$par.fixed[counter] = 0
        }
        counter = counter + 1
      }
      for (coef in 1:length(summary(model_hh_reduced_boot)$coefficients$zi[,1])){
        if (summary(model_hh_reduced_boot)$coefficients$cond[coef,4] >0.1){
          model_hh_reduced_boot$fit$par[counter] = 0
          model_hh_reduced_boot$sdr$par.fixed[counter] = 0
        }
        counter = counter + 1
      }   
      
      fitted_opportunity_hh_reduced_zprob_dist_onlysig[,bb] = predict(model_hh_reduced_boot, newdata = hh_data_pbar, type = 'zprob')
      fitted_opportunity_hh_reduced_response_dist_onlysig[,bb] = predict(model_hh_reduced_boot, newdata = hh_data_pbar, type = 'response')
      fitted_opportunity_hh_reduced_conditional_dist_onlysig[,bb] = predict(model_hh_reduced_boot, newdata = hh_data_pbar, type = 'conditional')
    }
    
  }
  
  ##################################
  ##### Test differences in AME ####
  ##################################
  
  # Here we test the differences between average marginal effects from the bootstrap
  # Bootstrap assumes asymptotic normal distribution. 
  # We also add in the bootstrapped standard errors for the AME 
  # And save the central scenario results as a .csv file. 
  # Function to perform AME comparison tests
  perform_ame_test = function(avg_margins_direct, avg_margins_reduced, avg_margins_direct_dist, avg_margins_reduced_dist, output_file) {
    AME_tests = data.frame(
      "variable" = avg_margins_direct$term,
      "stat" = rep(NA, nrow(avg_margins_direct)),
      "pval" = rep(NA, nrow(avg_margins_direct))
    )
    
    for (r in 1:nrow(AME_tests)) {
      var = AME_tests$variable[r]
      row_idx_direct = which(rownames(avg_margins_direct_dist) == var)
      row_idx_reduced = which(rownames(avg_margins_reduced_dist) == var)
      
      if (length(row_idx_direct) == 1 & length(row_idx_reduced) == 1) { # Variable in both models
        ame_direct = as.numeric(avg_margins_direct$estimate[row_idx_direct])
        ame_reduced = as.numeric(avg_margins_reduced$estimate[row_idx_reduced])
        se_direct = sd(avg_margins_direct_dist[row_idx_direct,], na.rm = TRUE)
        se_reduced = sd(avg_margins_reduced_dist[row_idx_reduced,], na.rm = TRUE)
        cov_ame = cov(
          as.numeric(avg_margins_direct_dist[row_idx_direct,]), 
          as.numeric(avg_margins_reduced_dist[row_idx_reduced,]), 
          method = 'pearson',
        )
        z_value = (ame_direct - ame_reduced) / sqrt(se_direct^2 + se_reduced^2 - 2 * cov_ame)
        AME_tests$stat[r] = z_value
        AME_tests$pval[r] = 2 * pnorm(z_value, lower.tail = FALSE)
      }
    }
    
    write.csv(AME_tests, here("results", output_file))
  }
  
  # Zero Inflated Model
  perform_ame_test(
    avg_margins_hh_direct_zprob,
    avg_margins_hh_reduced_zprob,
    avg_margins_hh_direct_zprob_dist,
    avg_margins_hh_reduced_zprob_dist,
    "Household zero inflated AME comparison tests.csv"
  )
  
  # Conditional Model
  perform_ame_test(
    avg_margins_hh_direct_cond,
    avg_margins_hh_reduced_cond,
    avg_margins_hh_direct_cond_dist,
    avg_margins_hh_reduced_cond_dist,
    "Household conditional AME comparison tests.csv"
  )
  
  # Update results table with bootstrap standard errors and statistics for both models
  update_results_table = function(avg_margins_dist, results_model) {
    for (r in 1:nrow(results_model)) {
      var = rownames(results_model)[r]
      row_idx_dist = which(rownames(avg_margins_dist) == var)
      
      if (length(row_idx_dist) == 1) { # Variable present in the distribution
        ame = as.numeric(results_model$avg_marginal_effects[r])
        se = sd(avg_margins_dist[row_idx_dist,], na.rm = TRUE)
        
        results_model$avg_marginal_effects_se[r] = se
        results_model$avg_marginal_effects_stat[r] = ame / se
        results_model$avg_marginal_effects_pval[r] = 2 * (1 - pnorm(abs(results_model$avg_marginal_effects_stat[r])))
      }
    }
    return(results_model)
  }
  
  results_hh_model_direct_zprob = update_results_table(avg_margins_hh_direct_zprob_dist, results_hh_model_direct_zprob)
  write.csv(results_hh_model_direct_zprob, here("results", "Continuous Household model - direct zprob - results.csv"))
  results_hh_model_reduced_zprob = update_results_table(avg_margins_hh_reduced_zprob_dist, results_hh_model_reduced_zprob)
  write.csv(results_hh_model_reduced_zprob, here("results", "Continuous Household model - reduced zprob - results.csv"))

  #######################
  ##### Save results ####
  #######################
  
  #Direct model results
  fitted_values_hh_direct_zprob = list(fitted_values_hh_direct_zprob, 
                                       fitted_opportunity_hh_direct_zprob_dist,
                                       fitted_preferences_hh_direct_zprob_dist,
                                       fitted_hh_direct_zprob_dist,
                                       fitted_opportunity_hh_direct_zprob_dist_onlysig)
  names(fitted_values_hh_direct_zprob) = c("Central_fitted", 
                                           "Dist_fitted_opportunity", 
                                           "Dist_fitted_preferences", 
                                           "Dist_fitted_all", 
                                           "Central_onlysig")
  
  fitted_values_hh_direct_response = list(fitted_values_hh_direct_response, 
                                          fitted_opportunity_hh_direct_response_dist,
                                          fitted_preferences_hh_direct_response_dist,
                                          fitted_hh_direct_response_dist,
                                          fitted_opportunity_hh_direct_response_dist_onlysig)
  names(fitted_values_hh_direct_response) = c("Central_fitted", 
                                              "Dist_fitted_opportunity", 
                                              "Dist_fitted_preferences", 
                                              "Dist_fitted_all",
                                              "Central_onlysig")
  
  fitted_values_hh_direct_conditional = list( fitted_values_hh_direct_conditional, 
                                             fitted_opportunity_hh_direct_conditional_dist,
                                             fitted_preferences_hh_direct_conditional_dist,
                                             fitted_hh_direct_conditional_dist,
                                             fitted_opportunity_hh_direct_conditional_dist_onlysig)
  names(fitted_values_hh_direct_conditional) = c("Central_fitted", 
                                                 "Dist_fitted_opportunity", 
                                                 "Dist_fitted_preferences", 
                                                 "Dist_fitted_all",
                                                 "Central_onlysig")
  
  fitted_values_direct = list(fitted_values_hh_direct_zprob, fitted_values_hh_direct_response, fitted_values_hh_direct_conditional)
  names(fitted_values_direct) = c("zprob", "response", "conditional")
  
  #reduced model results
  fitted_values_hh_reduced_zprob = list(fitted_values_hh_reduced_zprob, 
                                       fitted_opportunity_hh_reduced_zprob_dist,
                                       fitted_preferences_hh_reduced_zprob_dist,
                                       fitted_hh_reduced_zprob_dist,
                                       fitted_opportunity_hh_reduced_zprob_dist_onlysig)
  names(fitted_values_hh_reduced_zprob) = c("Central_fitted", 
                                            "Dist_fitted_opportunity", 
                                            "Dist_fitted_preferences", 
                                            "Dist_fitted_all", 
                                            "Central_onlysig")
  
  fitted_values_hh_reduced_response = list(fitted_values_hh_reduced_response, 
                                          fitted_opportunity_hh_reduced_response_dist,
                                          fitted_preferences_hh_reduced_response_dist,
                                          fitted_hh_reduced_response_dist,
                                          fitted_opportunity_hh_reduced_response_dist_onlysig)
  names(fitted_values_hh_reduced_response) = c("Central_fitted", 
                                               "Dist_fitted_opportunity", 
                                              "Dist_fitted_preferences", 
                                               "Dist_fitted_all",
                                               "Central_onlysig")
  
  fitted_values_hh_reduced_conditional = list( fitted_values_hh_reduced_conditional, 
                                              fitted_opportunity_hh_reduced_conditional_dist,
                                              fitted_preferences_hh_reduced_conditional_dist,
                                              fitted_hh_reduced_conditional_dist,
                                              fitted_opportunity_hh_reduced_conditional_dist_onlysig)
  names(fitted_values_hh_reduced_conditional) = c("Central_fitted", 
                                                  "Dist_fitted_opportunity", 
                                                  "Dist_fitted_preferences", 
                                                  "Dist_fitted_all",
                                                  "Central_onlysig")
  
  fitted_values_reduced = list(fitted_values_hh_reduced_zprob, fitted_values_hh_reduced_response, fitted_values_hh_reduced_conditional)
  names(fitted_values_reduced) = c("zprob", "response", "conditional")
  
  #combine and save
  fitted_values_hh = list(fitted_values_direct, fitted_values_reduced)
  names(fitted_values_hh) = c("direct", "reduced")
  
  return(fitted_values_hh)
}

