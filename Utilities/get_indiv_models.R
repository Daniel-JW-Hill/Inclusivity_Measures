
# This function performs simple logit models for the individual model
# Both the direct and reduced form models are estimated
# bootstrapping is performed to retrieve both compare these models (via Average marginal effects)
# and to retrieve standard errors for the inequality indices. 

get_indiv_models = function(indiv_data,
                            Outcome_indiv,
                            C_indiv,
                            P_indiv,
                            Z_indiv, 
                            V_indiv) {
  
  #######################
  ##### Direct model ####
  #######################
  
  library(logistf) #Firth's bias reduction method for logistic regression
  library(marginaleffects) #for average marginal effects of coefficients. 
  
  #Equation
  eq_indiv_direct =  paste(Outcome_indiv, sep = " ~ ", paste(paste(C_indiv, collapse = " + "), paste(P_indiv, collapse = " + "), Z_indiv, sep = " + "))
  
  #Estimate model
  model_indiv_direct = logistf(eq_indiv_direct, data = indiv_data)
  
  #Summary of model
  model_summary_indiv_direct = summary(model_indiv_direct)
  
  #Perform tests on model and save table
  log_likelihood = model_summary_indiv_direct$loglik
  McFaddenR2  =  1 - (model_summary_indiv_direct$loglik[1]/model_summary_indiv_direct$loglik[2])
  McFaddenR2_adj = 1 - ((model_summary_indiv_direct$loglik[1] - length(model_summary_indiv_direct$terms))/model_summary_indiv_direct$loglik[2])
  predicted_probs = model_indiv_direct$predict
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
  write.csv(tests_df, here("results", "Binary individual model - direct - tests.csv"))
  
  # Save central results in CSV table
  #Add intercept into marginal effects first
  avg_margins_indiv_direct = avg_slopes(model_indiv_direct)
  avg_margins_indiv_direct = avg_margins_indiv_direct[match(names(model_indiv_direct$coefficients), avg_margins_indiv_direct$term),]
  avg_margins_indiv_direct$term[1] = "Intercept"
  results_indiv_model_direct = data.frame("coefs" = model_indiv_direct$coefficients,
                                     "odds" = exp(model_indiv_direct$coefficients),
                                     "standard_errors_coefs" = sqrt(diag(vcov(model_indiv_direct))),
                                     "probs_coefs" = model_indiv_direct$prob,
                                     "avg_marginal_effects" = avg_margins_indiv_direct$estimate,
                                     "avg_marginal_effects_se" = avg_margins_indiv_direct$std.error,
                                     "avg_marginal_effects_stat" = avg_margins_indiv_direct$statistic,
                                     "avg_marginal_effects_pval" = avg_margins_indiv_direct$p.value)
  
  #############################
  ##### Reduced form model ####
  #############################
  
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
  
  # Save central results in CSV table
  # Add intercept into marginal effects first
  avg_margins_indiv_reduced = avg_slopes(model_indiv_reduced)
  avg_margins_indiv_reduced = avg_margins_indiv_reduced[match(names(model_indiv_reduced$coefficients), avg_margins_indiv_reduced$term),]
  avg_margins_indiv_reduced$term[1] = "Intercept"
  results_indiv_model_reduced = data.frame("coefs" = model_indiv_reduced$coefficients,
                                          "odds" = exp(model_indiv_reduced$coefficients),
                                          "standard_errors_coefs" = sqrt(diag(vcov(model_indiv_reduced))),
                                          "probs_coefs" = model_indiv_reduced$prob,
                                          "avg_marginal_effects" = avg_margins_indiv_reduced$estimate,
                                          "avg_marginal_effects_se" = avg_margins_indiv_reduced$std.error,
                                          "avg_marginal_effects_stat" = avg_margins_indiv_reduced$statistic,
                                          "avg_marginal_effects_pval" = avg_margins_indiv_reduced$p.value)

  #################################
  ##### Retrieve fitted values ####
  #################################
  
  # Equalise variation in data for estimating fitted values
  indiv_data_cbar = indiv_data_pbar = indiv_data
  indiv_data_cbar[C_indiv] = lapply(indiv_data_cbar[C_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  indiv_data_pbar[P_indiv] = lapply(indiv_data_pbar[P_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  indiv_data_pbar[Z_indiv] = lapply(indiv_data_pbar[Z_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  indiv_data_pbar[V_indiv] = lapply(indiv_data_pbar[V_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  
  #Direct model central fitted value estimates. 
  #Opportunity fitted values
  predicted_probs_opportunity_direct =  predict(model_indiv_direct, newdata = indiv_data_pbar, type = 'response' , se.fit = TRUE)
  #Preference fitted values
  predicted_probs_preferences_direct  = predict(model_indiv_direct, newdata = indiv_data_cbar, type = 'response' , se.fit = TRUE)
  #Fitted outcomes
  predicted_probs_total_direct  = predict(model_indiv_direct, data = indiv_data, type = 'response', se.fit = TRUE)
  #Combine into single output
  fitted_values_indiv_direct = cbind(predicted_probs_opportunity_direct$fit, 
                                     predicted_probs_preferences_direct$fit, 
                                     predicted_probs_total_direct$fit)
  colnames(fitted_values_indiv_direct) = c("fitted_opportunity", 
                                          "fitted_preferences", 
                                          "fitted")
  
  #Reduced form model central fitted value estimates. 
  #Opportunity fitted values
  predicted_probs_opportunity_reduced =  predict(model_indiv_reduced, newdata = indiv_data_pbar, type = 'response' , se.fit = TRUE)
  #Preference fitted values
  predicted_probs_preferences_reduced  = predict(model_indiv_reduced, newdata = indiv_data_cbar, type = 'response' , se.fit = TRUE)
  #Fitted outcomes
  predicted_probs_total_reduced  = predict(model_indiv_reduced, data = indiv_data, type = 'response', se.fit = TRUE)
  #Combine into single output
  fitted_values_indiv_reduced= cbind(predicted_probs_opportunity_reduced$fit, 
                                          predicted_probs_preferences_reduced$fit, 
                                          predicted_probs_total_reduced$fit)
  colnames(fitted_values_indiv_reduced) = c("fitted_opportunity", 
                                           "fitted_preferences", 
                                           "fitted")
  
  ################################
  ##### Bootstrap both models ####
  ################################
  
  # Bootstrapping is used to retrieve the correlation between models for the average marginal effects
  # and for the standard errors for the inequality indices. 
  
  #Establish bootstrap parameters and dataframes
  b = 500 #number of bootstrapped samples
  min_outcomes = 15 # to ensure model fit. 
  
  #Empty dataframes
  fitted_opportunity_indiv_direct_dist = matrix(NA, nrow = nrow(fitted_values_indiv_direct), ncol = b)
  fitted_opportunity_indiv_direct_dist_onlysig = matrix(NA, nrow = nrow(fitted_values_indiv_direct), ncol = b)
  fitted_preference_indiv_direct_dist = matrix(NA, nrow = nrow(fitted_values_indiv_direct), ncol = b)
  fitted_indiv_direct_dist = matrix(NA, nrow = nrow(fitted_values_indiv_direct), ncol = b)
  avg_margins_indiv_direct_dist = as.data.frame(matrix(NA, nrow = nrow(avg_margins_indiv_direct), ncol = b))
  rownames(avg_margins_indiv_direct_dist) = avg_margins_indiv_direct$term
 
  fitted_opportunity_indiv_reduced_dist = matrix(NA, nrow = nrow(fitted_values_indiv_reduced), ncol = b)
  fitted_opportunity_indiv_reduced_dist_onlysig = matrix(NA, nrow = nrow(fitted_values_indiv_reduced), ncol = b)
  fitted_preference_indiv_reduced_dist = matrix(NA, nrow = nrow(fitted_values_indiv_reduced), ncol = b)
  fitted_indiv_reduced_dist = matrix(NA, nrow = nrow(fitted_values_indiv_reduced), ncol = b)
  avg_margins_indiv_reduced_dist = as.data.frame(matrix(NA, nrow = nrow(avg_margins_indiv_reduced), ncol = b))
  rownames(avg_margins_indiv_reduced_dist) = avg_margins_indiv_reduced$term
  
  # Run bootstrap. 
  for (bb in 1:b){
    print((bb/b)*100)
    # Bootstrap sample and check min outcomes required, if not resample
    min_outcomes_condition  = FALSE
    while (min_outcomes_condition == FALSE){
      bootstrap_indices = sample(1:nrow(indiv_data), size = nrow(indiv_data), replace = TRUE)
      bootstrap_sample = indiv_data[bootstrap_indices, ]
      
      if (length(bootstrap_sample$BINARY_PARTICIPATION[bootstrap_sample$BINARY_PARTICIPATION > 0]) > min_outcomes) {
        min_outcomes_condition = TRUE # If condition is met, set the flag to True to exit the loop
      }
    
    }
    
    #re-estimate models with resampled boostrap
    model_indiv_direct_boot =  logistf(eq_indiv_direct, data = bootstrap_sample)
    model_indiv_reduced_boot =  logistf(eq_indiv_reduced, data = bootstrap_sample)
    
    #Check if both models converged (i.e converged before max iterations). If no, add central scenario for both 
    if (model_indiv_direct_boot$iter[1] ==0 | model_indiv_reduced_boot$iter[1] ==0){
      fitted_opportunity_indiv_direct_dist[,bb] = predicted_probs_opportunity_direct$fit 
      fitted_preferences_indiv_direct_dist[,bb] = predicted_probs_preferences_direct$fit 
      fitted_indiv_direct_dist[,bb] = predicted_probs_total_direct$fit 
      
      fitted_opportunity_indiv_reduced_dist[,bb] = predicted_probs_opportunity_reduced$fit 
      fitted_preferences_indiv_reduced_dist[,bb] = predicted_probs_preferences_reduced$fit 
      fitted_indiv_reduced_dist[,bb] = predicted_probs_total_reduced$fit 
      
      # Save average marginal effects for both models (adding in intercept as NA)
      avg_margins_indiv_direct_boot = avg_slopes(model_indiv_direct)
      avg_margins_indiv_direct_dist[,bb] = c(NA, avg_margins_indiv_direct$estimate)
      avg_margins_indiv_reduced_boot = avg_slopes(model_indiv_reduced)
      avg_margins_indiv_reduced_dist[,bb] = c(NA, avg_margins_indiv_reduced$estimate)
      
      # Finally retrieve central estimate only for the significant coefficients. 
      for (coef in 1:length(model_indiv_direct$coefficients)){
        if (model_indiv_direct$prob[coef]>0.1){
          model_indiv_direct$coefficients[coef] = 0
        }
      }
      for (coef in 1:length(model_indiv_reduced$coefficients)){
        if (model_indiv_reduced$prob[coef]>0.1){
          model_indiv_reduced$coefficients[coef] = 0
        }
      }
      fitted_opportunity_indiv_direct_dist_onlysig[,bb] = predict(model_indiv_direct, newdata = indiv_data_pbar, type = 'response')
      fitted_opportunity_indiv_reduced_dist_onlysig[,bb] = predict(model_indiv_reduced, newdata = indiv_data_pbar, type = 'response')
      
    } else {
      
      # Else, Fitted values - use original sample but results from bootstrap
      fitted_opportunity_indiv_direct_dist[,bb] = predict(model_indiv_direct_boot, newdata = indiv_data_pbar, type = 'response')
      fitted_preference_indiv_direct_dist[,bb] = predict(model_indiv_direct_boot, newdata = indiv_data_cbar, type = 'response')
      fitted_indiv_direct_dist[,bb] = predict(model_indiv_direct_boot, newdata = indiv_data, type = 'response')
      
      fitted_opportunity_indiv_reduced_dist[,bb] = predict(model_indiv_reduced_boot, newdata = indiv_data_pbar, type = 'response')
      fitted_preference_indiv_reduced_dist[,bb] = predict(model_indiv_reduced_boot, newdata = indiv_data_cbar, type = 'response')
      fitted_indiv_reduced_dist[,bb] = predict(model_indiv_reduced_boot, newdata = indiv_data, type = 'response')
      
      # Save average marginal effects for both models (adding in intercept as NA)
      avg_margins_indiv_direct_boot = avg_slopes(model_indiv_direct_boot)
      avg_margins_indiv_direct_boot = avg_margins_indiv_direct_boot[match(rownames(avg_margins_indiv_direct_dist), avg_margins_indiv_direct_boot$term),]
      avg_margins_indiv_direct_dist[,bb] = avg_margins_indiv_direct_boot$estimate
      avg_margins_indiv_reduced_boot = avg_slopes(model_indiv_reduced_boot)
      avg_margins_indiv_reduced_boot = avg_margins_indiv_reduced_boot[match(rownames(avg_margins_indiv_reduced_dist), avg_margins_indiv_reduced_boot$term),]
      avg_margins_indiv_reduced_dist[,bb] =  avg_margins_indiv_reduced_boot$estimate
      
      # Finally retrieve central estimate only for the significant coefficients. 
      for (coef in 1:length(model_indiv_direct_boot$coefficients)){
        if (model_indiv_direct_boot$prob[coef]>0.1){
          model_indiv_direct_boot$coefficients[coef] = 0
        }
      }
      for (coef in 1:length(model_indiv_reduced_boot$coefficients)){
        if (model_indiv_reduced_boot$prob[coef]>0.1){
          model_indiv_reduced_boot$coefficients[coef] = 0
        }
      }
      fitted_opportunity_indiv_direct_dist_onlysig[,bb] = predict(model_indiv_direct_boot, newdata = indiv_data_pbar, type = 'response')
      fitted_opportunity_indiv_reduced_dist_onlysig[,bb] = predict(model_indiv_reduced_boot, newdata = indiv_data_pbar, type = 'response')
    }
 }
  
  ##################################
  ##### Test differences in AME ####
  ##################################
  
  # Here we test the differences between average marginal effects from the bootstrap
  # Bootstrap assumes asymptotic normal distribution. 
  # We also add in the bootstrapped standard errors for the AME 
  # And save the central scenario results as a .csv file. 
  
  # Prepare data frame for AME test results
  AME_tests = data.frame(
    "variable" = avg_margins_indiv_direct$term,
    "stat" = rep(NA, nrow(avg_margins_indiv_direct)),
    "pval" = rep(NA, nrow(avg_margins_indiv_direct))
  )
  
  # Loop through variables to compute Z-statistic and p-value
  for (r in 1:nrow(AME_tests)) {
    var = AME_tests$variable[r]
    row_idx_direct = which(rownames(avg_margins_indiv_direct_dist) == var)
    row_idx_reduced = which(rownames(avg_margins_indiv_reduced_dist) == var)
    
    if (length(row_idx_direct) == 1 & length(row_idx_reduced) == 1) { # Variable in both models
      ame_direct = as.numeric(avg_margins_indiv_direct$estimate[row_idx_direct])
      ame_reduced = as.numeric(avg_margins_indiv_reduced$estimate[row_idx_reduced])
      se_direct = sd(avg_margins_indiv_direct_dist[row_idx_direct,], na.rm = TRUE)
      se_reduced = sd(avg_margins_indiv_reduced_dist[row_idx_reduced,], na.rm = TRUE)
      cov_ame = cov(
        as.numeric(avg_margins_indiv_direct_dist[row_idx_direct,]), 
        as.numeric(avg_margins_indiv_reduced_dist[row_idx_reduced,]),
        method = 'pearson'
      )
      z_value = (ame_direct - ame_reduced) / sqrt(se_direct^2 + se_reduced^2 - 2 * cov_ame)
      AME_tests$stat[r] = z_value
      AME_tests$pval[r] = 2 * pnorm(z_value, lower.tail = FALSE)
    }
  }
  
  # Save AME test results to CSV
  write.csv(AME_tests, here("results", "Binary individual AME comparison tests.csv"))
  
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
  
  # Update and save results for both models
  results_indiv_model_direct = update_results_table(avg_margins_indiv_direct_dist, results_indiv_model_direct)
  write.csv(results_indiv_model_direct, here("results", "Binary individual model - direct - results.csv"))
  
  results_indiv_model_reduced = update_results_table(avg_margins_indiv_reduced_dist, results_indiv_model_reduced)
  write.csv(results_indiv_model_reduced, here("results", "Binary individual model - reduced - results.csv"))
  
  ##################################
  ##### Save and return results ####
  ##################################

  fitted_values_direct = list(fitted_values_indiv_direct, 
                          fitted_opportunity_indiv_direct_dist, 
                          fitted_preference_indiv_direct_dist,
                          fitted_indiv_direct_dist,
                          fitted_opportunity_indiv_direct_dist_onlysig)
  
  fitted_values_reduced = list(fitted_values_indiv_reduced, 
                              fitted_opportunity_indiv_reduced_dist, 
                              fitted_preference_indiv_reduced_dist,
                              fitted_indiv_reduced_dist,
                              fitted_opportunity_indiv_reduced_dist_onlysig)
  
  names(fitted_values_direct) = c("Central_fitted","Dist_fitted_opportunity", "Dist_fitted_preferences","Dist_fitted_all", "Central_onlysig")
  names(fitted_values_reduced) = c("Central_fitted","Dist_fitted_opportunity", "Dist_fitted_preferences","Dist_fitted_all", "Central_onlysig")
  
  fitted_binary_indiv = list(fitted_values_direct,
                           fitted_values_reduced)
  names(fitted_binary_indiv) = c("Direct_models", "Reduced_models")
  
  return(fitted_binary_indiv)
         
} 
         
# End of function

