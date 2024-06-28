
# This function performs the continuous measure estimation for the reduced model
# Only for the household model. 

get_fitted_continuous_reduced = function(hh_data, 
                                      Outcome_hh,
                                      C_hh, 
                                      V_hh) {
  
  
  library(AER)
  
  #Equation
  eq_hh_reduced =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(V_hh, collapse = " + "), sep = " + "))
  eq_hh_reduced = as.formula(eq_hh_reduced)
  
  #Estimate model
  model_hh_reduced = tobit(eq_hh_reduced, left = 0, right = 1, data = hh_data)
  
  #Summary of model
  model_summary_hh_reduced = summary(model_hh_reduced)
  
  # Calculate pseudo R-squared McDonald and Xu (1995)
  log_likelihood = -model_hh_reduced$loglik[2]
  Mcdonald_R_squared = 1 - (model_hh_reduced$loglik[2]/ model_hh_reduced$loglik[1])
  aic = -2*log_likelihood +2*length(model_hh_reduced$coef)
  
  tests_df = data.frame(
    "log_likelihood" = log_likelihood,
    "pseudo_R_squared" = Mcdonald_R_squared,
    "aic" = aic
  )
  
  write.csv(tests_df, here("results", "Continuous Household model - reduced - tests.csv"))
  
  results_hh_model_reduced = data.frame("coefs" = c(model_hh_reduced$coefficients, model_hh_reduced$icoef[2]),
                                     "standard_errors" = sqrt(diag(vcov(model_hh_reduced))))
  
  write.csv(tests_df, here("results", "Continuous Household model - reduced - results.csv"))
  
  # Save output table
  library(stargazer)
  model_output = stargazer(model_hh_reduced,  
                           title="Continuous Household model - reduced" , 
                           type = "html",
                           out=here("Results","Continuous Household model - reduced.html")
  )
  
  #Retrieve coefficients
  coefficients = coef(model_hh_reduced)
  
  #Opportunity fitted values
  coef_opportunity = C_hh
  coefficients_opportunity = coefficients[coef_opportunity]
  model_matrix_opportunity = model.matrix(as.formula(eq_hh_reduced), data = hh_data)[, coef_opportunity]
  linear_predictor_opportunity = as.matrix(model_matrix_opportunity) %*% as.matrix(coefficients_opportunity)
  
  #Preference fitted values
  coef_preferences = V_hh
  coefficients_preferences = coefficients[coef_preferences]
  model_matrix_preferences = model.matrix(as.formula(eq_hh_reduced), data = hh_data)[, coef_preferences]
  linear_predictor_preferences = as.matrix(model_matrix_preferences) %*% as.matrix(coefficients_preferences)
  
  #Fitted outcomes (without error term)
  linear_predictor = fitted(model_hh_reduced)
  
  #Fitted outcomes (with error term)
  linear_predictor_all = fitted(model_hh_reduced) + residuals(model_hh_reduced)
  
  #Combine into single output
  fitted_values_hh_model = cbind(linear_predictor_opportunity, linear_predictor_preferences, linear_predictor, linear_predictor_all)
  colnames(fitted_values_hh_model) = c("fitted_opportunity", "fitted_preferences", "fitted", "fitted_w_error")
  
  #Extract distribution of coefficients and fitted values for intervals. 
  b = 1000
  
  fitted_opportunity_hh_model_dist = matrix(NA, nrow = nrow(fitted_values_hh_model), ncol = b)
  fitted_preferences_hh_model_dist = matrix(NA, nrow = nrow(fitted_values_hh_model), ncol = b)
  fitted_hh_model_dist = matrix(NA, nrow = nrow(fitted_values_hh_model), ncol = b)
  model_matrix_all =  model.matrix(as.formula(eq_hh_reduced), data = hh_data)
  
  for (bb in 1:b){
    
    #Draw sample of coefficients from coef and sd 
    results_hh_model_reduced$new_coefs = rnorm(n = length(results_hh_model_full$coefs), mean = results_hh_model_full$coefs, sd = results_hh_model_full$standard_errors)
    
    coefficients_bb = results_hh_model_reduced$new_coefs[which(rownames(results_hh_model_reduced) %in% coef_opportunity)]
    linear_predictor_opportunity_bb = as.matrix(model_matrix_opportunity) %*% as.matrix(coefficients_bb)
    fitted_opportunity_hh_model_dist[,bb] = linear_predictor_opportunity_bb
    
    coefficients_preferences_bb = results_hh_model_reduced$new_coefs[which(rownames(results_hh_model_reduced) %in% coef_preferences)]
    model_matrix_preferences_bb = model.matrix(as.formula(eq_hh_full), data = hh_data)[, coefficients_preferences_bb]
    fitted_preferences_hh_model_dist[,bb] = as.matrix(model_matrix_preferences) %*% as.matrix(coefficients_preferences_bb)
    
    coefficients_all = results_hh_model_full$new_coefs[1:length(results_hh_model_full$new_coefs)-1] # remove log scale
    linear_predictor_all_bb = as.matrix(model_matrix_all) %*% as.matrix(coefficients_all)
    fitted_hh_model_dist[,bb] = linear_predictor_all_bb
  }
  
  fitted_values_hh = list(fitted_values_hh_model, 
                          fitted_opportunity_hh_model_dist, 
                          fitted_preferences_hh_model_dist,
                          fitted_hh_model_dist)
  
  names(fitted_values_hh) = c("Central_fitted","Dist_fitted_opportunity", "Dist_fitted_preferences","Dist_fitted_all" )
  
  return(fitted_values_hh)
  
}
