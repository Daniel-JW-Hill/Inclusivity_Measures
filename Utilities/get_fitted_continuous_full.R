
# This function performs the continuous measure estimation for the full model
# Only for the household model. 

get_fitted_continuous_full = function(hh_data, 
                                  Outcome_hh,
                                  C_hh,
                                  P_hh,
                                  Z_hh) {
  
  
  library(AER)
  
  #Equation
  eq_hh_full =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(P_hh, collapse = " + "), Z_hh, sep = " + "))
  eq_hh_full = as.formula(eq_hh_full)
  
  #Estimate model
  model_hh_full = tobit(eq_hh_full, left = 0, right = 1, data = hh_data )
  
  #Summary of model
  model_summary_hh_full = summary(model_hh_full)
  
  # Calculate pseudo R-squared McDonald and Xu (1995)
  log_likelihood = -model_hh_full$loglik[2]
  Mcdonald_R_squared = 1 - (model_hh_full$loglik[2]/ model_hh_full$loglik[1])
  aic = -2*log_likelihood +2*length(model_hh_full$coef)
  
  tests_df = data.frame(
    "log_likelihood" = log_likelihood,
    "pseudo_R_squared" = Mcdonald_R_squared,
    "aic" = aic
  )
  
  write.csv(tests_df, here("results", "Continuous Household model - full - tests.csv"))
  
  results_hh_model_full = data.frame("coefs" = c(model_hh_full$coefficients, model_hh_full$icoef[2]),
                                     "standard_errors" = sqrt(diag(vcov(model_hh_full))))
  
  write.csv(tests_df, here("results", "Continuous Household model - full - results.csv"))
  
  # Save output table
  library(stargazer)
  model_output = stargazer(model_hh_full,  
                           title="Continuous Household model - full" , 
                           type = "html",
                           out=here("Results","Continuous Household model - full.html")
  )
 
  #Save central estimates 
  #Retrieve coefficients
  coefficients = coef(model_hh_full)
  
  #Opportunity fitted values
  coef_opportunity = C_hh
  coefficients_opportunity = coefficients[coef_opportunity]
  model_matrix_opportunity = model.matrix(as.formula(eq_hh_full), data = hh_data)[, coef_opportunity]
  linear_predictor_opportunity = as.matrix(model_matrix_opportunity) %*% as.matrix(coefficients_opportunity)
  
  #Preference fitted values
  coef_preferences = c(P_hh, Z_hh)
  coefficients_preferences = coefficients[coef_preferences]
  model_matrix_preferences = model.matrix(as.formula(eq_hh_full), data = hh_data)[, coef_preferences]
  linear_predictor_preferences = as.matrix(model_matrix_preferences) %*% as.matrix(coefficients_preferences)
  
  #Fitted outcomes (without error term)
  linear_predictor = fitted(model_hh_full)
  
  #Fitted outcomes (with error term)
  linear_predictor_all = fitted(model_hh_full) + residuals(model_hh_full)
  
  #Combine into single output
  fitted_values_hh_model = cbind(linear_predictor_opportunity, linear_predictor_preferences, linear_predictor, linear_predictor_all)
  colnames(fitted_values_hh_model) = c("fitted_opportunity", "fitted_preferences", "fitted", "fitted_w_error")
  
  #Extract distribution of coefficients and fitted values for intervals. 
  b = 1000
  
  fitted_opportunity_hh_model_dist = matrix(NA, nrow = nrow(fitted_values_hh_model), ncol = b)
  fitted_preferences_hh_model_dist = matrix(NA, nrow = nrow(fitted_values_hh_model), ncol = b)
  fitted_hh_model_dist = matrix(NA, nrow = nrow(fitted_values_hh_model), ncol = b)
  model_matrix_all =  model.matrix(as.formula(eq_hh_full), data = hh_data)
  
  for (bb in 1:b){
    
    #Draw sample of coefficients from coef and sd 
    results_hh_model_full$new_coefs = rnorm(n = length(results_hh_model_full$coefs), mean = results_hh_model_full$coefs, sd = results_hh_model_full$standard_errors)
    
    coefficients_bb = results_hh_model_full$new_coefs[which(rownames(results_hh_model_full) %in% coef_opportunity)]
    linear_predictor_opportunity_bb = as.matrix(model_matrix_opportunity) %*% as.matrix(coefficients_bb)
    fitted_opportunity_hh_model_dist[,bb] = linear_predictor_opportunity_bb
    
    coefficients_preferences_bb = results_hh_model_full$new_coefs[which(rownames(results_hh_model_full) %in% coef_preferences)]
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
