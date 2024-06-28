
#This function performs the bias introduced model estimations
#For the hh and individual models 

get_bias_ci_hh_binary = function(hh_data,Outcome_hh, C_hh, P_hh, Z_hh, V_hh) {

  library(glmtoolbox)
  source(here("Utilities", "get_inequality_functions.R"))
  source(here("Utilities", "ov_generate.R"))
  
  set.seed(123)
  n_samples = 100
  threshold = 10 #Variance inflation factor threshold. 
  
  #######################
  #### HH FULL MODEL ####
  #######################
  
  coefficient_vec = c(C_hh, P_hh, Z_hh)
  coef_opportunity = C_hh
  coef_preferences = c(P_hh, Z_hh)
  
  # Initialise dataframes. 
  coefficient_range = as.data.frame(matrix(NA, ncol = n_samples * length(coefficient_vec), nrow = length(coefficient_vec)+2))
  rownames(coefficient_range) = c("intercept", coefficient_vec, "OV")
  opportunity_range = preference_range = fitted_range = as.data.frame(matrix(NA, ncol = n_samples * length(coefficient_vec), nrow = 6))
  rownames(opportunity_range) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(preference_range) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(fitted_range) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  
  #Equation for the model
  eq_hh_full =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(P_hh, collapse = " + "), Z_hh, 'OV', sep = " + "))
  
  # Equalise variance for circumstances and opportunity
  hh_data_cbar = hh_data_pbar =hh_data
  hh_data_cbar[C_hh] = lapply(hh_data_cbar[C_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[P_hh] = lapply(hh_data_pbar[P_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[Z_hh] = lapply(hh_data_pbar[Z_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  
  # Set counter for saving results. 
  counter = 0
  
  # Loop through all variables and test scenarios. 
  for (c in 1:length(coefficient_vec)){
  
  print(paste("Full model - ", coefficient_vec[c]))
    
  # Retrieve variable
  col_idx = which(names(hh_data) == coefficient_vec[c])
    
  # Draw hypothetical correlations for sample frame. 
  rho_1 = runif(n_samples, min = -0.8, max = 0.8)
  rho_2 = runif(n_samples, min = -0.8, max = 0.8)
  rho = cbind(rho_1, rho_2)
  
  # Establish data frame
  y = cbind(hh_data[,Outcome_hh], hh_data[,col_idx])
  
  #Generate hypothetical variable. 
  OV_base = rnorm(nrow(hh_data))
  
    for (n in 1:n_samples){
      
      counter = counter+1
      
      #Generate omitted variable, and skip if correlation not possible. 
      OV = ov_generate(y, rho[n,], OV_base)
      if (all(OV == 0)){
        next
      }
      hh_data$OV = OV
      hh_data_pbar$OV = OV
      hh_data_cbar$OV = OV
  
      #Estimate the model and retrieve the coefficients. 
      model_hh_full = logistf(eq_hh_full, data = hh_data)
      
      #skip if not converging, indicated by OV results
      if (model_hh_full$prob["OV"] == 0) {
        next
      }
      
      #Test VIF and skip if results are unstable. A
      #Estimate this with standard LOGIT model without correction due to output type mismatch for GVIF function
      #GVIF outcomes will be equivalent between methods. 
      model_hh_full_VIF = glm(eq_hh_full, data = hh_data , family = binomial(link = "logit"))
      VIF = glmtoolbox::gvif(model_hh_full_VIF, verbose = FALSE)
       if(max(VIF)> threshold){
        next
      }
      
      #Save coefficients into data structure. 
      coefficients = model_hh_full$coefficients
      coefficient_range[,counter] = coefficients
      
      # Now estimate inequality indices and save in dataframe
      predicted_probs_opportunity =  predict(model_hh_full, newdata = hh_data_pbar, type = 'response')
      predicted_probs_preferences = predict(model_hh_full, newdata = hh_data_cbar, type = 'response')
      predicted_probs_fitted = predict(model_hh_full, data = hh_data, type = 'response')

      # Save inequality results in the initialised vectors
      opportunity_range[1,counter] =  PSY_ineq(predicted_probs_opportunity)
      opportunity_range[2,counter] =  ((PSY_ineq(predicted_probs_opportunity) - PSY_ineq(predicted_probs_fitted))/PSY_ineq(predicted_probs_fitted))
      opportunity_range[3,counter] =  erreygers_ineq(predicted_probs_opportunity)
      opportunity_range[4,counter] =  ((erreygers_ineq(predicted_probs_opportunity) - erreygers_ineq(predicted_probs_fitted))/erreygers_ineq(predicted_probs_fitted))
      opportunity_range[5,counter] =   GSC_ineq(predicted_probs_opportunity)
      opportunity_range[6,counter] =  ((GSC_ineq(predicted_probs_opportunity) - GSC_ineq(predicted_probs_fitted))/GSC_ineq(predicted_probs_fitted))
      
      preference_range[1,counter] =  PSY_ineq(predicted_probs_preferences)
      preference_range[2,counter] =  ((PSY_ineq(predicted_probs_preferences) - PSY_ineq(predicted_probs_fitted))/PSY_ineq(predicted_probs_fitted))
      preference_range[3,counter] =  erreygers_ineq(predicted_probs_preferences)
      preference_range[4,counter] =  ((erreygers_ineq(predicted_probs_preferences) - erreygers_ineq(predicted_probs_fitted))/erreygers_ineq(predicted_probs_fitted))
      preference_range[5,counter] =  GSC_ineq(predicted_probs_preferences)
      preference_range[6,counter] =  ((GSC_ineq(predicted_probs_preferences) - GSC_ineq(predicted_probs_fitted))/GSC_ineq(predicted_probs_fitted))
      
      fitted_range[1,counter] =  PSY_ineq(predicted_probs_fitted)
      fitted_range[2,counter] =  ((PSY_ineq(predicted_probs_fitted) - PSY_ineq(predicted_probs_fitted))/PSY_ineq(predicted_probs_fitted))
      fitted_range[3,counter] =  erreygers_ineq(predicted_probs_fitted)
      fitted_range[4,counter] =  ((erreygers_ineq(predicted_probs_fitted) - erreygers_ineq(predicted_probs_fitted))/erreygers_ineq(predicted_probs_fitted))
      fitted_range[5,counter] =  GSC_ineq(predicted_probs_fitted)
      fitted_range[6,counter] =  ((GSC_ineq(predicted_probs_fitted) - GSC_ineq(predicted_probs_fitted))/GSC_ineq(predicted_probs_fitted))
      
    }
  }
  
  #Trim significant outliers (model failed to converge)
  outliers = c()
  for (r in 1:nrow(coefficient_range)){
    left_tail = quantile(coefficient_range[r,], 0.01, na.rm = TRUE)
    right_tail = quantile(coefficient_range[r,], 0.99, na.rm = TRUE)
    left_idx = which(coefficient_range[r,] <= left_tail)
    right_idx = which(coefficient_range[r,] >= right_tail)
    outliers = c(outliers, left_idx, right_idx)
  }
  coefficient_range = coefficient_range[,-outliers]
  opportunity_range = opportunity_range[,-outliers]
  preference_range = preference_range[,-outliers]
  fitted_range = fitted_range[,-outliers]

  # Remove columns with NA values
  coefficient_range = coefficient_range[ , colSums(is.na(coefficient_range))==0]
  opportunity_range = opportunity_range[ , colSums(is.na(opportunity_range))==0]
  preference_range = preference_range[ , colSums(is.na(preference_range))==0]
  fitted_range = fitted_range[ , colSums(is.na(fitted_range))==0]
  
  # Save confidence intervals for coefficients
  coefficient_intervals = as.data.frame(matrix(NA, nrow = length(coefficient_vec)+2, ncol = 2))
  rownames(coefficient_intervals) = c("intercept", coefficient_vec, "OV")
  colnames(coefficient_intervals) = c("left_95", "right_95")
  for (r in 1:nrow(coefficient_range)){
  test  = t.test(coefficient_range[r,])
  coefficient_intervals[r,1] = test$conf.int[1]
  coefficient_intervals[r,2]  = test$conf.int[2]
  }
  
  # Save confidence intervals for inequality indices
  opportunity_intervals = preference_intervals = fitted_intervals = all_intervals =  as.data.frame(matrix(NA, nrow = 6, ncol = 2))
  rownames(opportunity_intervals) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(preference_intervals) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(fitted_intervals) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  colnames(opportunity_intervals) = c("left_95", "right_95")
  colnames(preference_intervals)= c("left_95", "right_95")
  colnames(fitted_intervals)= c("left_95", "right_95")

   for (r in 1:6){
    test  = t.test(opportunity_range[r,])
    opportunity_intervals[r,1] = test$conf.int[1]
    opportunity_intervals[r,2]  = test$conf.int[2]
    test  = t.test(preference_range[r,])
    preference_intervals[r,1] = test$conf.int[1]
    preference_intervals[r,2]  = test$conf.int[2]
    test  = t.test(fitted_range[r,])
    fitted_intervals[r,1] = test$conf.int[1]
    fitted_intervals[r,2]  = test$conf.int[2]

   }
  fitted_intervals = na.omit(fitted_intervals) # relative measures do not matter for this one. 
  
  #write csv files with ranges. 
  write.csv(coefficient_intervals, file = here("Results", "coefficient_bias_intervals_hh_binary_full.csv"))
  write.csv(opportunity_intervals, file = here("Results", "opportunity_ineq_bias_intervals_hh_binary_full.csv"))
  write.csv(preference_intervals, file = here("Results", "preference_ineq_bias_intervals_hh_binary_full.csv"))
  write.csv(fitted_intervals, file = here("Results", "fitted_ineq_bias_intervals_hh_binary_full.csv"))
  
  ##########################
  #### HH REDUCED MODEL ####
  ##########################
  
  coefficient_vec = c(C_hh, V_hh)
  coef_opportunity = C_hh
  coef_preferences = V_hh
  
  # Initialise dataframes. 
  coefficient_range = as.data.frame(matrix(NA, ncol = n_samples * length(coefficient_vec), nrow = length(coefficient_vec)+2))
  rownames(coefficient_range) = c("intercept", coefficient_vec, "OV")
  opportunity_range = preference_range = fitted_range  = as.data.frame(matrix(NA, ncol = n_samples * length(coefficient_vec), nrow = 6))
  rownames(opportunity_range) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(preference_range) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(fitted_range) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  
  #Equation for the model
  eq_hh_reduced =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(V_hh, collapse = " + "), 'OV', sep = " + "))
  
  # Equalise variance for circumstances and opportunity
  hh_data_cbar = hh_data_pbar =hh_data
  hh_data_cbar[C_hh] = lapply(hh_data_cbar[C_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[V_hh] = lapply(hh_data_pbar[V_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
 
  # Set counter for saving results. 
  counter = 0
  
  # Loop through all variables and test scenarios. 
  for (c in 1:length(coefficient_vec)){
    print(paste("Reduced model - ", coefficient_vec[c]))
    
    # Retrieve variable
    col_idx = which(names(hh_data) == coefficient_vec[c])
    
    # Draw hypothetical correlations for sample frame. 
    rho_1 = runif(n_samples, min = -0.8, max = 0.8)
    rho_2 = runif(n_samples, min = -0.8, max = 0.8)
    rho = cbind(rho_1, rho_2)
    
    # Establish data frame
    y = cbind(hh_data[,Outcome_hh], hh_data[,col_idx])
    
    #Generate hypothetical variable. 
    OV_base = rnorm(nrow(hh_data))
    
    for (n in 1:n_samples){
      
      counter = counter + 1
      
      #Generate omitted variable, and skip if correlation not possible. 
      OV = ov_generate(y, rho[n,], OV_base)
      if (all(OV == 0)){
        next
      }
      hh_data$OV = OV
      hh_data_pbar$OV = OV
      hh_data_cbar$OV = OV
      
      #Estimate the model and retrieve the coefficients. 
      model_hh_reduced = logistf(eq_hh_reduced, data = hh_data)
      
      #skip if not converging, indicated by OV results
      if (model_hh_reduced$prob["OV"] == 0) {
        next
      }
      
      #Test VIF and skip if results are unstable. 
      model_hh_reduced_VIF = glm(eq_hh_reduced, data = hh_data , family = binomial(link = "logit"))
      VIF = glmtoolbox::gvif(model_hh_reduced_VIF, verbose = FALSE)
      if(max(VIF)> threshold){
        next
      }
      
      #Save coefficients into data structure. 
      coefficients = model_hh_reduced$coefficients
      coefficient_range[,counter] = coefficients
      
      # Now estimate inequality indices and save in dataframe
      predicted_probs_opportunity =  predict(model_hh_reduced, newdata = hh_data_pbar, type = 'response')
      predicted_probs_preferences = predict(model_hh_reduced, newdata = hh_data_cbar, type = 'response')
      predicted_probs_fitted = predict(model_hh_reduced, data = hh_data, type = 'response')
      
      # Save inequality results in the initialised vectors
      opportunity_range[1,counter] =  PSY_ineq(predicted_probs_opportunity)
      opportunity_range[2,counter] =  ((PSY_ineq(predicted_probs_opportunity) - PSY_ineq(predicted_probs_fitted))/PSY_ineq(predicted_probs_fitted))
      opportunity_range[3,counter] =  erreygers_ineq(predicted_probs_opportunity)
      opportunity_range[4,counter] =  ((erreygers_ineq(predicted_probs_opportunity) - erreygers_ineq(predicted_probs_fitted))/erreygers_ineq(predicted_probs_fitted))
      opportunity_range[5,counter] =   GSC_ineq(predicted_probs_opportunity)
      opportunity_range[6,counter] =  ((GSC_ineq(predicted_probs_opportunity) - GSC_ineq(predicted_probs_fitted))/GSC_ineq(predicted_probs_fitted))
      
      preference_range[1,counter] =  PSY_ineq(predicted_probs_preferences)
      preference_range[2,counter] =  ((PSY_ineq(predicted_probs_preferences) - PSY_ineq(predicted_probs_fitted))/PSY_ineq(predicted_probs_fitted))
      preference_range[3,counter] =  erreygers_ineq(predicted_probs_preferences)
      preference_range[4,counter] =  ((erreygers_ineq(predicted_probs_preferences) - erreygers_ineq(predicted_probs_fitted))/erreygers_ineq(predicted_probs_fitted))
      preference_range[5,counter] =  GSC_ineq(predicted_probs_preferences)
      preference_range[6,counter] =  ((GSC_ineq(predicted_probs_preferences) - GSC_ineq(predicted_probs_fitted))/GSC_ineq(predicted_probs_fitted))
      
      fitted_range[1,counter] =  PSY_ineq(predicted_probs_fitted)
      fitted_range[2,counter] =  ((PSY_ineq(predicted_probs_fitted) - PSY_ineq(predicted_probs_fitted))/PSY_ineq(predicted_probs_fitted))
      fitted_range[3,counter] =  erreygers_ineq(predicted_probs_fitted)
      fitted_range[4,counter] =  ((erreygers_ineq(predicted_probs_fitted) - erreygers_ineq(predicted_probs_fitted))/erreygers_ineq(predicted_probs_fitted))
      fitted_range[5,counter] =  GSC_ineq(predicted_probs_fitted)
      fitted_range[6,counter] =  ((GSC_ineq(predicted_probs_fitted) - GSC_ineq(predicted_probs_fitted))/GSC_ineq(predicted_probs_fitted))
      
    }
  }
  
  #Trim significant outliers (model failed to converge)
  outliers = c()
  for (r in 1:nrow(coefficient_range)){
    left_tail = quantile(coefficient_range[r,], 0.01, na.rm = TRUE)
    right_tail = quantile(coefficient_range[r,], 0.99, na.rm = TRUE)
    left_idx = which(coefficient_range[r,] <= left_tail)
    right_idx = which(coefficient_range[r,] >= right_tail)
    outliers = c(outliers, left_idx, right_idx)
  }
  coefficient_range = coefficient_range[,-outliers]
  opportunity_range = opportunity_range[,-outliers]
  preference_range = preference_range[,-outliers]
  fitted_range = fitted_range[,-outliers]
  
  # Remove columns with NA values
  coefficient_range = coefficient_range[ , colSums(is.na(coefficient_range))==0]
  opportunity_range = opportunity_range[ , colSums(is.na(opportunity_range))==0]
  preference_range = preference_range[ , colSums(is.na(preference_range))==0]
  fitted_range = fitted_range[ , colSums(is.na(fitted_range))==0]
  
  # Save confidence intervals for coefficients
  coefficient_intervals = as.data.frame(matrix(NA, nrow = length(coefficient_vec)+2, ncol = 2))
  rownames(coefficient_intervals) = c("intercept", coefficient_vec, "OV")
  colnames(coefficient_intervals) = c("left_95", "right_95")
  for (r in 1:nrow(coefficient_range)){
    test  = t.test(coefficient_range[r,])
    coefficient_intervals[r,1] = test$conf.int[1]
    coefficient_intervals[r,2]  = test$conf.int[2]
  }
  
  # Save confidence intervals for inequality indices
  opportunity_intervals = preference_intervals = fitted_intervals = all_intervals =  as.data.frame(matrix(NA, nrow = 6, ncol = 2))
  rownames(opportunity_intervals) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(preference_intervals) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(fitted_intervals) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  colnames(opportunity_intervals) = c("left_95", "right_95")
  colnames(preference_intervals)= c("left_95", "right_95")
  colnames(fitted_intervals)= c("left_95", "right_95")

  
  for (r in 1:6){
    test  = t.test(opportunity_range[r,])
    opportunity_intervals[r,1] = test$conf.int[1]
    opportunity_intervals[r,2]  = test$conf.int[2]
    test  = t.test(preference_range[r,])
    preference_intervals[r,1] = test$conf.int[1]
    preference_intervals[r,2]  = test$conf.int[2]
    test  = t.test(fitted_range[r,])
    fitted_intervals[r,1] = test$conf.int[1]
    fitted_intervals[r,2]  = test$conf.int[2]
  }
  fitted_intervals = na.omit(fitted_intervals) # relative measures do not matter for this one. 
  
  #write csv files with ranges. 
  write.csv(coefficient_intervals, file = here("Results", "coefficient_bias_intervals_hh_binary_reduced.csv"))
  write.csv(opportunity_intervals, file = here("Results", "opportunity_ineq_bias_intervals_hh_binary_reduced.csv"))
  write.csv(preference_intervals, file = here("Results", "preference_ineq_bias_intervals_hh_binary_reduced.csv"))
  write.csv(fitted_intervals, file = here("Results", "fitted_ineq_bias_intervals_hh_binary_reduced.csv"))

  
}


# End of function


