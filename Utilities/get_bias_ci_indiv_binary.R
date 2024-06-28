
#This function performs the bias introduced model estimations
#For the individual models 

get_bias_ci_indiv_binary = function(indiv_data,Outcome_indiv, C_indiv, P_indiv, Z_indiv, V_indiv) {

  library(glmtoolbox)
  source(here("Utilities", "get_inequality_functions.R"))
  source(here("Utilities", "ov_generate.R"))
  
  set.seed(123)
  n_samples = 25
  n_bootstraps = 25
  threshold = 10 #Variance inflation factor threshold. 
  min_outcomes = 15
  
  #######################
  #### INDIV FULL MODEL ####
  #######################
  
  coefficient_vec = c(C_indiv, P_indiv, Z_indiv)
  coef_opportunity = C_indiv
  coef_preferences = c(P_indiv, Z_indiv)

  
  # Initialise dataframes to save mean results for each OV instance
  coefficient_range = as.data.frame(matrix(NA, ncol = n_samples * length(coefficient_vec), nrow = length(coefficient_vec)+2))
  rownames(coefficient_range) = c("intercept", coefficient_vec, "OV")
  opportunity_range = preference_range = fitted_range = as.data.frame(matrix(NA, ncol = n_samples * length(coefficient_vec), nrow = 6))
  rownames(opportunity_range) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(preference_range) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(fitted_range) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  
  #Equation for the model
  eq_indiv_full =  paste(Outcome_indiv, sep = " ~ ", paste(paste(C_indiv, collapse = " + "), paste(P_indiv, collapse = " + "), Z_indiv, 'OV', sep = " + "))
  
  # Equalise variance for circumstances and opportunity
  indiv_data_cbar = indiv_data_pbar =indiv_data
  indiv_data_cbar[C_indiv] = lapply(indiv_data_cbar[C_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  indiv_data_pbar[P_indiv] = lapply(indiv_data_pbar[P_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  indiv_data_pbar[Z_indiv] = lapply(indiv_data_pbar[Z_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  
  # Set counter for saving results. 
  counter = 0
  
  # Loop through all variables and test scenarios. 
  for (c in 1:length(coefficient_vec)){
    
    print(paste("Full model - ", coefficient_vec[c]))
    
    # Retrieve variable
    col_idx = which(names(indiv_data) == coefficient_vec[c])
    
    # Draw hypothetical correlations for sample frame. 
    rho_1 = runif(n_samples, min = -0.8, max = 0.8)
    rho_2 = runif(n_samples, min = -0.8, max = 0.8)
    rho = cbind(rho_1, rho_2)
    
    # Establish data frame
    y = cbind(indiv_data[,Outcome_indiv], indiv_data[,col_idx])
    
    #Generate hypothetical variable. 
    OV_base = rnorm(nrow(indiv_data))
    
    for (n in 1:n_samples){
      
      counter = counter+1
      # Reset bootstrap matrices
      coefficients_bb = as.data.frame(matrix(NA, ncol = n_bootstraps , nrow = length(coefficient_vec)+2))
      opportunity_bb = preference_bb = fitted_bb = as.data.frame(matrix(NA, ncol = n_bootstraps , nrow = 6))
      
      #Generate omitted variable, and skip if correlation not possible. 
      OV = ov_generate(y, rho[n,], OV_base)
      
      if (all(OV == 0)){
        next
      }
      
      indiv_data$OV = OV
      indiv_data_pbar$OV = OV
      indiv_data_cbar$OV = OV
      
      for (b in 1:n_bootstraps) {

        #Draw bootstrapped sample
        min_outcomes_condition  = FALSE
        while (min_outcomes_condition == FALSE){
          bootstrap_indices = sample(1:nrow(indiv_data), size = nrow(indiv_data), replace = TRUE)
          bootstrap_sample = indiv_data[bootstrap_indices, ]
          if (length(bootstrap_sample$BINARY_PARTICIPATION[bootstrap_sample$BINARY_PARTICIPATION > 0]) > min_outcomes) {
            # If condition is met, set the flag to True to exit the loop
            min_outcomes_condition = TRUE
          }
        }
        
        #Estimate the model and retrieve the coefficients. 
        model_indiv_full = logistf(eq_indiv_full, data = bootstrap_sample)
        
        #skip if not converging, indicated by OV results
        if (model_indiv_full$prob["OV"] == 0) {
          next
        }
        
        #Test VIF and skip if results are unstable. A
        #Estimate this with standard LOGIT model without correction due to output type mismatch for GVIF function
        #GVIF outcomes will be equivalent between methods. 
        model_indiv_full_VIF = glm(eq_indiv_full, data = indiv_data , family = binomial(link = "logit"))
        VIF = glmtoolbox::gvif(model_indiv_full_VIF, verbose = FALSE)
        if(max(VIF)> threshold){
          next
        }
        
        coefficients_bb[,b] = model_indiv_full$coefficients
        
        # Now estimate inequality indices and save in dataframe
        predicted_probs_opportunity =  predict(model_indiv_full, newdata = indiv_data_pbar, type = 'response')
        predicted_probs_preferences = predict(model_indiv_full, newdata = indiv_data_cbar, type = 'response')
        predicted_probs_fitted = predict(model_indiv_full, data = indiv_data, type = 'response')
        
        # Save inequality results in the initialised vectors
        opportunity_bb[1,b] =  PSY_ineq(predicted_probs_opportunity)
        opportunity_bb[2,b] =  ((PSY_ineq(predicted_probs_opportunity) - PSY_ineq(predicted_probs_fitted))/PSY_ineq(predicted_probs_fitted))
        opportunity_bb[3,b] =  erreygers_ineq(predicted_probs_opportunity)
        opportunity_bb[4,b] =  ((erreygers_ineq(predicted_probs_opportunity) - erreygers_ineq(predicted_probs_fitted))/erreygers_ineq(predicted_probs_fitted))
        opportunity_bb[5,b] =   GSC_ineq(predicted_probs_opportunity)
        opportunity_bb[6,b] =  ((GSC_ineq(predicted_probs_opportunity) - GSC_ineq(predicted_probs_fitted))/GSC_ineq(predicted_probs_fitted))
        
        preference_bb[1,b] =  PSY_ineq(predicted_probs_preferences)
        preference_bb[2,b] =  ((PSY_ineq(predicted_probs_preferences) - PSY_ineq(predicted_probs_fitted))/PSY_ineq(predicted_probs_fitted))
        preference_bb[3,b] =  erreygers_ineq(predicted_probs_preferences)
        preference_bb[4,b] =  ((erreygers_ineq(predicted_probs_preferences) - erreygers_ineq(predicted_probs_fitted))/erreygers_ineq(predicted_probs_fitted))
        preference_bb[5,b] =  GSC_ineq(predicted_probs_preferences)
        preference_bb[6,b] =  ((GSC_ineq(predicted_probs_preferences) - GSC_ineq(predicted_probs_fitted))/GSC_ineq(predicted_probs_fitted))
        
        fitted_bb[1,b] =  PSY_ineq(predicted_probs_fitted)
        fitted_bb[2,b] =  ((PSY_ineq(predicted_probs_fitted) - PSY_ineq(predicted_probs_fitted))/PSY_ineq(predicted_probs_fitted))
        fitted_bb[3,b] =  erreygers_ineq(predicted_probs_fitted)
        fitted_bb[4,b] =  ((erreygers_ineq(predicted_probs_fitted) - erreygers_ineq(predicted_probs_fitted))/erreygers_ineq(predicted_probs_fitted))
        fitted_bb[5,b] =  GSC_ineq(predicted_probs_fitted)
        fitted_bb[6,b] =  ((GSC_ineq(predicted_probs_fitted) - GSC_ineq(predicted_probs_fitted))/GSC_ineq(predicted_probs_fitted))
        
      }
        
      # Save mean bootstrap estimates in core dataframe. 
      coefficients = rowMeans(as.matrix(coefficients_bb), na.rm = TRUE)
      coefficient_range[,counter] = coefficients
      
      # Save inequality results in the initialised vectors
      opportunity_range[1,counter] =  mean(as.numeric(opportunity_bb[1,]), na.rm = TRUE)
      opportunity_range[2,counter] =  mean(as.numeric(opportunity_bb[2,]), na.rm = TRUE)
      opportunity_range[3,counter] =  mean(as.numeric(opportunity_bb[3,]), na.rm = TRUE)
      opportunity_range[4,counter] =  mean(as.numeric(opportunity_bb[4,]), na.rm = TRUE)
      opportunity_range[5,counter] =  mean(as.numeric(opportunity_bb[5,]), na.rm = TRUE)
      opportunity_range[6,counter] =  mean(as.numeric(opportunity_bb[6,]), na.rm = TRUE)
      
      preference_range[1,counter] =  mean(as.numeric(preference_bb[1,]), na.rm = TRUE)
      preference_range[2,counter] =  mean(as.numeric(preference_bb[2,]), na.rm = TRUE)
      preference_range[3,counter] =  mean(as.numeric(preference_bb[3,]), na.rm = TRUE)
      preference_range[4,counter] =  mean(as.numeric(preference_bb[4,]), na.rm = TRUE)
      preference_range[5,counter] =  mean(as.numeric(preference_bb[5,]), na.rm = TRUE)
      preference_range[6,counter] =  mean(as.numeric(preference_bb[6,]), na.rm = TRUE)
      
      fitted_range[1,counter] =  mean(as.numeric(fitted_bb[1,]), na.rm = TRUE)
      fitted_range[2,counter] =  mean(as.numeric(fitted_bb[2,]), na.rm = TRUE)
      fitted_range[3,counter] =  mean(as.numeric(fitted_bb[3,]), na.rm = TRUE)
      fitted_range[4,counter] =  mean(as.numeric(fitted_bb[4,]), na.rm = TRUE)
      fitted_range[5,counter] =  mean(as.numeric(fitted_bb[5,]), na.rm = TRUE)
      fitted_range[6,counter] =  mean(as.numeric(fitted_bb[6,]), na.rm = TRUE)
    }
  }
  
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
  write.csv(coefficient_intervals, file = here("Results", "coefficient_bias_intervals_indiv_binary_full.csv"))
  write.csv(opportunity_intervals, file = here("Results", "opportunity_ineq_bias_intervals_indiv_binary_full.csv"))
  write.csv(preference_intervals, file = here("Results", "preference_ineq_bias_intervals_indiv_binary_full.csv"))
  write.csv(fitted_intervals, file = here("Results", "fitted_ineq_bias_intervals_indiv_binary_full.csv"))
  
  ##########################
  #### INDIV REDUCED MODEL ####
  ##########################
  
  coefficient_vec = c(C_indiv, V_indiv)
  coef_opportunity = C_indiv
  coef_preferences = V_indiv
  
  # Initialise dataframes. 
  coefficient_range = as.data.frame(matrix(NA, ncol = n_samples * length(coefficient_vec), nrow = length(coefficient_vec)+2))
  rownames(coefficient_range) = c("intercept", coefficient_vec, "OV")
  opportunity_range = preference_range = fitted_range  = as.data.frame(matrix(NA, ncol = n_samples * length(coefficient_vec), nrow = 6))
  rownames(opportunity_range) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(preference_range) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(fitted_range) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  
  #Equation for the model
  eq_indiv_reduced =  paste(Outcome_indiv, sep = " ~ ", paste(paste(C_indiv, collapse = " + "), paste(V_indiv, collapse = " + "), 'OV', sep = " + "))
  
  # Equalise variance for circumstances and opportunity
  indiv_data_cbar = indiv_data_pbar =indiv_data
  indiv_data_cbar[C_indiv] = lapply(indiv_data_cbar[C_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  indiv_data_pbar[V_indiv] = lapply(indiv_data_pbar[V_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  
  # Set counter for saving results. 
  counter = 0
  
  # Loop through all variables and test scenarios. 
  for (c in 1:length(coefficient_vec)){
    print(paste("Reduced model - ", coefficient_vec[c]))
    
    # Retrieve variable
    col_idx = which(names(indiv_data) == coefficient_vec[c])
    
    # Draw hypothetical correlations for sample frame. 
    rho_1 = runif(n_samples, min = -0.8, max = 0.8)
    rho_2 = runif(n_samples, min = -0.8, max = 0.8)
    rho = cbind(rho_1, rho_2)
    
    # Establish data frame
    y = cbind(indiv_data[,Outcome_indiv], indiv_data[,col_idx])
    
    #Generate hypothetical variable. 
    OV_base = rnorm(nrow(indiv_data))
    
    for (n in 1:n_samples){
      
      counter = counter+1
      # Reset bootstrap matrices
      coefficients_bb = as.data.frame(matrix(NA, ncol = n_bootstraps , nrow = length(coefficient_vec)+2))
      opportunity_bb = preference_bb = fitted_bb = as.data.frame(matrix(NA, ncol = n_bootstraps , nrow = 6))
      
      #Generate omitted variable, and skip if correlation not possible. 
      OV = ov_generate(y, rho[n,], OV_base)
      
      if (all(OV == 0)){
        next
      }
      
      indiv_data$OV = OV
      indiv_data_pbar$OV = OV
      indiv_data_cbar$OV = OV
      
      for (b in 1:n_bootstraps) {
        
        #Draw bootstrapped sample
        min_outcomes_condition  = FALSE
        while (min_outcomes_condition == FALSE){
          bootstrap_indices = sample(1:nrow(indiv_data), size = nrow(indiv_data), replace = TRUE)
          bootstrap_sample = indiv_data[bootstrap_indices, ]
          if (length(bootstrap_sample$BINARY_PARTICIPATION[bootstrap_sample$BINARY_PARTICIPATION > 0]) > min_outcomes) {
            # If condition is met, set the flag to True to exit the loop
            min_outcomes_condition = TRUE
          }
        }
        
      #Estimate the model and retrieve the coefficients. 
      model_indiv_reduced = logistf(eq_indiv_reduced, data = bootstrap_sample)
      
      #skip if not converging, indicated by OV results
      if (model_indiv_reduced$prob["OV"] == 0) {
        next
      }
      
      #Test VIF and skip if results are unstable. A
      #Estimate this with standard LOGIT model without correction due to output type mismatch for GVIF function
      #GVIF outcomes will be equivalent between methods. 
      model_indiv_reduced_VIF = glm(eq_indiv_reduced, data = indiv_data , family = binomial(link = "logit"))
      VIF = glmtoolbox::gvif(model_indiv_reduced_VIF, verbose = FALSE)
      if(max(VIF)> threshold){
        next
      }
      
      coefficients_bb[,b] = model_indiv_reduced$coefficients
      
      # Now estimate inequality indices and save in dataframe
      predicted_probs_opportunity =  predict(model_indiv_reduced, newdata = indiv_data_pbar, type = 'response')
      predicted_probs_preferences = predict(model_indiv_reduced, newdata = indiv_data_cbar, type = 'response')
      predicted_probs_fitted = predict(model_indiv_reduced, data = indiv_data, type = 'response')
      
      # Save inequality results in the initialised vectors
      opportunity_bb[1,b] =  PSY_ineq(predicted_probs_opportunity)
      opportunity_bb[2,b] =  ((PSY_ineq(predicted_probs_opportunity) - PSY_ineq(predicted_probs_fitted))/PSY_ineq(predicted_probs_fitted))
      opportunity_bb[3,b] =  erreygers_ineq(predicted_probs_opportunity)
      opportunity_bb[4,b] =  ((erreygers_ineq(predicted_probs_opportunity) - erreygers_ineq(predicted_probs_fitted))/erreygers_ineq(predicted_probs_fitted))
      opportunity_bb[5,b] =   GSC_ineq(predicted_probs_opportunity)
      opportunity_bb[6,b] =  ((GSC_ineq(predicted_probs_opportunity) - GSC_ineq(predicted_probs_fitted))/GSC_ineq(predicted_probs_fitted))
      
      preference_bb[1,b] =  PSY_ineq(predicted_probs_preferences)
      preference_bb[2,b] =  ((PSY_ineq(predicted_probs_preferences) - PSY_ineq(predicted_probs_fitted))/PSY_ineq(predicted_probs_fitted))
      preference_bb[3,b] =  erreygers_ineq(predicted_probs_preferences)
      preference_bb[4,b] =  ((erreygers_ineq(predicted_probs_preferences) - erreygers_ineq(predicted_probs_fitted))/erreygers_ineq(predicted_probs_fitted))
      preference_bb[5,b] =  GSC_ineq(predicted_probs_preferences)
      preference_bb[6,b] =  ((GSC_ineq(predicted_probs_preferences) - GSC_ineq(predicted_probs_fitted))/GSC_ineq(predicted_probs_fitted))
      
      fitted_bb[1,b] =  PSY_ineq(predicted_probs_fitted)
      fitted_bb[2,b] =  ((PSY_ineq(predicted_probs_fitted) - PSY_ineq(predicted_probs_fitted))/PSY_ineq(predicted_probs_fitted))
      fitted_bb[3,b] =  erreygers_ineq(predicted_probs_fitted)
      fitted_bb[4,b] =  ((erreygers_ineq(predicted_probs_fitted) - erreygers_ineq(predicted_probs_fitted))/erreygers_ineq(predicted_probs_fitted))
      fitted_bb[5,b] =  GSC_ineq(predicted_probs_fitted)
      fitted_bb[6,b] =  ((GSC_ineq(predicted_probs_fitted) - GSC_ineq(predicted_probs_fitted))/GSC_ineq(predicted_probs_fitted))
      
      }
      
      # Save mean bootstrap estimates in core dataframe. 
      coefficients = rowMeans(as.matrix(coefficients_bb), na.rm = TRUE)
      coefficient_range[,counter] = coefficients
      
      # Save inequality results in the initialised vectors
      opportunity_range[1,counter] =  mean(as.numeric(opportunity_bb[1,]), na.rm = TRUE)
      opportunity_range[2,counter] =  mean(as.numeric(opportunity_bb[2,]), na.rm = TRUE)
      opportunity_range[3,counter] =  mean(as.numeric(opportunity_bb[3,]), na.rm = TRUE)
      opportunity_range[4,counter] =  mean(as.numeric(opportunity_bb[4,]), na.rm = TRUE)
      opportunity_range[5,counter] =  mean(as.numeric(opportunity_bb[5,]), na.rm = TRUE)
      opportunity_range[6,counter] =  mean(as.numeric(opportunity_bb[6,]), na.rm = TRUE)
      
      preference_range[1,counter] =  mean(as.numeric(preference_bb[1,]), na.rm = TRUE)
      preference_range[2,counter] =  mean(as.numeric(preference_bb[2,]), na.rm = TRUE)
      preference_range[3,counter] =  mean(as.numeric(preference_bb[3,]), na.rm = TRUE)
      preference_range[4,counter] =  mean(as.numeric(preference_bb[4,]), na.rm = TRUE)
      preference_range[5,counter] =  mean(as.numeric(preference_bb[5,]), na.rm = TRUE)
      preference_range[6,counter] =  mean(as.numeric(preference_bb[6,]), na.rm = TRUE)
      
      fitted_range[1,counter] =  mean(as.numeric(fitted_bb[1,]), na.rm = TRUE)
      fitted_range[2,counter] =  mean(as.numeric(fitted_bb[2,]), na.rm = TRUE)
      fitted_range[3,counter] =  mean(as.numeric(fitted_bb[3,]), na.rm = TRUE)
      fitted_range[4,counter] =  mean(as.numeric(fitted_bb[4,]), na.rm = TRUE)
      fitted_range[5,counter] =  mean(as.numeric(fitted_bb[5,]), na.rm = TRUE)
      fitted_range[6,counter] =  mean(as.numeric(fitted_bb[6,]), na.rm = TRUE)
    }
  }
  
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
  write.csv(coefficient_intervals, file = here("Results", "coefficient_bias_intervals_indiv_binary_reduced.csv"))
  write.csv(opportunity_intervals, file = here("Results", "opportunity_ineq_bias_intervals_indiv_binary_reduced.csv"))
  write.csv(preference_intervals, file = here("Results", "preference_ineq_bias_intervals_indiv_binary_reduced.csv"))
  write.csv(fitted_intervals, file = here("Results", "fitted_ineq_bias_intervals_indiv_binary_reduced.csv"))
  
  
}


# End of function


