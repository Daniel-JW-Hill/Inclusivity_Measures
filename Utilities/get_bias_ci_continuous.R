
#This function performs the bias introduced model estimations
#For the continuous household models

get_bias_ci_continuous = function(hh_data, Outcome_hh, C_hh, P_hh, Z_hh, V_hh) {

  library(glmmTMB)
  library(performance)
  library(glmtoolbox)
  library(rms) 
  source(here("Utilities", "get_inequality_functions.R"))
  source(here("Utilities", "ov_generate.R"))
  
  set.seed(123)
  n_samples = 25
  n_bootstraps = 20
  threshold = 10 # Variance inflation factor threshold. 
  threshold_2 = 10 # VIF for conditional model can be set differently
  min_outcomes = 15
  
  #######################
  #### HH FULL MODEL ####
  #######################
  
  coefficient_vec = c(C_hh, P_hh, Z_hh)
  coef_opportunity = C_hh
  coef_preferences = c(P_hh, Z_hh)
  
  # Initialise dataframes. 
  coefficient_range_zi = as.data.frame(matrix(NA, ncol = n_samples * length(coefficient_vec), nrow = length(coefficient_vec)+2))
  rownames(coefficient_range_zi) = c("intercept", coefficient_vec, "OV")
  coefficient_range_cond =  coefficient_range_zi
  
  opportunity_range_zi = preferences_range_zi = fitted_range_zi = as.data.frame(matrix(NA, ncol = n_samples * length(coefficient_vec), nrow = 6))
  rownames(opportunity_range_zi) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(preferences_range_zi) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(fitted_range_zi) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  opportunity_range_conditional = opportunity_range_response = opportunity_range_zi
  preferences_range_conditional = preferences_range_response = preferences_range_zi
  fitted_range_conditional = fitted_range_response = fitted_range_zi
  
  #Equations for the model
  eq_hh_full =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(P_hh, collapse = " + "), Z_hh, 'OV', sep = " + "))
  eq_hh_zero = paste(" ~ ", paste(paste(C_hh, collapse = " + "), paste(P_hh, collapse = " + "), Z_hh, 'OV', sep = " + "), sep = "")
  eq_hh_full = as.formula(eq_hh_full)
  eq_hh_zero = as.formula(eq_hh_zero)
  
  ### Create new data frames equalising preferences, and equalising circumstances. 
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
      
      # Reset bootstrap matrices
      coefficients_bb_zi = coefficients_bb_cond = as.data.frame(matrix(NA, ncol = n_bootstraps , nrow = length(coefficient_vec)+2))
      opportunity_bb_zi = preferences_bb_zi = fitted_bb_zi = as.data.frame(matrix(NA, ncol = n_bootstraps , nrow = 6))
      opportunity_bb_response = preferences_bb_response = fitted_bb_response  = as.data.frame(matrix(NA, ncol = n_bootstraps , nrow = 6))
      opportunity_bb_conditional = preferences_bb_conditional = fitted_bb_conditional = as.data.frame(matrix(NA, ncol = n_bootstraps , nrow = 6))
      
      #Generate omitted variable, and skip if correlation not possible. 
      OV = ov_generate(y, rho[n,], OV_base)
      if (all(OV == 0)){
        next
      }
      
      hh_data$OV = OV
      hh_data_cbar$OV = OV
      hh_data_pbar$OV = OV
      
      for (b in 1:n_bootstraps) {
       
        #Draw bootstrap sample
        min_outcomes_condition  = FALSE
        while (min_outcomes_condition == FALSE){
          bootstrap_indices = sample(1:nrow(hh_data), size = nrow(hh_data), replace = TRUE)
          bootstrap_sample = hh_data[bootstrap_indices, ]
          if (length(bootstrap_sample$BINARY_PARTICIPATION[bootstrap_sample$BINARY_PARTICIPATION > 0]) > min_outcomes) {
            # If condition is met, set the flag to True to exit the loop
            min_outcomes_condition = TRUE
          }
        }
        
        #Estimate the model and retrieve the coefficients. 
        model_hh_full = glmmTMB(eq_hh_full, data = bootstrap_sample, family = beta_family(link = "logit"), ziformula = eq_hh_zero, dispformula = ~ 1)
        model_summary = summary(model_hh_full)
        
        if (model_hh_full$fit$convergence != 0){ #failed to converge
          next
        }
        
        #Test VIF and skip if results are unstable.
        #Test this using 2 distinct models as VIF functions for zero inflated beta hard to do
        eq_hh_full_zi = paste("BINARY_PARTICIPATION", sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(P_hh, collapse = " + "), Z_hh, 'OV', sep = " + "))
        zi_model_hh_full_VIF = glm(eq_hh_full_zi, data = bootstrap_sample, family = binomial(link = "logit"))
        VIF_zi = glmtoolbox::gvif(zi_model_hh_full_VIF, verbose = FALSE)
        if (any(is.na(VIF_zi))){
          next
        } else if (max(VIF_zi)> threshold){
          next
        }
        #Test continuous using poisson distribution (for simplicity just to detect multicollinearity)
        cond_model_hh_full_VIF = glm(eq_hh_full_zi, data = subset(bootstrap_sample, bootstrap_sample$CONTINUOUS_PARTICIPATION>0 , family = poisson(link = "log")))
        VIF_cond = glmtoolbox::gvif(cond_model_hh_full_VIF, verbose = FALSE)
        if (any(is.na(VIF_cond))){
          next
        } else if (max(VIF_cond)> threshold_2){
          next
        }
        
        #Save coefficients into data structure.
        coefficients_bb_zi[,b] = as.numeric(model_summary$coefficients$zi[,1])
        coefficients_bb_cond[,b] = as.numeric(model_summary$coefficients$cond[,1])
        
        # Now estimate inequality indices and save in dataframe
        pred_opportunity_zi = predict(model_hh_full, newdata = hh_data_pbar, type = 'zprob')
        pred_opportunity_response = predict(model_hh_full, newdata = hh_data_pbar, type = 'response')
        pred_opportunity_conditional = predict(model_hh_full, newdata = hh_data_pbar, type = 'conditional')
        
        pred_preferences_zi = predict(model_hh_full, newdata = hh_data_cbar, type = 'zprob')
        pred_preferences_response = predict(model_hh_full, newdata = hh_data_cbar, type = 'response')
        pred_preferences_conditional = predict(model_hh_full, newdata = hh_data_cbar, type = 'conditional')
        
        pred_zi = predict(model_hh_full, newdata = hh_data, type = 'zprob')
        pred_response = predict(model_hh_full, newdata = hh_data, type = 'response')
        pred_conditional = predict(model_hh_full, newdata = hh_data, type = 'conditional')
        
        # Save inequality results in the initialised vectors
        opportunity_bb_zi[1,b] =  PSY_ineq (pred_opportunity_zi)
        opportunity_bb_zi[2,b] =  ((PSY_ineq (pred_opportunity_zi) - PSY_ineq (pred_zi))/PSY_ineq (pred_zi))
        opportunity_bb_zi[3,b] =  erreygers_ineq (pred_opportunity_zi)
        opportunity_bb_zi[4,b] =  ((erreygers_ineq (pred_opportunity_zi) - erreygers_ineq (pred_zi))/erreygers_ineq (pred_zi))
        opportunity_bb_zi[5,b] =   GSC_ineq (pred_opportunity_zi)
        opportunity_bb_zi[6,b] =  ((GSC_ineq (pred_opportunity_zi) - GSC_ineq (pred_zi))/GSC_ineq (pred_zi))
        opportunity_bb_response[1,b] =  PSY_ineq (pred_opportunity_response)
        opportunity_bb_response[2,b] =  ((PSY_ineq (pred_opportunity_response) - PSY_ineq (pred_response))/PSY_ineq (pred_response))
        opportunity_bb_response[3,b] =  erreygers_ineq (pred_opportunity_response)
        opportunity_bb_response[4,b] =  ((erreygers_ineq (pred_opportunity_response) - erreygers_ineq (pred_response))/erreygers_ineq (pred_response))
        opportunity_bb_response[5,b] =   GSC_ineq (pred_opportunity_response)
        opportunity_bb_response[6,b] =  ((GSC_ineq (pred_opportunity_response) - GSC_ineq (pred_response))/GSC_ineq (pred_response))
        opportunity_bb_conditional[1,b] =  PSY_ineq (pred_opportunity_conditional)
        opportunity_bb_conditional[2,b] =  ((PSY_ineq (pred_opportunity_conditional) - PSY_ineq (pred_conditional))/PSY_ineq (pred_conditional))
        opportunity_bb_conditional[3,b] =  erreygers_ineq (pred_opportunity_conditional)
        opportunity_bb_conditional[4,b] =  ((erreygers_ineq (pred_opportunity_conditional) - erreygers_ineq (pred_conditional))/erreygers_ineq (pred_conditional))
        opportunity_bb_conditional[5,b] =   GSC_ineq (pred_opportunity_conditional)
        opportunity_bb_conditional[6,b] =  ((GSC_ineq (pred_opportunity_conditional) - GSC_ineq (pred_conditional))/GSC_ineq (pred_conditional))
        
        preferences_bb_zi[1,b] =  PSY_ineq (pred_preferences_zi)
        preferences_bb_zi[2,b] =  ((PSY_ineq (pred_preferences_zi) - PSY_ineq (pred_zi))/PSY_ineq (pred_zi))
        preferences_bb_zi[3,b] =  erreygers_ineq (pred_preferences_zi)
        preferences_bb_zi[4,b] =  ((erreygers_ineq (pred_preferences_zi) - erreygers_ineq (pred_zi))/erreygers_ineq (pred_zi))
        preferences_bb_zi[5,b] =   GSC_ineq (pred_preferences_zi)
        preferences_bb_zi[6,b] =  ((GSC_ineq (pred_preferences_zi) - GSC_ineq (pred_zi))/GSC_ineq (pred_zi))
        preferences_bb_response[1,b] =  PSY_ineq (pred_preferences_response)
        preferences_bb_response[2,b] =  ((PSY_ineq (pred_preferences_response) - PSY_ineq (pred_response))/PSY_ineq (pred_response))
        preferences_bb_response[3,b] =  erreygers_ineq (pred_preferences_response)
        preferences_bb_response[4,b] =  ((erreygers_ineq (pred_preferences_response) - erreygers_ineq (pred_response))/erreygers_ineq (pred_response))
        preferences_bb_response[5,b] =   GSC_ineq (pred_preferences_response)
        preferences_bb_response[6,b] =  ((GSC_ineq (pred_preferences_response) - GSC_ineq (pred_response))/GSC_ineq (pred_response))
        preferences_bb_conditional[1,b] =  PSY_ineq (pred_preferences_conditional)
        preferences_bb_conditional[2,b] =  ((PSY_ineq (pred_preferences_conditional) - PSY_ineq (pred_conditional))/PSY_ineq (pred_conditional))
        preferences_bb_conditional[3,b] =  erreygers_ineq (pred_preferences_conditional)
        preferences_bb_conditional[4,b] =  ((erreygers_ineq (pred_preferences_conditional) - erreygers_ineq (pred_conditional))/erreygers_ineq (pred_conditional))
        preferences_bb_conditional[5,b] =   GSC_ineq (pred_preferences_conditional)
        preferences_bb_conditional[6,b] =  ((GSC_ineq (pred_preferences_conditional) - GSC_ineq (pred_conditional))/GSC_ineq (pred_conditional))
        
        fitted_bb_zi[1,b] =  PSY_ineq (pred_zi)
        fitted_bb_zi[2,b] =  ((PSY_ineq (pred_zi) - PSY_ineq (pred_zi))/PSY_ineq (pred_zi))
        fitted_bb_zi[3,b] =  erreygers_ineq (pred_zi)
        fitted_bb_zi[4,b] =  ((erreygers_ineq (pred_zi) - erreygers_ineq (pred_zi))/erreygers_ineq (pred_zi))
        fitted_bb_zi[5,b] =   GSC_ineq (pred_zi)
        fitted_bb_zi[6,b] =  ((GSC_ineq (pred_zi) - GSC_ineq (pred_zi))/GSC_ineq (pred_zi))
        fitted_bb_response[1,b] =  PSY_ineq (pred_response)
        fitted_bb_response[2,b] =  ((PSY_ineq (pred_response) - PSY_ineq (pred_response))/PSY_ineq (pred_response))
        fitted_bb_response[3,b] =  erreygers_ineq (pred_response)
        fitted_bb_response[4,b] =  ((erreygers_ineq (pred_response) - erreygers_ineq (pred_response))/erreygers_ineq (pred_response))
        fitted_bb_response[5,b] =   GSC_ineq (pred_response)
        fitted_bb_response[6,b] =  ((GSC_ineq (pred_response) - GSC_ineq (pred_response))/GSC_ineq (pred_response))
        fitted_bb_conditional[1,b] =  PSY_ineq (pred_conditional)
        fitted_bb_conditional[2,b] =  ((PSY_ineq (pred_conditional) - PSY_ineq (pred_conditional))/PSY_ineq (pred_conditional))
        fitted_bb_conditional[3,b] =  erreygers_ineq (pred_conditional)
        fitted_bb_conditional[4,b] =  ((erreygers_ineq (pred_conditional) - erreygers_ineq (pred_conditional))/erreygers_ineq (pred_conditional))
        fitted_bb_conditional[5,b] =   GSC_ineq (pred_conditional)
        fitted_bb_conditional[6,b] =  ((GSC_ineq (pred_conditional) - GSC_ineq (pred_conditional))/GSC_ineq (pred_conditional))
      }
        
      # Save mean bootstrap estimates in core dataframe. 
      coefficients_zi = rowMeans(as.matrix(coefficients_bb_zi), na.rm = TRUE)
      coefficients_cond = rowMeans(as.matrix(coefficients_bb_cond), na.rm = TRUE)
      coefficient_range_zi[,counter] = coefficients_zi
      coefficient_range_cond[,counter] = coefficients_cond
      
      # Save inequality results in the initialised vectors
      opportunity_range_zi[1,counter] =  mean(as.numeric(opportunity_bb_zi[1,]), na.rm = TRUE)
      opportunity_range_zi[2,counter] =  mean(as.numeric(opportunity_bb_zi[2,]), na.rm = TRUE)
      opportunity_range_zi[3,counter] =  mean(as.numeric(opportunity_bb_zi[3,]), na.rm = TRUE)
      opportunity_range_zi[4,counter] =  mean(as.numeric(opportunity_bb_zi[4,]), na.rm = TRUE)
      opportunity_range_zi[5,counter] =  mean(as.numeric(opportunity_bb_zi[5,]), na.rm = TRUE)
      opportunity_range_zi[6,counter] =  mean(as.numeric(opportunity_bb_zi[6,]), na.rm = TRUE)
      opportunity_range_response[1,counter] =  mean(as.numeric(opportunity_bb_response[1,]), na.rm = TRUE)
      opportunity_range_response[2,counter] =  mean(as.numeric(opportunity_bb_response[2,]), na.rm = TRUE)
      opportunity_range_response[3,counter] =  mean(as.numeric(opportunity_bb_response[3,]), na.rm = TRUE)
      opportunity_range_response[4,counter] =  mean(as.numeric(opportunity_bb_response[4,]), na.rm = TRUE)
      opportunity_range_response[5,counter] =  mean(as.numeric(opportunity_bb_response[5,]), na.rm = TRUE)
      opportunity_range_response[6,counter] =  mean(as.numeric(opportunity_bb_response[6,]), na.rm = TRUE)
      opportunity_range_conditional[1,counter] =  mean(as.numeric(opportunity_bb_conditional[1,]), na.rm = TRUE)
      opportunity_range_conditional[2,counter] =  mean(as.numeric(opportunity_bb_conditional[2,]), na.rm = TRUE)
      opportunity_range_conditional[3,counter] =  mean(as.numeric(opportunity_bb_conditional[3,]), na.rm = TRUE)
      opportunity_range_conditional[4,counter] =  mean(as.numeric(opportunity_bb_conditional[4,]), na.rm = TRUE)
      opportunity_range_conditional[5,counter] =  mean(as.numeric(opportunity_bb_conditional[5,]), na.rm = TRUE)
      opportunity_range_conditional[6,counter] =  mean(as.numeric(opportunity_bb_conditional[6,]), na.rm = TRUE)
      
      preferences_range_zi[1,counter] =  mean(as.numeric(preferences_bb_zi[1,]), na.rm = TRUE)
      preferences_range_zi[2,counter] =  mean(as.numeric(preferences_bb_zi[2,]), na.rm = TRUE)
      preferences_range_zi[3,counter] =  mean(as.numeric(preferences_bb_zi[3,]), na.rm = TRUE)
      preferences_range_zi[4,counter] =  mean(as.numeric(preferences_bb_zi[4,]), na.rm = TRUE)
      preferences_range_zi[5,counter] =  mean(as.numeric(preferences_bb_zi[5,]), na.rm = TRUE)
      preferences_range_zi[6,counter] =  mean(as.numeric(preferences_bb_zi[6,]), na.rm = TRUE)
      preferences_range_response[1,counter] =  mean(as.numeric(preferences_bb_response[1,]), na.rm = TRUE)
      preferences_range_response[2,counter] =  mean(as.numeric(preferences_bb_response[2,]), na.rm = TRUE)
      preferences_range_response[3,counter] =  mean(as.numeric(preferences_bb_response[3,]), na.rm = TRUE)
      preferences_range_response[4,counter] =  mean(as.numeric(preferences_bb_response[4,]), na.rm = TRUE)
      preferences_range_response[5,counter] =  mean(as.numeric(preferences_bb_response[5,]), na.rm = TRUE)
      preferences_range_response[6,counter] =  mean(as.numeric(preferences_bb_response[6,]), na.rm = TRUE)
      preferences_range_conditional[1,counter] =  mean(as.numeric(preferences_bb_conditional[1,]), na.rm = TRUE)
      preferences_range_conditional[2,counter] =  mean(as.numeric(preferences_bb_conditional[2,]), na.rm = TRUE)
      preferences_range_conditional[3,counter] =  mean(as.numeric(preferences_bb_conditional[3,]), na.rm = TRUE)
      preferences_range_conditional[4,counter] =  mean(as.numeric(preferences_bb_conditional[4,]), na.rm = TRUE)
      preferences_range_conditional[5,counter] =  mean(as.numeric(preferences_bb_conditional[5,]), na.rm = TRUE)
      preferences_range_conditional[6,counter] =  mean(as.numeric(preferences_bb_conditional[6,]), na.rm = TRUE)
      
      fitted_range_zi[1,counter] =  mean(as.numeric(fitted_bb_zi[1,]), na.rm = TRUE)
      fitted_range_zi[2,counter] =  mean(as.numeric(fitted_bb_zi[2,]), na.rm = TRUE)
      fitted_range_zi[3,counter] =  mean(as.numeric(fitted_bb_zi[3,]), na.rm = TRUE)
      fitted_range_zi[4,counter] =  mean(as.numeric(fitted_bb_zi[4,]), na.rm = TRUE)
      fitted_range_zi[5,counter] =  mean(as.numeric(fitted_bb_zi[5,]), na.rm = TRUE)
      fitted_range_zi[6,counter] =  mean(as.numeric(fitted_bb_zi[6,]), na.rm = TRUE)
      fitted_range_response[1,counter] =  mean(as.numeric(fitted_bb_response[1,]), na.rm = TRUE)
      fitted_range_response[2,counter] =  mean(as.numeric(fitted_bb_response[2,]), na.rm = TRUE)
      fitted_range_response[3,counter] =  mean(as.numeric(fitted_bb_response[3,]), na.rm = TRUE)
      fitted_range_response[4,counter] =  mean(as.numeric(fitted_bb_response[4,]), na.rm = TRUE)
      fitted_range_response[5,counter] =  mean(as.numeric(fitted_bb_response[5,]), na.rm = TRUE)
      fitted_range_response[6,counter] =  mean(as.numeric(fitted_bb_response[6,]), na.rm = TRUE)
      fitted_range_conditional[1,counter] =  mean(as.numeric(fitted_bb_conditional[1,]), na.rm = TRUE)
      fitted_range_conditional[2,counter] =  mean(as.numeric(fitted_bb_conditional[2,]), na.rm = TRUE)
      fitted_range_conditional[3,counter] =  mean(as.numeric(fitted_bb_conditional[3,]), na.rm = TRUE)
      fitted_range_conditional[4,counter] =  mean(as.numeric(fitted_bb_conditional[4,]), na.rm = TRUE)
      fitted_range_conditional[5,counter] =  mean(as.numeric(fitted_bb_conditional[5,]), na.rm = TRUE)
      fitted_range_conditional[6,counter] =  mean(as.numeric(fitted_bb_conditional[6,]), na.rm = TRUE)
    }
  }
  

  # Remove columns with NA values
  coefficient_range_zi = coefficient_range_zi[ , colSums(is.na(coefficient_range_zi))==0]
  coefficient_range_cond = coefficient_range_cond[ , colSums(is.na(coefficient_range_cond))==0]
  opportunity_range_zi = opportunity_range_zi[ , colSums(is.na(opportunity_range_zi))==0]
  opportunity_range_response = opportunity_range_response[ , colSums(is.na(opportunity_range_response))==0]
  opportunity_range_conditional = opportunity_range_conditional[ , colSums(is.na(opportunity_range_conditional))==0]
  preferences_range_zi = preferences_range_zi[ , colSums(is.na(preferences_range_zi))==0]
  preferences_range_response = preferences_range_response[ , colSums(is.na(preferences_range_response))==0]
  preferences_range_conditional = preferences_range_conditional[ , colSums(is.na(preferences_range_conditional))==0]
  fitted_range_zi = fitted_range_zi[ , colSums(is.na(fitted_range_zi))==0]
  fitted_range_response = fitted_range_response[ , colSums(is.na(fitted_range_response))==0]
  fitted_range_conditional = fitted_range_conditional[ , colSums(is.na(fitted_range_conditional))==0]

  # Save confidence intervals for coefficients
  coefficient_intervals_zi = as.data.frame(matrix(NA, nrow = length(coefficient_vec)+2, ncol = 2))
  rownames(coefficient_intervals_zi) = c("intercept", coefficient_vec, "OV")
  colnames(coefficient_intervals_zi) = c("left_95", "right_95")
  for (r in 1:nrow(coefficient_range_zi)){
  test  = t.test(coefficient_range_zi[r,])
  coefficient_intervals_zi[r,1] = test$conf.int[1]
  coefficient_intervals_zi[r,2]  = test$conf.int[2]
  }
  
  coefficient_intervals_cond = as.data.frame(matrix(NA, nrow = length(coefficient_vec)+2, ncol = 2))
  rownames(coefficient_intervals_cond) = c("intercept", coefficient_vec, "OV")
  colnames(coefficient_intervals_cond) = c("left_95", "right_95")
  for (r in 1:nrow(coefficient_range_cond)){
    test  = t.test(coefficient_range_cond[r,])
    coefficient_intervals_cond[r,1] = test$conf.int[1]
    coefficient_intervals_cond[r,2]  = test$conf.int[2]
  }
  
  # Save confidence intervals for inequality indices
  opportunity_intervals_zi = preferences_intervals_zi = fitted_intervals_zi =  as.data.frame(matrix(NA, nrow = 6, ncol = 2))
  rownames(opportunity_intervals_zi) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(preferences_intervals_zi) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(fitted_intervals_zi) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  colnames(opportunity_intervals_zi) = c("left_95", "right_95")
  colnames(preferences_intervals_zi)= c("left_95", "right_95")
  colnames(fitted_intervals_zi)= c("left_95", "right_95")
  opportunity_intervals_conditional = opportunity_intervals_response = opportunity_intervals_zi
  preferences_intervals_conditional = preferences_intervals_response = preferences_intervals_zi
  fitted_intervals_conditional = fitted_intervals_response = fitted_intervals_zi
 
  for (r in 1:6){
    test  = t.test(opportunity_range_zi[r,])
    opportunity_intervals_zi[r,1] = test$conf.int[1]
    opportunity_intervals_zi[r,2]  = test$conf.int[2]
    test  = t.test(preferences_range_zi[r,])
    preferences_intervals_zi[r,1] = test$conf.int[1]
    preferences_intervals_zi[r,2]  = test$conf.int[2]
    test  = t.test(fitted_range_zi[r,])
    fitted_intervals_zi[r,1] = test$conf.int[1]
    fitted_intervals_zi[r,2]  = test$conf.int[2]
  }
  
   for (r in 1:6){
    test  = t.test(opportunity_range_response[r,])
    opportunity_intervals_response[r,1] = test$conf.int[1]
    opportunity_intervals_response[r,2]  = test$conf.int[2]
    test  = t.test(preferences_range_response[r,])
    preferences_intervals_response[r,1] = test$conf.int[1]
    preferences_intervals_response[r,2]  = test$conf.int[2]
    test  = t.test(fitted_range_response[r,])
    fitted_intervals_response[r,1] = test$conf.int[1]
    fitted_intervals_response[r,2]  = test$conf.int[2]
   }
  
  for (r in 1:6){
    test  = t.test(opportunity_range_conditional[r,])
    opportunity_intervals_conditional[r,1] = test$conf.int[1]
    opportunity_intervals_conditional[r,2]  = test$conf.int[2]
    test  = t.test(preferences_range_conditional[r,])
    preferences_intervals_conditional[r,1] = test$conf.int[1]
    preferences_intervals_conditional[r,2]  = test$conf.int[2]
    test  = t.test(fitted_range_conditional[r,])
    fitted_intervals_conditional[r,1] = test$conf.int[1]
    fitted_intervals_conditional[r,2]  = test$conf.int[2]
  }
  
  #write csv files with ranges. 
  write.csv(coefficient_intervals_zi, file = here("Results", "coefficient_bias_intervals_hh_continuous_full_zeroinflated.csv"))
  write.csv(coefficient_intervals_cond, file = here("Results", "coefficient_bias_intervals_hh_continuous_full_conditional.csv"))
  write.csv(opportunity_intervals_zi, file = here("Results", "opportunity_ineq_bias_intervals_hh_continuous_full_zi.csv"))
  write.csv(opportunity_intervals_response, file = here("Results", "opportunity_ineq_bias_intervals_hh_continuous_full_response.csv"))
  write.csv(opportunity_intervals_conditional, file = here("Results", "opportunity_ineq_bias_intervals_hh_continuous_full_conditional.csv"))
  write.csv(preferences_intervals_zi, file = here("Results", "preference_ineq_bias_intervals_hh_continuous_full_zi.csv"))
  write.csv(preferences_intervals_response, file = here("Results", "preference_ineq_bias_intervals_hh_continuous_full_response.csv"))
  write.csv(preferences_intervals_conditional, file = here("Results", "preference_ineq_bias_intervals_hh_continuous_full_conditional.csv"))
  write.csv(fitted_intervals_zi, file = here("Results", "fitted_ineq_bias_intervals_hh_continuous_full_zi.csv"))
  write.csv(fitted_intervals_response, file = here("Results", "fitted_ineq_bias_intervals_hh_continuous_full_response.csv"))
  write.csv(fitted_intervals_conditional, file = here("Results", "fitted_ineq_bias_intervals_hh_continuous_full_conditional.csv"))
  
  ##########################
  #### HH REDUCED MODEL ####
  ##########################
  
  coefficient_vec = c(C_hh, V_hh)
  coef_opportunity = C_hh
  coef_preferences = V_hh
  
  # Initialise dataframes. 
  coefficient_range_zi = as.data.frame(matrix(NA, ncol = n_samples * length(coefficient_vec), nrow = length(coefficient_vec)+2))
  rownames(coefficient_range_zi) = c("intercept", coefficient_vec, "OV")
  coefficient_range_cond =  coefficient_range_zi
  
  opportunity_range_zi = preferences_range_zi = fitted_range_zi = as.data.frame(matrix(NA, ncol = n_samples * length(coefficient_vec), nrow = 6))
  rownames(opportunity_range_zi) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(preferences_range_zi) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(fitted_range_zi) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  opportunity_range_conditional = opportunity_range_response = opportunity_range_zi
  preferences_range_conditional = preferences_range_response = preferences_range_zi
  fitted_range_conditional = fitted_range_response = fitted_range_zi
  
  #Equations for the model
  eq_hh_reduced =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(V_hh, collapse = " + "), 'OV', sep = " + "))
  eq_hh_zero = paste(" ~ ", paste(paste(C_hh, collapse = " + "), paste(V_hh, collapse = " + "), 'OV', sep = " + "), sep = "")
  eq_hh_reduced = as.formula(eq_hh_reduced)
  eq_hh_zero = as.formula(eq_hh_zero)
  
  ### Create new data frames equalising preferences, and equalising circumstances. 
  hh_data_cbar = hh_data_pbar = hh_data
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
      
      counter = counter+1
      
      # Reset bootstrap matrices
      coefficients_bb_zi = coefficients_bb_cond = as.data.frame(matrix(NA, ncol = n_bootstraps , nrow = length(coefficient_vec)+2))
      opportunity_bb_zi = preferences_bb_zi = fitted_bb_zi = as.data.frame(matrix(NA, ncol = n_bootstraps , nrow = 6))
      opportunity_bb_response = preferences_bb_response = fitted_bb_response  = as.data.frame(matrix(NA, ncol = n_bootstraps , nrow = 6))
      opportunity_bb_conditional = preferences_bb_conditional = fitted_bb_conditional = as.data.frame(matrix(NA, ncol = n_bootstraps , nrow = 6))
      
      #Generate omitted variable, and skip if correlation not possible. 
      OV = ov_generate(y, rho[n,], OV_base)
      if (all(OV == 0)){
        next
      }
      
      hh_data$OV = OV
      hh_data_cbar$OV = OV
      hh_data_pbar$OV = OV
      
      for (b in 1:n_bootstraps) {
        
        #Draw bootstrap sample
        min_outcomes_condition  = FALSE
        while (min_outcomes_condition == FALSE){
          bootstrap_indices = sample(1:nrow(hh_data), size = nrow(hh_data), replace = TRUE)
          bootstrap_sample = hh_data[bootstrap_indices, ]
          if (length(bootstrap_sample$BINARY_PARTICIPATION[bootstrap_sample$BINARY_PARTICIPATION > 0]) > min_outcomes) {
            # If condition is met, set the flag to True to exit the loop
            min_outcomes_condition = TRUE
          }
        }
        
        #Estimate the model and retrieve the coefficients. 
        model_hh_reduced = glmmTMB(eq_hh_reduced, data = bootstrap_sample, family = beta_family(link = "logit"), ziformula = eq_hh_zero, dispformula = ~ 1)
        model_summary = summary(model_hh_reduced)
        
        if (model_hh_reduced$fit$convergence != 0){ #failed to converge
          next
        }
        
        #Test VIF and skip if results are unstable.
        #Test this using 2 distinct models as VIFG functions for zero inflated beta hard to do
        eq_hh_reduced_zi = paste("BINARY_PARTICIPATION", sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(V_hh, collapse = " + "),  'OV', sep = " + "))
        zi_model_hh_reduced_VIF = glm(eq_hh_reduced_zi, data = bootstrap_sample, family = binomial(link = "logit"))
        VIF_zi = glmtoolbox::gvif(zi_model_hh_reduced_VIF, verbose = FALSE)
        if (any(is.na(VIF_zi))){
          next
        } else if (max(VIF_zi)> threshold){
          next
        }
        
        #Test continuous using poisson distribution (for simplicity just to detect multicollinearity)
        cond_model_hh_reduced_VIF = glm(eq_hh_reduced_zi, data = subset(bootstrap_sample, bootstrap_sample$CONTINUOUS_PARTICIPATION>0 , family = poisson(link = "log")))
        VIF_cond = glmtoolbox::gvif(cond_model_hh_reduced_VIF, verbose = FALSE)
        if (any(is.na(VIF_cond))){
          next
        } else if (max(VIF_cond)> threshold_2){
          next
        }
        
        #Save coefficients into data structure.
        coefficients_bb_zi[,b] = as.numeric(model_summary$coefficients$zi[,1])
        coefficients_bb_cond[,b] = as.numeric(model_summary$coefficients$cond[,1])
        
        # Now estimate inequality indices and save in dataframe
        pred_opportunity_zi = predict(model_hh_reduced, newdata = hh_data_pbar, type = 'zprob')
        pred_opportunity_response = predict(model_hh_reduced, newdata = hh_data_pbar, type = 'response')
        pred_opportunity_conditional = predict(model_hh_reduced, newdata = hh_data_pbar, type = 'conditional')
        
        pred_preferences_zi = predict(model_hh_reduced, newdata = hh_data_cbar, type = 'zprob')
        pred_preferences_response = predict(model_hh_reduced, newdata = hh_data_cbar, type = 'response')
        pred_preferences_conditional = predict(model_hh_reduced, newdata = hh_data_cbar, type = 'conditional')
        
        pred_zi = predict(model_hh_reduced, newdata = hh_data, type = 'zprob')
        pred_response = predict(model_hh_reduced, newdata = hh_data, type = 'response')
        pred_conditional = predict(model_hh_reduced, newdata = hh_data, type = 'conditional')
        
        # Save inequality results in the initialised vectors
        opportunity_bb_zi[1,b] =  PSY_ineq (pred_opportunity_zi)
        opportunity_bb_zi[2,b] =  ((PSY_ineq (pred_opportunity_zi) - PSY_ineq (pred_zi))/PSY_ineq (pred_zi))
        opportunity_bb_zi[3,b] =  erreygers_ineq (pred_opportunity_zi)
        opportunity_bb_zi[4,b] =  ((erreygers_ineq (pred_opportunity_zi) - erreygers_ineq (pred_zi))/erreygers_ineq (pred_zi))
        opportunity_bb_zi[5,b] =   GSC_ineq (pred_opportunity_zi)
        opportunity_bb_zi[6,b] =  ((GSC_ineq (pred_opportunity_zi) - GSC_ineq (pred_zi))/GSC_ineq (pred_zi))
        opportunity_bb_response[1,b] =  PSY_ineq (pred_opportunity_response)
        opportunity_bb_response[2,b] =  ((PSY_ineq (pred_opportunity_response) - PSY_ineq (pred_response))/PSY_ineq (pred_response))
        opportunity_bb_response[3,b] =  erreygers_ineq (pred_opportunity_response)
        opportunity_bb_response[4,b] =  ((erreygers_ineq (pred_opportunity_response) - erreygers_ineq (pred_response))/erreygers_ineq (pred_response))
        opportunity_bb_response[5,b] =   GSC_ineq (pred_opportunity_response)
        opportunity_bb_response[6,b] =  ((GSC_ineq (pred_opportunity_response) - GSC_ineq (pred_response))/GSC_ineq (pred_response))
        opportunity_bb_conditional[1,b] =  PSY_ineq (pred_opportunity_conditional)
        opportunity_bb_conditional[2,b] =  ((PSY_ineq (pred_opportunity_conditional) - PSY_ineq (pred_conditional))/PSY_ineq (pred_conditional))
        opportunity_bb_conditional[3,b] =  erreygers_ineq (pred_opportunity_conditional)
        opportunity_bb_conditional[4,b] =  ((erreygers_ineq (pred_opportunity_conditional) - erreygers_ineq (pred_conditional))/erreygers_ineq (pred_conditional))
        opportunity_bb_conditional[5,b] =   GSC_ineq (pred_opportunity_conditional)
        opportunity_bb_conditional[6,b] =  ((GSC_ineq (pred_opportunity_conditional) - GSC_ineq (pred_conditional))/GSC_ineq (pred_conditional))
        
        preferences_bb_zi[1,b] =  PSY_ineq (pred_preferences_zi)
        preferences_bb_zi[2,b] =  ((PSY_ineq (pred_preferences_zi) - PSY_ineq (pred_zi))/PSY_ineq (pred_zi))
        preferences_bb_zi[3,b] =  erreygers_ineq (pred_preferences_zi)
        preferences_bb_zi[4,b] =  ((erreygers_ineq (pred_preferences_zi) - erreygers_ineq (pred_zi))/erreygers_ineq (pred_zi))
        preferences_bb_zi[5,b] =   GSC_ineq (pred_preferences_zi)
        preferences_bb_zi[6,b] =  ((GSC_ineq (pred_preferences_zi) - GSC_ineq (pred_zi))/GSC_ineq (pred_zi))
        preferences_bb_response[1,b] =  PSY_ineq (pred_preferences_response)
        preferences_bb_response[2,b] =  ((PSY_ineq (pred_preferences_response) - PSY_ineq (pred_response))/PSY_ineq (pred_response))
        preferences_bb_response[3,b] =  erreygers_ineq (pred_preferences_response)
        preferences_bb_response[4,b] =  ((erreygers_ineq (pred_preferences_response) - erreygers_ineq (pred_response))/erreygers_ineq (pred_response))
        preferences_bb_response[5,b] =   GSC_ineq (pred_preferences_response)
        preferences_bb_response[6,b] =  ((GSC_ineq (pred_preferences_response) - GSC_ineq (pred_response))/GSC_ineq (pred_response))
        preferences_bb_conditional[1,b] =  PSY_ineq (pred_preferences_conditional)
        preferences_bb_conditional[2,b] =  ((PSY_ineq (pred_preferences_conditional) - PSY_ineq (pred_conditional))/PSY_ineq (pred_conditional))
        preferences_bb_conditional[3,b] =  erreygers_ineq (pred_preferences_conditional)
        preferences_bb_conditional[4,b] =  ((erreygers_ineq (pred_preferences_conditional) - erreygers_ineq (pred_conditional))/erreygers_ineq (pred_conditional))
        preferences_bb_conditional[5,b] =   GSC_ineq (pred_preferences_conditional)
        preferences_bb_conditional[6,b] =  ((GSC_ineq (pred_preferences_conditional) - GSC_ineq (pred_conditional))/GSC_ineq (pred_conditional))
        
        fitted_bb_zi[1,b] =  PSY_ineq (pred_zi)
        fitted_bb_zi[2,b] =  ((PSY_ineq (pred_zi) - PSY_ineq (pred_zi))/PSY_ineq (pred_zi))
        fitted_bb_zi[3,b] =  erreygers_ineq (pred_zi)
        fitted_bb_zi[4,b] =  ((erreygers_ineq (pred_zi) - erreygers_ineq (pred_zi))/erreygers_ineq (pred_zi))
        fitted_bb_zi[5,b] =   GSC_ineq (pred_zi)
        fitted_bb_zi[6,b] =  ((GSC_ineq (pred_zi) - GSC_ineq (pred_zi))/GSC_ineq (pred_zi))
        fitted_bb_response[1,b] =  PSY_ineq (pred_response)
        fitted_bb_response[2,b] =  ((PSY_ineq (pred_response) - PSY_ineq (pred_response))/PSY_ineq (pred_response))
        fitted_bb_response[3,b] =  erreygers_ineq (pred_response)
        fitted_bb_response[4,b] =  ((erreygers_ineq (pred_response) - erreygers_ineq (pred_response))/erreygers_ineq (pred_response))
        fitted_bb_response[5,b] =   GSC_ineq (pred_response)
        fitted_bb_response[6,b] =  ((GSC_ineq (pred_response) - GSC_ineq (pred_response))/GSC_ineq (pred_response))
        fitted_bb_conditional[1,b] =  PSY_ineq (pred_conditional)
        fitted_bb_conditional[2,b] =  ((PSY_ineq (pred_conditional) - PSY_ineq (pred_conditional))/PSY_ineq (pred_conditional))
        fitted_bb_conditional[3,b] =  erreygers_ineq (pred_conditional)
        fitted_bb_conditional[4,b] =  ((erreygers_ineq (pred_conditional) - erreygers_ineq (pred_conditional))/erreygers_ineq (pred_conditional))
        fitted_bb_conditional[5,b] =   GSC_ineq (pred_conditional)
        fitted_bb_conditional[6,b] =  ((GSC_ineq (pred_conditional) - GSC_ineq (pred_conditional))/GSC_ineq (pred_conditional))
      }
      
      # Save mean bootstrap estimates in core dataframe. 
      coefficients_zi = rowMeans(as.matrix(coefficients_bb_zi), na.rm = TRUE)
      coefficients_cond = rowMeans(as.matrix(coefficients_bb_cond), na.rm = TRUE)
      coefficient_range_zi[,counter] = coefficients_zi
      coefficient_range_cond[,counter] = coefficients_cond
      
      # Save inequality results in the initialised vectors
      opportunity_range_zi[1,counter] =  mean(as.numeric(opportunity_bb_zi[1,]), na.rm = TRUE)
      opportunity_range_zi[2,counter] =  mean(as.numeric(opportunity_bb_zi[2,]), na.rm = TRUE)
      opportunity_range_zi[3,counter] =  mean(as.numeric(opportunity_bb_zi[3,]), na.rm = TRUE)
      opportunity_range_zi[4,counter] =  mean(as.numeric(opportunity_bb_zi[4,]), na.rm = TRUE)
      opportunity_range_zi[5,counter] =  mean(as.numeric(opportunity_bb_zi[5,]), na.rm = TRUE)
      opportunity_range_zi[6,counter] =  mean(as.numeric(opportunity_bb_zi[6,]), na.rm = TRUE)
      opportunity_range_response[1,counter] =  mean(as.numeric(opportunity_bb_response[1,]), na.rm = TRUE)
      opportunity_range_response[2,counter] =  mean(as.numeric(opportunity_bb_response[2,]), na.rm = TRUE)
      opportunity_range_response[3,counter] =  mean(as.numeric(opportunity_bb_response[3,]), na.rm = TRUE)
      opportunity_range_response[4,counter] =  mean(as.numeric(opportunity_bb_response[4,]), na.rm = TRUE)
      opportunity_range_response[5,counter] =  mean(as.numeric(opportunity_bb_response[5,]), na.rm = TRUE)
      opportunity_range_response[6,counter] =  mean(as.numeric(opportunity_bb_response[6,]), na.rm = TRUE)
      opportunity_range_conditional[1,counter] =  mean(as.numeric(opportunity_bb_conditional[1,]), na.rm = TRUE)
      opportunity_range_conditional[2,counter] =  mean(as.numeric(opportunity_bb_conditional[2,]), na.rm = TRUE)
      opportunity_range_conditional[3,counter] =  mean(as.numeric(opportunity_bb_conditional[3,]), na.rm = TRUE)
      opportunity_range_conditional[4,counter] =  mean(as.numeric(opportunity_bb_conditional[4,]), na.rm = TRUE)
      opportunity_range_conditional[5,counter] =  mean(as.numeric(opportunity_bb_conditional[5,]), na.rm = TRUE)
      opportunity_range_conditional[6,counter] =  mean(as.numeric(opportunity_bb_conditional[6,]), na.rm = TRUE)
      
      preferences_range_zi[1,counter] =  mean(as.numeric(preferences_bb_zi[1,]), na.rm = TRUE)
      preferences_range_zi[2,counter] =  mean(as.numeric(preferences_bb_zi[2,]), na.rm = TRUE)
      preferences_range_zi[3,counter] =  mean(as.numeric(preferences_bb_zi[3,]), na.rm = TRUE)
      preferences_range_zi[4,counter] =  mean(as.numeric(preferences_bb_zi[4,]), na.rm = TRUE)
      preferences_range_zi[5,counter] =  mean(as.numeric(preferences_bb_zi[5,]), na.rm = TRUE)
      preferences_range_zi[6,counter] =  mean(as.numeric(preferences_bb_zi[6,]), na.rm = TRUE)
      preferences_range_response[1,counter] =  mean(as.numeric(preferences_bb_response[1,]), na.rm = TRUE)
      preferences_range_response[2,counter] =  mean(as.numeric(preferences_bb_response[2,]), na.rm = TRUE)
      preferences_range_response[3,counter] =  mean(as.numeric(preferences_bb_response[3,]), na.rm = TRUE)
      preferences_range_response[4,counter] =  mean(as.numeric(preferences_bb_response[4,]), na.rm = TRUE)
      preferences_range_response[5,counter] =  mean(as.numeric(preferences_bb_response[5,]), na.rm = TRUE)
      preferences_range_response[6,counter] =  mean(as.numeric(preferences_bb_response[6,]), na.rm = TRUE)
      preferences_range_conditional[1,counter] =  mean(as.numeric(preferences_bb_conditional[1,]), na.rm = TRUE)
      preferences_range_conditional[2,counter] =  mean(as.numeric(preferences_bb_conditional[2,]), na.rm = TRUE)
      preferences_range_conditional[3,counter] =  mean(as.numeric(preferences_bb_conditional[3,]), na.rm = TRUE)
      preferences_range_conditional[4,counter] =  mean(as.numeric(preferences_bb_conditional[4,]), na.rm = TRUE)
      preferences_range_conditional[5,counter] =  mean(as.numeric(preferences_bb_conditional[5,]), na.rm = TRUE)
      preferences_range_conditional[6,counter] =  mean(as.numeric(preferences_bb_conditional[6,]), na.rm = TRUE)
      
      fitted_range_zi[1,counter] =  mean(as.numeric(fitted_bb_zi[1,]), na.rm = TRUE)
      fitted_range_zi[2,counter] =  mean(as.numeric(fitted_bb_zi[2,]), na.rm = TRUE)
      fitted_range_zi[3,counter] =  mean(as.numeric(fitted_bb_zi[3,]), na.rm = TRUE)
      fitted_range_zi[4,counter] =  mean(as.numeric(fitted_bb_zi[4,]), na.rm = TRUE)
      fitted_range_zi[5,counter] =  mean(as.numeric(fitted_bb_zi[5,]), na.rm = TRUE)
      fitted_range_zi[6,counter] =  mean(as.numeric(fitted_bb_zi[6,]), na.rm = TRUE)
      fitted_range_response[1,counter] =  mean(as.numeric(fitted_bb_response[1,]), na.rm = TRUE)
      fitted_range_response[2,counter] =  mean(as.numeric(fitted_bb_response[2,]), na.rm = TRUE)
      fitted_range_response[3,counter] =  mean(as.numeric(fitted_bb_response[3,]), na.rm = TRUE)
      fitted_range_response[4,counter] =  mean(as.numeric(fitted_bb_response[4,]), na.rm = TRUE)
      fitted_range_response[5,counter] =  mean(as.numeric(fitted_bb_response[5,]), na.rm = TRUE)
      fitted_range_response[6,counter] =  mean(as.numeric(fitted_bb_response[6,]), na.rm = TRUE)
      fitted_range_conditional[1,counter] =  mean(as.numeric(fitted_bb_conditional[1,]), na.rm = TRUE)
      fitted_range_conditional[2,counter] =  mean(as.numeric(fitted_bb_conditional[2,]), na.rm = TRUE)
      fitted_range_conditional[3,counter] =  mean(as.numeric(fitted_bb_conditional[3,]), na.rm = TRUE)
      fitted_range_conditional[4,counter] =  mean(as.numeric(fitted_bb_conditional[4,]), na.rm = TRUE)
      fitted_range_conditional[5,counter] =  mean(as.numeric(fitted_bb_conditional[5,]), na.rm = TRUE)
      fitted_range_conditional[6,counter] =  mean(as.numeric(fitted_bb_conditional[6,]), na.rm = TRUE)
    }
  }
  
  
  # Remove columns with NA values
  coefficient_range_zi = coefficient_range_zi[ , colSums(is.na(coefficient_range_zi))==0]
  coefficient_range_cond = coefficient_range_cond[ , colSums(is.na(coefficient_range_cond))==0]
  opportunity_range_zi = opportunity_range_zi[ , colSums(is.na(opportunity_range_zi))==0]
  opportunity_range_response = opportunity_range_response[ , colSums(is.na(opportunity_range_response))==0]
  opportunity_range_conditional = opportunity_range_conditional[ , colSums(is.na(opportunity_range_conditional))==0]
  preferences_range_zi = preferences_range_zi[ , colSums(is.na(preferences_range_zi))==0]
  preferences_range_response = preferences_range_response[ , colSums(is.na(preferences_range_response))==0]
  preferences_range_conditional = preferences_range_conditional[ , colSums(is.na(preferences_range_conditional))==0]
  fitted_range_zi = fitted_range_zi[ , colSums(is.na(fitted_range_zi))==0]
  fitted_range_response = fitted_range_response[ , colSums(is.na(fitted_range_response))==0]
  fitted_range_conditional = fitted_range_conditional[ , colSums(is.na(fitted_range_conditional))==0]
  
  # Save confidence intervals for coefficients
  coefficient_intervals_zi = as.data.frame(matrix(NA, nrow = length(coefficient_vec)+2, ncol = 2))
  rownames(coefficient_intervals_zi) = c("intercept", coefficient_vec, "OV")
  colnames(coefficient_intervals_zi) = c("left_95", "right_95")
  for (r in 1:nrow(coefficient_range_zi)){
    test  = t.test(coefficient_range_zi[r,])
    coefficient_intervals_zi[r,1] = test$conf.int[1]
    coefficient_intervals_zi[r,2]  = test$conf.int[2]
  }
  
  coefficient_intervals_cond = as.data.frame(matrix(NA, nrow = length(coefficient_vec)+2, ncol = 2))
  rownames(coefficient_intervals_cond) = c("intercept", coefficient_vec, "OV")
  colnames(coefficient_intervals_cond) = c("left_95", "right_95")
  for (r in 1:nrow(coefficient_range_cond)){
    test  = t.test(coefficient_range_cond[r,])
    coefficient_intervals_cond[r,1] = test$conf.int[1]
    coefficient_intervals_cond[r,2]  = test$conf.int[2]
  }
  
  # Save confidence intervals for inequality indices
  opportunity_intervals_zi = preferences_intervals_zi = fitted_intervals_zi =  as.data.frame(matrix(NA, nrow = 6, ncol = 2))
  rownames(opportunity_intervals_zi) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(preferences_intervals_zi) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  rownames(fitted_intervals_zi) = c("PSY_ABS", "PSY_REL", "ERR_ABS", "ERR_REL", "GSC_ABS", "GSC_REL")
  colnames(opportunity_intervals_zi) = c("left_95", "right_95")
  colnames(preferences_intervals_zi)= c("left_95", "right_95")
  colnames(fitted_intervals_zi)= c("left_95", "right_95")
  opportunity_intervals_conditional = opportunity_intervals_response = opportunity_intervals_zi
  preferences_intervals_conditional = preferences_intervals_response = preferences_intervals_zi
  fitted_intervals_conditional = fitted_intervals_response = fitted_intervals_zi
  
  for (r in 1:6){
    test  = t.test(opportunity_range_zi[r,])
    opportunity_intervals_zi[r,1] = test$conf.int[1]
    opportunity_intervals_zi[r,2]  = test$conf.int[2]
    test  = t.test(preferences_range_zi[r,])
    preferences_intervals_zi[r,1] = test$conf.int[1]
    preferences_intervals_zi[r,2]  = test$conf.int[2]
    test  = t.test(fitted_range_zi[r,])
    fitted_intervals_zi[r,1] = test$conf.int[1]
    fitted_intervals_zi[r,2]  = test$conf.int[2]
  }
  
  for (r in 1:6){
    test  = t.test(opportunity_range_response[r,])
    opportunity_intervals_response[r,1] = test$conf.int[1]
    opportunity_intervals_response[r,2]  = test$conf.int[2]
    test  = t.test(preferences_range_response[r,])
    preferences_intervals_response[r,1] = test$conf.int[1]
    preferences_intervals_response[r,2]  = test$conf.int[2]
    test  = t.test(fitted_range_response[r,])
    fitted_intervals_response[r,1] = test$conf.int[1]
    fitted_intervals_response[r,2]  = test$conf.int[2]
  }
  
  for (r in 1:6){
    test  = t.test(opportunity_range_conditional[r,])
    opportunity_intervals_conditional[r,1] = test$conf.int[1]
    opportunity_intervals_conditional[r,2]  = test$conf.int[2]
    test  = t.test(preferences_range_conditional[r,])
    preferences_intervals_conditional[r,1] = test$conf.int[1]
    preferences_intervals_conditional[r,2]  = test$conf.int[2]
    test  = t.test(fitted_range_conditional[r,])
    fitted_intervals_conditional[r,1] = test$conf.int[1]
    fitted_intervals_conditional[r,2]  = test$conf.int[2]
  }
  
  #write csv files with ranges. 
  write.csv(coefficient_intervals_zi, file = here("Results", "coefficient_bias_intervals_hh_continuous_reduced_zeroinflated.csv"))
  write.csv(coefficient_intervals_cond, file = here("Results", "coefficient_bias_intervals_hh_continuous_reduced_conditional.csv"))
  write.csv(opportunity_intervals_zi, file = here("Results", "opportunity_ineq_bias_intervals_hh_continuous_reduced_zi.csv"))
  write.csv(opportunity_intervals_response, file = here("Results", "opportunity_ineq_bias_intervals_hh_continuous_reduced_response.csv"))
  write.csv(opportunity_intervals_conditional, file = here("Results", "opportunity_ineq_bias_intervals_hh_continuous_reduced_conditional.csv"))
  write.csv(preferences_intervals_zi, file = here("Results", "preference_ineq_bias_intervals_hh_continuous_reduced_zi.csv"))
  write.csv(preferences_intervals_response, file = here("Results", "preference_ineq_bias_intervals_hh_continuous_reduced_response.csv"))
  write.csv(preferences_intervals_conditional, file = here("Results", "preference_ineq_bias_intervals_hh_continuous_reduced_conditional.csv"))
  write.csv(fitted_intervals_zi, file = here("Results", "fitted_ineq_bias_intervals_hh_continuous_reduced_zi.csv"))
  write.csv(fitted_intervals_response, file = here("Results", "fitted_ineq_bias_intervals_hh_continuous_reduced_response.csv"))
  write.csv(fitted_intervals_conditional, file = here("Results", "fitted_ineq_bias_intervals_hh_continuous_reduced_conditional.csv"))
  
}


# End of function


