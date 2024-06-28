
# This script returns the inequality measures given the central and bootstrapped direct and total effects matrices. 


get_inequality_measures = function(fitted_values_full,
                                   fitted_values_reduced,
                                   title = "No title", 
                                   relative_to = "fitted") {
  
  source(here("Utilities", "get_inequality_functions.R"))
  
  ##################
  #### Outcomes ####
  ##################

  PSY_outcomes = PSY_ineq(fitted_values_full[[1]][,relative_to])
  ERR_outcomes = erreygers_ineq(fitted_values_full[[1]][,relative_to])
  GSC_outcomes = GSC_ineq(fitted_values_full[[1]][,relative_to])
  
  #####################
  #### FULL MODEL #####
  #####################
  
  #Now estimate these for distribution of fitted values
  PSY_full_dist = ERR_full_dist = GSC_full_dist = cbind(rep(0, ncol(fitted_values_full[[2]])), rep(0, ncol(fitted_values_full[[2]])))
  
  fitted_opportunity_dist = fitted_values_full$Dist_fitted_opportunity
  
  for (i in 1:ncol(fitted_opportunity_dist)){
    
    PSY_full_dist[i,1] = PSY_ineq(fitted_opportunity_dist[,i])
    PSY_full_dist[i,2] = (PSY_outcomes  - PSY_full_dist[i,1])/ PSY_outcomes 
    
    ERR_full_dist[i,1] = erreygers_ineq(fitted_opportunity_dist[,i])
    ERR_full_dist[i,2] = (ERR_outcomes  - ERR_full_dist[i,1])/ ERR_outcomes 
    
    GSC_full_dist[i,1] = GSC_ineq(fitted_opportunity_dist[,i])
    GSC_full_dist[i,2] = (GSC_outcomes  - GSC_full_dist[i,1])/ GSC_outcomes 
  }
  
  #Central estimates full model
  PSY_full_central_abs = PSY_ineq(fitted_values_full$Central_fitted[,1])
  PSY_full_central_rel = (PSY_outcomes - PSY_ineq(fitted_values_full$Central_fitted[,1]))/ PSY_outcomes
  
  ERR_full_central_abs = erreygers_ineq(fitted_values_full$Central_fitted[,1])
  ERR_full_central_rel = (ERR_outcomes - erreygers_ineq(fitted_values_full$Central_fitted[,1]))/ ERR_outcomes
  
  GSC_full_central_abs = GSC_ineq(fitted_values_full$Central_fitted[,1])
  GSC_full_central_rel = (GSC_outcomes - GSC_ineq(fitted_values_full$Central_fitted[,1]))/ GSC_outcomes
  
  #Distribution of results for only significant coefficents
  PSY_full_dist_sigonly = ERR_full_dist_sigonly = GSC_full_dist_sigonly = cbind(rep(0, ncol(fitted_values_full[[5]])), rep(0, ncol(fitted_values_full[[5]])))
  fitted_opportunity_dist = fitted_values_full$Central_onlysig
  for (i in 1:ncol(fitted_opportunity_dist)){
    PSY_full_dist_sigonly[i,1] = PSY_ineq(fitted_opportunity_dist[,i])
    PSY_full_dist_sigonly[i,2] = (PSY_outcomes  - PSY_full_dist[i,1])/ PSY_outcomes 
    
    ERR_full_dist_sigonly[i,1] = erreygers_ineq(fitted_opportunity_dist[,i])
    ERR_full_dist_sigonly[i,2] = (ERR_outcomes  - ERR_full_dist[i,1])/ ERR_outcomes 
    
    GSC_full_dist_sigonly[i,1] = GSC_ineq(fitted_opportunity_dist[,i])
    GSC_full_dist_sigonly[i,2] = (GSC_outcomes  - GSC_full_dist[i,1])/ GSC_outcomes 
  }
  
  #############################
  #### REDUCED FORM MODEL #####
  #############################
  
  #Calculate inequality for full distributions
  PSY_reduced_dist = ERR_reduced_dist = GSC_reduced_dist = cbind(rep(0, ncol(fitted_values_reduced[[2]])), rep(0, ncol(fitted_values_reduced[[2]])))
  fitted_opportunity_dist = fitted_values_reduced$Dist_fitted_opportunity
  
  for (i in 1:ncol(fitted_opportunity_dist)){
  
    PSY_reduced_dist[i,1] = PSY_ineq(fitted_opportunity_dist[,i])
    PSY_reduced_dist[i,2] = (PSY_outcomes  - PSY_reduced_dist[i,1])/ PSY_outcomes 
    
    ERR_reduced_dist[i,1] = erreygers_ineq(fitted_opportunity_dist[,i])
    ERR_reduced_dist[i,2] = (ERR_outcomes  - ERR_reduced_dist[i,1])/ ERR_outcomes 
    
    GSC_reduced_dist[i,1] = GSC_ineq(fitted_opportunity_dist[,i])
    GSC_reduced_dist[i,2] = (GSC_outcomes  - GSC_reduced_dist[i,1])/ GSC_outcomes 
  }
  
  #Central estimates reduced model
  PSY_reduced_central_abs = PSY_ineq(fitted_values_reduced$Central_fitted[,1])
  PSY_reduced_central_rel = (PSY_outcomes - PSY_ineq(fitted_values_reduced$Central_fitted[,1]))/ PSY_outcomes
  
  ERR_reduced_central_abs = erreygers_ineq(fitted_values_reduced$Central_fitted[,1])
  ERR_reduced_central_rel = (ERR_outcomes - erreygers_ineq(fitted_values_reduced$Central_fitted[,1]))/ ERR_outcomes
  
  GSC_reduced_central_abs = GSC_ineq(fitted_values_reduced$Central_fitted[,1])
  GSC_reduced_central_rel = (GSC_outcomes - GSC_ineq(fitted_values_reduced$Central_fitted[,1]))/ GSC_outcomes
  
  #Distribution of results for only significant coefficents
  PSY_reduced_dist_sigonly = ERR_reduced_dist_sigonly = GSC_reduced_dist_sigonly = cbind(rep(0, ncol(fitted_values_reduced[[5]])), rep(0, ncol(fitted_values_reduced[[5]])))
  fitted_opportunity_dist = fitted_values_reduced$Central_onlysig
  for (i in 1:ncol(fitted_opportunity_dist)){
    PSY_reduced_dist_sigonly[i,1] = PSY_ineq(fitted_opportunity_dist[,i])
    PSY_reduced_dist_sigonly[i,2] = (PSY_outcomes  - PSY_reduced_dist[i,1])/ PSY_outcomes 
    
    ERR_reduced_dist_sigonly[i,1] = erreygers_ineq(fitted_opportunity_dist[,i])
    ERR_reduced_dist_sigonly[i,2] = (ERR_outcomes  - ERR_reduced_dist[i,1])/ ERR_outcomes 
    
    GSC_reduced_dist_sigonly[i,1] = GSC_ineq(fitted_opportunity_dist[,i])
    GSC_reduced_dist_sigonly[i,2] = (GSC_outcomes  - GSC_reduced_dist[i,1])/ GSC_outcomes 
  }
  
  #######################
  #### Save results #####
  #######################
  
  outcomes_df_abs = data.frame(
    Measure = c("PSY", "ERR", "GSC"),
    Full_Model_Outcome = c(t.test(PSY_full_dist[,1])$estimate, t.test(ERR_full_dist[,1])$estimate, t.test(GSC_full_dist[,1])$estimate),
    Full_Model_Lower = c(t.test(PSY_full_dist[,1])$conf.int[1], t.test(ERR_full_dist[,1])$conf.int[1], t.test(GSC_full_dist[,1])$conf.int[1]),
    Full_Model_Upper = c(t.test(PSY_full_dist[,1])$conf.int[2], t.test(ERR_full_dist[,1])$conf.int[2], t.test(GSC_full_dist[,1])$conf.int[2]),
    Full_model_sigonly = c(t.test(PSY_full_dist_sigonly[,1])$estimate, t.test(ERR_full_dist_sigonly[,1])$estimate, t.test(GSC_full_dist_sigonly[,1])$estimate),
    Reduced_Model_Outcome = c(t.test(PSY_reduced_dist[,1])$estimate, t.test(ERR_reduced_dist[,1])$estimate, t.test(GSC_reduced_dist[,1])$estimate),
    Reduced_Model_Lower = c(t.test(PSY_reduced_dist[,1])$conf.int[1], t.test(ERR_reduced_dist[,1])$conf.int[1], t.test(GSC_reduced_dist[,1])$conf.int[1]),
    Reduced_Model_Upper = c(t.test(PSY_reduced_dist[,1])$conf.int[2], t.test(ERR_reduced_dist[,1])$conf.int[2], t.test(GSC_reduced_dist[,1])$conf.int[2]),
    Reduced_model_sigonly = c(t.test(PSY_reduced_dist_sigonly[,1])$estimate, t.test(ERR_reduced_dist_sigonly[,1])$estimate, t.test(GSC_reduced_dist_sigonly[,1])$estimate)
  )
  
  write.csv(outcomes_df_abs, file = here("Results", paste(title, "- absolute.csv", sep = "")))
  
  outcomes_df_relative = data.frame(
    Measure = c("PSY", "ERR", "GSC"),
    Full_Model_Outcome = c(t.test(PSY_full_dist[,2])$estimate, t.test(ERR_full_dist[,2])$estimate, t.test(GSC_full_dist[,2])$estimate),
    Full_Model_Lower = c(t.test(PSY_full_dist[,2])$conf.int[1], t.test(ERR_full_dist[,2])$conf.int[1], t.test(GSC_full_dist[,2])$conf.int[1]),
    Full_Model_Upper = c(t.test(PSY_full_dist[,2])$conf.int[2], t.test(ERR_full_dist[,2])$conf.int[2], t.test(GSC_full_dist[,2])$conf.int[2]),
    Full_model_sigonly = c(t.test(PSY_full_dist_sigonly[,2])$estimate, t.test(ERR_full_dist_sigonly[,2])$estimate, t.test(GSC_full_dist_sigonly[,2])$estimate),
    Reduced_Model_Outcome = c(t.test(PSY_reduced_dist[,2])$estimate, t.test(ERR_reduced_dist[,2])$estimate, t.test(GSC_reduced_dist[,2])$estimate),
    Reduced_Model_Lower = c(t.test(PSY_reduced_dist[,2])$conf.int[1], t.test(ERR_reduced_dist[,2])$conf.int[1], t.test(GSC_reduced_dist[,2])$conf.int[1]),
    Reduced_Model_Upper = c(t.test(PSY_reduced_dist[,2])$conf.int[2], t.test(ERR_reduced_dist[,2])$conf.int[2], t.test(GSC_reduced_dist[,2])$conf.int[2]),
    Reduced_model_sigonly = c(t.test(PSY_reduced_dist_sigonly[,2])$estimate, t.test(ERR_reduced_dist_sigonly[,2])$estimate, t.test(GSC_reduced_dist_sigonly[,2])$estimate)
  )
  
  write.csv(outcomes_df_relative, file = here("Results", paste(title, "- relative.csv", sep = "")))
  
}