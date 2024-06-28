
# This script returns the inequality measures given the central and bootstrapped direct and total effects matrices. 


get_other_inequality_measures = function(fitted_values_full,
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
  
  #Now estimate inequality for distribution of fitted values 
  PSY_full_pref = ERR_full_pref = GSC_full_pref = cbind(rep(0, ncol(fitted_values_full[[3]])), rep(0, ncol(fitted_values_full[[3]])))
  
  fitted_preferences_dist = fitted_values_full$Dist_fitted_preferences
  
  for (i in 1:ncol(fitted_preferences_dist)){
    if (min(fitted_preferences_dist[,i]) <0){
      fitted_preferences_dist[,i] = fitted_preferences_dist[,i] + abs(min(fitted_preferences_dist[,i]))
    }
    
    PSY_full_pref[i,1] = PSY_ineq(fitted_preferences_dist[,i])
    PSY_full_pref[i,2] = (PSY_outcomes  - PSY_full_pref[i,1])/ PSY_outcomes 
    
    ERR_full_pref[i,1] = erreygers_ineq(fitted_preferences_dist[,i])
    ERR_full_pref[i,2] = (ERR_outcomes  - ERR_full_pref[i,1])/ ERR_outcomes 
    
    GSC_full_pref[i,1] = GSC_ineq(fitted_preferences_dist[,i])
    GSC_full_pref[i,2] = (GSC_outcomes  - GSC_full_pref[i,1])/ GSC_outcomes 
  }
  
  PSY_full_latent = ERR_full_latent = GSC_full_latent = cbind(rep(0, ncol(fitted_values_full[[4]])), rep(0, ncol(fitted_values_full[[4]])))
  
  fitted_latent_dist = fitted_values_full$Dist_fitted_all
  
  for (i in 1:ncol(fitted_latent_dist)){
    if (min(fitted_latent_dist[,i]) <0){
      fitted_latent_dist[,i] = fitted_latent_dist[,i] + abs(min(fitted_latent_dist[,i]))
    }
    
    PSY_full_latent[i,1] = PSY_ineq(fitted_latent_dist[,i])
    PSY_full_latent[i,2] = (PSY_outcomes  - PSY_full_latent[i,1])/ PSY_outcomes 
    
    ERR_full_latent[i,1] = erreygers_ineq(fitted_latent_dist[,i])
    ERR_full_latent[i,2] = (ERR_outcomes  - ERR_full_latent[i,1])/ ERR_outcomes 
    
    GSC_full_latent[i,1] = GSC_ineq(fitted_latent_dist[,i])
    GSC_full_latent[i,2] = (GSC_outcomes  - GSC_full_latent[i,1])/ GSC_outcomes 
  }
  

  #Central estimates full model - preferences
  PSY_full_central_abs_pref = PSY_ineq(fitted_values_full$Central_fitted[,2])
  PSY_full_central_rel_pref = (PSY_outcomes - PSY_ineq(fitted_values_full$Central_fitted[,2]))/ PSY_outcomes
  
  ERR_full_central_abs_pref = erreygers_ineq(fitted_values_full$Central_fitted[,2])
  ERR_full_central_rel_pref = (ERR_outcomes - erreygers_ineq(fitted_values_full$Central_fitted[,2]))/ ERR_outcomes
  
  GSC_full_central_abs_pref = GSC_ineq(fitted_values_full$Central_fitted[,2])
  GSC_full_central_rel_pref =(GSC_outcomes - GSC_ineq(fitted_values_full$Central_fitted[,2]))/ GSC_outcomes
  
  #Central estimates full model - all
  PSY_full_central_abs_latent = PSY_ineq(fitted_values_full$Central_fitted[,3])
  PSY_full_central_rel_latent = (PSY_outcomes - PSY_ineq(fitted_values_full$Central_fitted[,3]))/ PSY_outcomes
  
  ERR_full_central_abs_latent = erreygers_ineq(fitted_values_full$Central_fitted[,3])
  ERR_full_central_rel_latent = (ERR_outcomes - erreygers_ineq(fitted_values_full$Central_fitted[,3]))/ ERR_outcomes
  
  GSC_full_central_abs_latent = GSC_ineq(fitted_values_full$Central_fitted[,3])
  GSC_full_central_rel_latent =(GSC_outcomes - GSC_ineq(fitted_values_full$Central_fitted[,3]))/ GSC_outcomes
  
  
  #############################
  #### REDUCED FORM MODEL #####
  #############################
  
  PSY_outcomes = PSY_ineq(fitted_values_reduced[[1]][,relative_to])
  ERR_outcomes = erreygers_ineq(fitted_values_reduced[[1]][,relative_to])
  GSC_outcomes = GSC_ineq(fitted_values_reduced[[1]][,relative_to])
  
  #Now estimate inequality for distribution of fitted values 
  PSY_reduced_pref = ERR_reduced_pref = GSC_reduced_pref = cbind(rep(0, ncol(fitted_values_reduced[[3]])), rep(0, ncol(fitted_values_reduced[[3]])))
  
  fitted_preferences_dist = fitted_values_reduced$Dist_fitted_preferences
  
  for (i in 1:ncol(fitted_preferences_dist)){
    if (min(fitted_preferences_dist[,i]) <0){
      fitted_preferences_dist[,i] = fitted_preferences_dist[,i] + abs(min(fitted_preferences_dist[,i]))
    }
    
    PSY_reduced_pref[i,1] = PSY_ineq(fitted_preferences_dist[,i])
    PSY_reduced_pref[i,2] = (PSY_outcomes  - PSY_reduced_pref[i,1])/ PSY_outcomes 
    
    ERR_reduced_pref[i,1] = erreygers_ineq(fitted_preferences_dist[,i])
    ERR_reduced_pref[i,2] = (ERR_outcomes  - ERR_reduced_pref[i,1])/ ERR_outcomes 
    
    GSC_reduced_pref[i,1] = GSC_ineq(fitted_preferences_dist[,i])
    GSC_reduced_pref[i,2] = (GSC_outcomes  - GSC_reduced_pref[i,1])/ GSC_outcomes 
  }
  
  PSY_reduced_latent = ERR_reduced_latent = GSC_reduced_latent = cbind(rep(0, ncol(fitted_values_reduced[[4]])), rep(0, ncol(fitted_values_reduced[[4]])))
  
  fitted_latent_dist = fitted_values_reduced$Dist_fitted_all
  
  for (i in 1:ncol(fitted_latent_dist)){
    if (min(fitted_latent_dist[,i]) <0){
      fitted_latent_dist[,i] = fitted_latent_dist[,i] + abs(min(fitted_latent_dist[,i]))
    }
    
    PSY_reduced_latent[i,1] = PSY_ineq(fitted_latent_dist[,i])
    PSY_reduced_latent[i,2] = (PSY_outcomes  - PSY_reduced_latent[i,1])/ PSY_outcomes 
    
    ERR_reduced_latent[i,1] = erreygers_ineq(fitted_latent_dist[,i])
    ERR_reduced_latent[i,2] = (ERR_outcomes  - ERR_reduced_latent[i,1])/ ERR_outcomes 
    
    GSC_reduced_latent[i,1] = GSC_ineq(fitted_latent_dist[,i])
    GSC_reduced_latent[i,2] = (GSC_outcomes  - GSC_reduced_latent[i,1])/ GSC_outcomes 
  }
  
  
  #Central estimates reduced model - preferences
  PSY_reduced_central_abs_pref = PSY_ineq(fitted_values_reduced$Central_fitted[,2])
  PSY_reduced_central_rel_pref = (PSY_outcomes - PSY_ineq(fitted_values_reduced$Central_fitted[,2]))/ PSY_outcomes
  
  ERR_reduced_central_abs_pref = erreygers_ineq(fitted_values_reduced$Central_fitted[,2])
  ERR_reduced_central_rel_pref = (ERR_outcomes - erreygers_ineq(fitted_values_reduced$Central_fitted[,2]))/ ERR_outcomes
  
  GSC_reduced_central_abs_pref = GSC_ineq(fitted_values_reduced$Central_fitted[,2])
  GSC_reduced_central_rel_pref =(GSC_outcomes - GSC_ineq(fitted_values_reduced$Central_fitted[,2]))/ GSC_outcomes
  
  #Central estimates reduced model - all
  PSY_reduced_central_abs_latent = PSY_ineq(fitted_values_reduced$Central_fitted[,3])
  PSY_reduced_central_rel_latent = (PSY_outcomes - PSY_ineq(fitted_values_reduced$Central_fitted[,3]))/ PSY_outcomes
  
  ERR_reduced_central_abs_latent = erreygers_ineq(fitted_values_reduced$Central_fitted[,3])
  ERR_reduced_central_rel_latent = (ERR_outcomes - erreygers_ineq(fitted_values_reduced$Central_fitted[,3]))/ ERR_outcomes
  
  GSC_reduced_central_abs_latent = GSC_ineq(fitted_values_reduced$Central_fitted[,3])
  GSC_reduced_central_rel_latent =(GSC_outcomes - GSC_ineq(fitted_values_reduced$Central_fitted[,3]))/ GSC_outcomes
  
  # Save the results
  outcomes_df_abs_pref = data.frame(
    Measure = c("PSY", "ERR", "GSC"),
    Full_Model_Outcome = c(t.test(PSY_full_pref[,1])$estimate, t.test(ERR_full_pref[,1])$estimate, t.test(GSC_full_pref[,1])$estimate),
    Full_Model_Lower = c(t.test(PSY_full_pref[,1])$conf.int[1], t.test(ERR_full_pref[,1])$conf.int[1], t.test(GSC_full_pref[,1])$conf.int[1]),
    Full_Model_Upper = c(t.test(PSY_full_pref[,1])$conf.int[2], t.test(ERR_full_pref[,1])$conf.int[2], t.test(GSC_full_pref[,1])$conf.int[2]),
    Reduced_Model_Outcome = c(t.test(PSY_reduced_pref[,1])$estimate, t.test(ERR_reduced_pref[,1])$estimate, t.test(GSC_reduced_pref[,1])$estimate),
    Reduced_Model_Lower = c(t.test(PSY_reduced_pref[,1])$conf.int[1], t.test(ERR_reduced_pref[,1])$conf.int[1], t.test(GSC_reduced_pref[,1])$conf.int[1]),
    Reduced_Model_Upper = c(t.test(PSY_reduced_pref[,1])$conf.int[2], t.test(ERR_reduced_pref[,1])$conf.int[2], t.test(GSC_reduced_pref[,1])$conf.int[2])
  )
  
  write.csv(outcomes_df_abs_pref, file = here("Results", paste(title, "- preferences absolute .csv", sep = "")))
  
  outcomes_df_relative_pref = data.frame(
    Measure = c("PSY", "ERR", "GSC"),
    Full_Model_Outcome = c(t.test(PSY_full_pref[,2])$estimate, t.test(ERR_full_pref[,2])$estimate, t.test(GSC_full_pref[,2])$estimate),
    Full_Model_Lower = c(t.test(PSY_full_pref[,2])$conf.int[1], t.test(ERR_full_pref[,2])$conf.int[1], t.test(GSC_full_pref[,2])$conf.int[1]),
    Full_Model_Upper = c(t.test(PSY_full_pref[,2])$conf.int[2], t.test(ERR_full_pref[,2])$conf.int[2], t.test(GSC_full_pref[,2])$conf.int[2]),
    Reduced_Model_Outcome = c(t.test(PSY_reduced_pref[,2])$estimate, t.test(ERR_reduced_pref[,2])$estimate, t.test(GSC_reduced_pref[,2])$estimate),
    Reduced_Model_Lower = c(t.test(PSY_reduced_pref[,2])$conf.int[1], t.test(ERR_reduced_pref[,2])$conf.int[1], t.test(GSC_reduced_pref[,2])$conf.int[1]),
    Reduced_Model_Upper = c(t.test(PSY_reduced_pref[,2])$conf.int[2], t.test(ERR_reduced_pref[,2])$conf.int[2], t.test(GSC_reduced_pref[,2])$conf.int[2])
  )
  
  write.csv(outcomes_df_relative_pref, file = here("Results", paste(title, "- preferences relative.csv", sep = "")))
  

  outcomes_df_abs_latent = data.frame(
    Measure = c("PSY", "ERR", "GSC"),
    Full_Model_Outcome = c(t.test(PSY_full_latent[,1])$estimate, t.test(ERR_full_latent[,1])$estimate, t.test(GSC_full_latent[,1])$estimate),
    Full_Model_Lower = c(t.test(PSY_full_latent[,1])$conf.int[1], t.test(ERR_full_latent[,1])$conf.int[1], t.test(GSC_full_latent[,1])$conf.int[1]),
    Full_Model_Upper = c(t.test(PSY_full_latent[,1])$conf.int[2], t.test(ERR_full_latent[,1])$conf.int[2], t.test(GSC_full_latent[,1])$conf.int[2]),
    Reduced_Model_Outcome = c(t.test(PSY_reduced_latent[,1])$estimate, t.test(ERR_reduced_latent[,1])$estimate, t.test(GSC_reduced_latent[,1])$estimate),
    Reduced_Model_Lower = c(t.test(PSY_reduced_latent[,1])$conf.int[1], t.test(ERR_reduced_latent[,1])$conf.int[1], t.test(GSC_reduced_latent[,1])$conf.int[1]),
    Reduced_Model_Upper = c(t.test(PSY_reduced_latent[,1])$conf.int[2], t.test(ERR_reduced_latent[,1])$conf.int[2], t.test(GSC_reduced_latent[,1])$conf.int[2])
  )
  
  write.csv(outcomes_df_abs_latent, file = here("Results", paste(title, "- latent absolute .csv", sep = "")))
  
  outcomes_df_relative_latent = data.frame(
    Measure = c("PSY", "ERR", "GSC"),
    Full_Model_Outcome = c(t.test(PSY_full_latent[,2])$estimate, t.test(ERR_full_latent[,2])$estimate, t.test(GSC_full_latent[,2])$estimate),
    Full_Model_Lower = c(t.test(PSY_full_latent[,2])$conf.int[1], t.test(ERR_full_latent[,2])$conf.int[1], t.test(GSC_full_latent[,2])$conf.int[1]),
    Full_Model_Upper = c(t.test(PSY_full_latent[,2])$conf.int[2], t.test(ERR_full_latent[,2])$conf.int[2], t.test(GSC_full_latent[,2])$conf.int[2]),
    Reduced_Model_Outcome = c(t.test(PSY_reduced_latent[,2])$estimate, t.test(ERR_reduced_latent[,2])$estimate, t.test(GSC_reduced_latent[,2])$estimate),
    Reduced_Model_Lower = c(t.test(PSY_reduced_latent[,2])$conf.int[1], t.test(ERR_reduced_latent[,2])$conf.int[1], t.test(GSC_reduced_latent[,2])$conf.int[1]),
    Reduced_Model_Upper = c(t.test(PSY_reduced_latent[,2])$conf.int[2], t.test(ERR_reduced_latent[,2])$conf.int[2], t.test(GSC_reduced_latent[,2])$conf.int[2])
  )
  
  write.csv(outcomes_df_relative_latent, file = here("Results", paste(title, "- latent relative.csv", sep = "")))
  
  
}