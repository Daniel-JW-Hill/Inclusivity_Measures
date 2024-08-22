
# This function performs the decomposition for the continuous hh  models. 
# 

get_decomposition_hh = function(hh_data, 
                                Outcome_hh,
                                C_hh,
                                P_hh,
                                Z_hh,
                                V_hh) {
  
 
  library(glmmTMB)
  library(performance)
  source(here("Utilities", "get_inequality_functions.R"))
  
  #######################
  ##### Direct MODEL ####
  #######################
  
  #Estimate central model
  eq_hh_direct =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(P_hh, collapse = " + "), Z_hh, sep = " + "))
  eq_hh_zero = paste(" ~ ", paste(paste(C_hh, collapse = " + "), paste(P_hh, collapse = " + "), Z_hh, sep = " + "), sep = "")
  
  eq_hh_direct = as.formula(eq_hh_direct)
  eq_hh_zero = as.formula(eq_hh_zero)
  
  #Estimate model
  model_hh_direct = glmmTMB(eq_hh_direct, data = hh_data, family = beta_family(link = "logit"), ziformula = eq_hh_zero, dispformula = ~ 1)
  
  ### Create new data frames equalising preferences, and equalising circumstances. 
  hh_data_cbar = hh_data_pbar =hh_data
  hh_data_cbar[C_hh] = lapply(hh_data_cbar[C_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[P_hh] = lapply(hh_data_pbar[P_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[Z_hh] = lapply(hh_data_pbar[Z_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))

  #Opportunity fitted values
  pred_opportunity_zi = predict(model_hh_direct, newdata = hh_data_pbar, type = "zprob")
  pred_opportunity_response = predict(model_hh_direct, newdata = hh_data_pbar, type = 'response')
  pred_opportunity_conditional = predict(model_hh_direct, newdata = hh_data_pbar, type = 'conditional')
  
  #Preference fitted values
  pred_preferences_zi = predict(model_hh_direct, newdata = hh_data_cbar, type = 'zprob')
  pred_preferences_response = predict(model_hh_direct, newdata = hh_data_cbar, type = 'response')
  pred_preferences_conditional = predict(model_hh_direct, newdata = hh_data_cbar, type = 'conditional')
  
  #Fitted outcomes (without error term)
  pred_zi = predict(model_hh_direct, newdata = hh_data, type = 'zprob')
  pred_response = predict(model_hh_direct, newdata = hh_data, type = 'response')
  pred_conditional = predict(model_hh_direct, newdata = hh_data, type = 'conditional')
  
  PSY_zi = c(PSY_ineq(pred_opportunity_zi),
               PSY_ineq(pred_preferences_zi),
               PSY_ineq(pred_zi))
  ERR_zi = c(erreygers_ineq(pred_opportunity_zi),
                erreygers_ineq(pred_preferences_zi),
                erreygers_ineq(pred_zi))
  GSC_zi = c(GSC_ineq(pred_opportunity_zi),
              GSC_ineq(pred_preferences_zi),
              GSC_ineq(pred_zi))

  PSY_res = c(PSY_ineq(pred_opportunity_response),
               PSY_ineq(pred_preferences_response),
               PSY_ineq(pred_response))
  ERR_res = c(erreygers_ineq(pred_opportunity_response),
                erreygers_ineq(pred_preferences_response),
                erreygers_ineq(pred_response))
  GSC_res = c(GSC_ineq(pred_opportunity_response),
              GSC_ineq(pred_preferences_response),
              GSC_ineq(pred_response))
  
  PSY_cond = c(PSY_ineq(pred_opportunity_conditional),
               PSY_ineq(pred_preferences_conditional),
               PSY_ineq(pred_conditional))
  ERR_cond = c(erreygers_ineq(pred_opportunity_conditional),
                erreygers_ineq(pred_preferences_conditional),
                erreygers_ineq(pred_conditional))
  GSC_cond = c(GSC_ineq(pred_opportunity_conditional),
              GSC_ineq(pred_preferences_conditional),
              GSC_ineq(pred_conditional))
              
  # Now retrieve contributions for opportunities      
  opportunity_PSY_contributions_zi = numeric(length(C_hh)+1)
  opportunity_ERR_contributions_zi = numeric(length(C_hh)+1)
  opportunity_GSC_contributions_zi = numeric(length(C_hh)+1)
  opportunity_PSY_contributions_res = numeric(length(C_hh)+1)
  opportunity_ERR_contributions_res = numeric(length(C_hh)+1)
  opportunity_GSC_contributions_res = numeric(length(C_hh)+1)
  opportunity_PSY_contributions_cond = numeric(length(C_hh)+1)
  opportunity_ERR_contributions_cond = numeric(length(C_hh)+1)
  opportunity_GSC_contributions_cond = numeric(length(C_hh)+1)
  
  for (i in 1:length(C_hh)) {
    hh_data_new = hh_data_pbar
    hh_data_new[C_hh[i]] = hh_data_cbar[C_hh[i]]
    
    pred_opportunity_zi_c = predict(model_hh_direct, newdata = hh_data_new, type = 'zprob')
    pred_opportunity_response_c = predict(model_hh_direct, newdata = hh_data_new, type = 'response')
    pred_opportunity_conditional_c = predict(model_hh_direct, newdata = hh_data_new, type = 'conditional')
    
    PSY_zi_c = PSY_ineq(pred_opportunity_zi_c)
    ERR_zi_c = erreygers_ineq(pred_opportunity_zi_c)
    GSC_zi_c = GSC_ineq(pred_opportunity_zi_c)
    
    PSY_res_c = PSY_ineq(pred_opportunity_response_c)
    ERR_res_c = erreygers_ineq(pred_opportunity_response_c)
    GSC_res_c = GSC_ineq(pred_opportunity_response_c)
    
    PSY_cond_c = PSY_ineq(pred_opportunity_conditional_c)
    ERR_cond_c = erreygers_ineq(pred_opportunity_conditional_c)
    GSC_cond_c = GSC_ineq(pred_opportunity_conditional_c)
    
    opportunity_PSY_contributions_zi[i] = ((PSY_zi_c - PSY_zi[1])/PSY_zi[1])
    opportunity_ERR_contributions_zi[i] = ((ERR_zi_c - ERR_zi[1])/ERR_zi[1])
    opportunity_GSC_contributions_zi[i] = ((GSC_zi_c - GSC_zi[1])/GSC_zi[1])

    opportunity_PSY_contributions_res[i] = ((PSY_res_c - PSY_res[1])/PSY_res[1])
    opportunity_ERR_contributions_res[i] = ((ERR_res_c - ERR_res[1])/ERR_res[1])
    opportunity_GSC_contributions_res[i] = ((GSC_res_c - GSC_res[1])/GSC_res[1])
    
    opportunity_PSY_contributions_cond[i] = ((PSY_cond_c - PSY_cond[1])/PSY_cond[1])
    opportunity_ERR_contributions_cond[i] = ((ERR_cond_c - ERR_cond[1])/ERR_cond[1])
    opportunity_GSC_contributions_cond[i] = ((GSC_cond_c - GSC_cond[1])/GSC_cond[1])
  }
  
  #Spatial decomposition
  hh_data_new = hh_data_pbar
  hh_data_new$DISTANCE_PAVED_ROAD= mean(hh_data_new$DISTANCE_PAVED_ROAD, na.rm = TRUE)
  hh_data_new$ALTITUDE = mean(hh_data_new$ALTITUDE, na.rm = TRUE)
  pred_opportunity_zi_c = predict(model_hh_direct, newdata = hh_data_new, type = 'zprob')
  pred_opportunity_response_c = predict(model_hh_direct, newdata = hh_data_new, type = 'response')
  pred_opportunity_conditional_c = predict(model_hh_direct, newdata = hh_data_new, type = 'conditional')
  PSY_zi_c = PSY_ineq(pred_opportunity_zi_c)
  ERR_zi_c = erreygers_ineq(pred_opportunity_zi_c)
  GSC_zi_c = GSC_ineq(pred_opportunity_zi_c)
  PSY_res_c = PSY_ineq(pred_opportunity_response_c)
  ERR_res_c = erreygers_ineq(pred_opportunity_response_c)
  GSC_res_c = GSC_ineq(pred_opportunity_response_c)
  PSY_cond_c = PSY_ineq(pred_opportunity_conditional_c)
  ERR_cond_c = erreygers_ineq(pred_opportunity_conditional_c)
  GSC_cond_c = GSC_ineq(pred_opportunity_conditional_c)
  idx = length(opportunity_PSY_contributions_res)
  opportunity_PSY_contributions_zi[idx] = ((PSY_zi_c - PSY_zi[1])/PSY_zi[1])
  opportunity_ERR_contributions_zi[idx] = ((ERR_zi_c - ERR_zi[1])/ERR_zi[1])
  opportunity_GSC_contributions_zi[idx] = ((GSC_zi_c - GSC_zi[1])/GSC_zi[1])
  opportunity_PSY_contributions_res[idx] = ((PSY_res_c - PSY_res[1])/PSY_res[1])
  opportunity_ERR_contributions_res[idx] = ((ERR_res_c - ERR_res[1])/ERR_res[1])
  opportunity_GSC_contributions_res[idx] = ((GSC_res_c - GSC_res[1])/GSC_res[1])
  opportunity_PSY_contributions_cond[idx] = ((PSY_cond_c - PSY_cond[1])/PSY_cond[1])
  opportunity_ERR_contributions_cond[idx] = ((ERR_cond_c - ERR_cond[1])/ERR_cond[1])
  opportunity_GSC_contributions_cond[idx] = ((GSC_cond_c - GSC_cond[1])/GSC_cond[1])
  
  
  # Save results of decomposition. 
  opportunity_decomp_hh_zi = data.frame("PSY" = c(PSY_zi[1], opportunity_PSY_contributions_zi),
                                         "ERR" = c(ERR_zi[1], opportunity_ERR_contributions_zi),
                                         "GSC" = c(GSC_zi[1], opportunity_GSC_contributions_zi))
  rownames(opportunity_decomp_hh_zi) = c("overall", C_hh,  "spatial")
  
  opportunity_decomp_hh_res = data.frame("PSY" = c(PSY_res[1], opportunity_PSY_contributions_res),
                                     "ERR" = c(ERR_res[1], opportunity_ERR_contributions_res),
                                     "GSC" = c(GSC_res[1], opportunity_GSC_contributions_res))
  rownames(opportunity_decomp_hh_res) = c("overall", C_hh,  "spatial")
  
  opportunity_decomp_hh_cond = data.frame("PSY" = c(PSY_cond[1], opportunity_PSY_contributions_cond),
                                         "ERR" = c(ERR_cond[1], opportunity_ERR_contributions_cond),
                                         "GSC" = c(GSC_cond[1], opportunity_GSC_contributions_cond))
  rownames(opportunity_decomp_hh_cond) = c("overall", C_hh, "spatial")
  
  write.csv(opportunity_decomp_hh_zi, file = here("Results", "Opportunity decomposition continuous zi direct model.csv"))
  write.csv(opportunity_decomp_hh_res, file = here("Results", "Opportunity decomposition continuous response direct model.csv"))
  write.csv(opportunity_decomp_hh_cond, file = here("Results", "Opportunity decomposition continuous conditional direct model.csv"))
  
  ########################
  ##### REDUCED MODEL ####
  ########################
  
  #Estimate central model
  eq_hh_reduced =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(V_hh, collapse = " + "), sep = " + "))
  eq_hh_zero = paste(" ~ ", paste(paste(C_hh, collapse = " + "), paste(V_hh, collapse = " + "),  sep = " + "), sep = "")
  
  eq_hh_reduced = as.formula(eq_hh_reduced)
  eq_hh_zero = as.formula(eq_hh_zero)
  
  #Estimate model
  model_hh_reduced = glmmTMB(eq_hh_reduced, data = hh_data, family = beta_family(link = "logit"), ziformula = eq_hh_zero, dispformula = ~ 1)
  
  ### Create new data frames equalising preferences, and equalising circumstances. 
  hh_data_cbar = hh_data_pbar =hh_data
  hh_data_cbar[C_hh] = lapply(hh_data_cbar[C_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[P_hh] = lapply(hh_data_pbar[P_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[Z_hh] = lapply(hh_data_pbar[Z_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  
  #Opportunity fitted values
  pred_opportunity_zi = predict(model_hh_reduced, newdata = hh_data_pbar, type = "zprob")
  pred_opportunity_response = predict(model_hh_reduced, newdata = hh_data_pbar, type = 'response')
  pred_opportunity_conditional = predict(model_hh_reduced, newdata = hh_data_pbar, type = 'conditional')
  
  #Preference fitted values
  pred_preferences_zi = predict(model_hh_reduced, newdata = hh_data_cbar, type = 'zprob')
  pred_preferences_response = predict(model_hh_reduced, newdata = hh_data_cbar, type = 'response')
  pred_preferences_conditional = predict(model_hh_reduced, newdata = hh_data_cbar, type = 'conditional')
  
  #Fitted outcomes (without error term)
  pred_zi = predict(model_hh_reduced, newdata = hh_data, type = 'zprob')
  pred_response = predict(model_hh_reduced, newdata = hh_data, type = 'response')
  pred_conditional = predict(model_hh_reduced, newdata = hh_data, type = 'conditional')
  
  PSY_zi = c(PSY_ineq(pred_opportunity_zi),
              PSY_ineq(pred_preferences_zi),
              PSY_ineq(pred_zi))
  ERR_zi = c(erreygers_ineq(pred_opportunity_zi),
               erreygers_ineq(pred_preferences_zi),
               erreygers_ineq(pred_zi))
  GSC_zi = c(GSC_ineq(pred_opportunity_zi),
             GSC_ineq(pred_preferences_zi),
             GSC_ineq(pred_zi))
  
  PSY_res = c(PSY_ineq(pred_opportunity_response),
               PSY_ineq(pred_preferences_response),
               PSY_ineq(pred_response))
  ERR_res = c(erreygers_ineq(pred_opportunity_response),
                erreygers_ineq(pred_preferences_response),
                erreygers_ineq(pred_response))
  GSC_res = c(GSC_ineq(pred_opportunity_response),
              GSC_ineq(pred_preferences_response),
              GSC_ineq(pred_response))
  
  PSY_cond = c(PSY_ineq(pred_opportunity_conditional),
                PSY_ineq(pred_preferences_conditional),
                PSY_ineq(pred_conditional))
  ERR_cond = c(erreygers_ineq(pred_opportunity_conditional),
                 erreygers_ineq(pred_preferences_conditional),
                 erreygers_ineq(pred_conditional))
  GSC_cond = c(GSC_ineq(pred_opportunity_conditional),
               GSC_ineq(pred_preferences_conditional),
               GSC_ineq(pred_conditional))
  
  # Now retrieve contributions for opportunities      
  opportunity_PSY_contributions_zi = numeric(length(C_hh)+1)
  opportunity_ERR_contributions_zi = numeric(length(C_hh)+1)
  opportunity_GSC_contributions_zi = numeric(length(C_hh)+1)
  opportunity_PSY_contributions_res = numeric(length(C_hh)+1)
  opportunity_ERR_contributions_res = numeric(length(C_hh)+1)
  opportunity_GSC_contributions_res = numeric(length(C_hh)+1)
  opportunity_PSY_contributions_cond = numeric(length(C_hh)+1)
  opportunity_ERR_contributions_cond = numeric(length(C_hh)+1)
  opportunity_GSC_contributions_cond = numeric(length(C_hh)+1)
  
  for (i in 1:length(C_hh)) {
    hh_data_new = hh_data_pbar
    hh_data_new[C_hh[i]] = hh_data_cbar[C_hh[i]]
    
    pred_opportunity_zi_c = predict(model_hh_reduced, newdata = hh_data_new, type = 'zprob')
    pred_opportunity_response_c = predict(model_hh_reduced, newdata = hh_data_new, type = 'response')
    pred_opportunity_conditional_c = predict(model_hh_reduced, newdata = hh_data_new, type = 'conditional')
    
    PSY_zi_c = PSY_ineq(pred_opportunity_zi_c)
    ERR_zi_c = erreygers_ineq(pred_opportunity_zi_c)
    GSC_zi_c = GSC_ineq(pred_opportunity_zi_c)
    
    PSY_res_c = PSY_ineq(pred_opportunity_response_c)
    ERR_res_c = erreygers_ineq(pred_opportunity_response_c)
    GSC_res_c = GSC_ineq(pred_opportunity_response_c)
    
    PSY_cond_c = PSY_ineq(pred_opportunity_conditional_c)
    ERR_cond_c = erreygers_ineq(pred_opportunity_conditional_c)
    GSC_cond_c = GSC_ineq(pred_opportunity_conditional_c)
    
    opportunity_PSY_contributions_zi[i] = ((PSY_zi_c - PSY_zi[1])/PSY_zi[1])
    opportunity_ERR_contributions_zi[i] = ((ERR_zi_c - ERR_zi[1])/ERR_zi[1])
    opportunity_GSC_contributions_zi[i] = ((GSC_zi_c - GSC_zi[1])/GSC_zi[1])
    
    opportunity_PSY_contributions_res[i] = ((PSY_res_c - PSY_res[1])/PSY_res[1])
    opportunity_ERR_contributions_res[i] = ((ERR_res_c - ERR_res[1])/ERR_res[1])
    opportunity_GSC_contributions_res[i] = ((GSC_res_c - GSC_res[1])/GSC_res[1])
    
    opportunity_PSY_contributions_cond[i] = ((PSY_cond_c - PSY_cond[1])/PSY_cond[1])
    opportunity_ERR_contributions_cond[i] = ((ERR_cond_c - ERR_cond[1])/ERR_cond[1])
    opportunity_GSC_contributions_cond[i] = ((GSC_cond_c - GSC_cond[1])/GSC_cond[1])
  }
  
  #Spatial decomposition
  hh_data_new = hh_data_pbar
  hh_data_new$DISTANCE_PAVED_ROAD= mean(hh_data_new$DISTANCE_PAVED_ROAD, na.rm = TRUE)
  hh_data_new$ALTITUDE = mean(hh_data_new$ALTITUDE, na.rm = TRUE)
  pred_opportunity_zi_c = predict(model_hh_reduced, newdata = hh_data_new, type = 'zprob')
  pred_opportunity_response_c = predict(model_hh_reduced, newdata = hh_data_new, type = 'response')
  pred_opportunity_conditional_c = predict(model_hh_reduced, newdata = hh_data_new, type = 'conditional')
  PSY_zi_c = PSY_ineq(pred_opportunity_zi_c)
  ERR_zi_c = erreygers_ineq(pred_opportunity_zi_c)
  GSC_zi_c = GSC_ineq(pred_opportunity_zi_c)
  PSY_res_c = PSY_ineq(pred_opportunity_response_c)
  ERR_res_c = erreygers_ineq(pred_opportunity_response_c)
  GSC_res_c = GSC_ineq(pred_opportunity_response_c)
  PSY_cond_c = PSY_ineq(pred_opportunity_conditional_c)
  ERR_cond_c = erreygers_ineq(pred_opportunity_conditional_c)
  GSC_cond_c = GSC_ineq(pred_opportunity_conditional_c)
  idx = length(opportunity_PSY_contributions_res)
  opportunity_PSY_contributions_zi[idx] = ((PSY_zi_c - PSY_zi[1])/PSY_zi[1])
  opportunity_ERR_contributions_zi[idx] = ((ERR_zi_c - ERR_zi[1])/ERR_zi[1])
  opportunity_GSC_contributions_zi[idx] = ((GSC_zi_c - GSC_zi[1])/GSC_zi[1])
  opportunity_PSY_contributions_res[idx] = ((PSY_res_c - PSY_res[1])/PSY_res[1])
  opportunity_ERR_contributions_res[idx] = ((ERR_res_c - ERR_res[1])/ERR_res[1])
  opportunity_GSC_contributions_res[idx] = ((GSC_res_c - GSC_res[1])/GSC_res[1])
  opportunity_PSY_contributions_cond[idx] = ((PSY_cond_c - PSY_cond[1])/PSY_cond[1])
  opportunity_ERR_contributions_cond[idx] = ((ERR_cond_c - ERR_cond[1])/ERR_cond[1])
  opportunity_GSC_contributions_cond[idx] = ((GSC_cond_c - GSC_cond[1])/GSC_cond[1])
  
  
  # Save results of decomposition. 
  opportunity_decomp_hh_zi = data.frame("PSY" = c(PSY_zi[1], opportunity_PSY_contributions_zi),
                                        "ERR" = c(ERR_zi[1], opportunity_ERR_contributions_zi),
                                        "GSC" = c(GSC_zi[1], opportunity_GSC_contributions_zi))
  rownames(opportunity_decomp_hh_zi) = c("overall", C_hh,  "spatial")
  
  opportunity_decomp_hh_res = data.frame("PSY" = c(PSY_res[1], opportunity_PSY_contributions_res),
                                         "ERR" = c(ERR_res[1], opportunity_ERR_contributions_res),
                                         "GSC" = c(GSC_res[1], opportunity_GSC_contributions_res))
  rownames(opportunity_decomp_hh_res) = c("overall", C_hh,  "spatial")
  
  opportunity_decomp_hh_cond = data.frame("PSY" = c(PSY_cond[1], opportunity_PSY_contributions_cond),
                                          "ERR" = c(ERR_cond[1], opportunity_ERR_contributions_cond),
                                          "GSC" = c(GSC_cond[1], opportunity_GSC_contributions_cond))
  rownames(opportunity_decomp_hh_cond) = c("overall", C_hh, "spatial")
  
  write.csv(opportunity_decomp_hh_zi, file = here("Results", "Opportunity decomposition continuous zi reduced model.csv"))
  write.csv(opportunity_decomp_hh_res, file = here("Results", "Opportunity decomposition continuous response reduced model.csv"))
  write.csv(opportunity_decomp_hh_cond, file = here("Results", "Opportunity decomposition continuous conditional reduced model.csv"))
  
}
# End of function





  