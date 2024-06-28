
# This function performs the decompositions for the reduced hh and individual models. 
# 

get_contributions_binary_reduced = function(hh_data, 
                                    indiv_data,
                                    Outcome_hh,
                                    C_hh,
                                    V_hh,
                                    Outcome_indiv,
                                    C_indiv,
                                    V_indiv) {
  
  library(logistf)
  source(here("Utilities", "get_inequality_functions.R"))
  
  ###################
  ##### HH MODEL ####
  ###################
  
  #Estimate central model
  eq_hh_reduced =  paste(Outcome_hh, sep = " ~ ", paste(paste(C_hh, collapse = " + "), paste(V_hh, collapse = " + "),  sep = " + "))
  model_hh_reduced = logistf(eq_hh_reduced, data = hh_data)
  model_summary_hh_reduced = summary(model_hh_reduced)
  
  # Equalise variation for estimating fitted values
  hh_data_cbar = hh_data_pbar =hh_data
  hh_data_cbar[C_hh] = lapply(hh_data_cbar[C_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  hh_data_pbar[V_hh] = lapply(hh_data_pbar[V_hh], function(x) rep(mean(x, na.rm = TRUE), length(x)))

  fitted_opportunity =  predict(model_hh_reduced, newdata = hh_data_pbar, type = 'response' )
  fitted_preferences = predict(model_hh_reduced, newdata = hh_data_cbar, type = 'response' )
  fitted_outcomes = predict(model_hh_reduced, data = hh_data, type = 'response')
  
  PSY = c(PSY_ineq(fitted_opportunity),
           PSY_ineq(fitted_preferences),
           PSY_ineq(fitted_outcomes))
  ERR = c(erreygers_ineq(fitted_opportunity),
            erreygers_ineq(fitted_preferences),
            erreygers_ineq(fitted_outcomes))
  GSC = c(GSC_ineq(fitted_opportunity),
          GSC_ineq(fitted_preferences),
          GSC_ineq(fitted_outcomes))
  
  # Now retrieve contributions for opportunities      
  opportunity_PSY_contributions = numeric(length(C_hh)+1)
  opportunity_ERR_contributions = numeric(length(C_hh)+1)
  opportunity_GSC_contributions = numeric(length(C_hh)+1)
  
  for (i in 1:length(C_hh)) {
    
    hh_data_new = hh_data_pbar
    hh_data_new[C_hh[i]] = hh_data_cbar[C_hh[i]]
    
    pred_opportunity_c = predict(model_hh_reduced, newdata = hh_data_new, type = 'response')
    
    PSY_c = PSY_ineq(pred_opportunity_c)
    ERR_c = erreygers_ineq(pred_opportunity_c)
    GSC_c = GSC_ineq(pred_opportunity_c)
    
    opportunity_PSY_contributions[i] = ((PSY_c - PSY[1])/PSY[1])
    opportunity_ERR_contributions[i] = ((ERR_c - ERR[1])/ERR[1])
    opportunity_GSC_contributions[i] = ((GSC_c - GSC[1])/GSC[1])
  }

  #Spatial decomposition
  hh_data_new = hh_data_pbar
  hh_data_new$DISTANCE_PAVED_ROAD= mean(hh_data_new$DISTANCE_PAVED_ROAD, na.rm = TRUE)
  hh_data_new$ALTITUDE = mean(hh_data_new$ALTITUDE, na.rm = TRUE)
  pred_opportunity_c = predict(model_hh_reduced, newdata = hh_data_new, type = 'response')
  PSY_c = PSY_ineq(pred_opportunity_c)
  ERR_c = erreygers_ineq(pred_opportunity_c)
  GSC_c = GSC_ineq(pred_opportunity_c)
  idx = length(opportunity_PSY_contributions)
  opportunity_PSY_contributions[idx] = ((PSY_c - PSY[1])/PSY[1])
  opportunity_ERR_contributions[idx] = ((ERR_c - ERR[1])/ERR[1])
  opportunity_GSC_contributions[idx] = ((GSC_c - GSC[1])/GSC[1])
  
  # Save results of decomposition. 
  opportunity_decomp_hh = data.frame("PSY" = c(PSY[1], opportunity_PSY_contributions),
                                     "ERR" = c(ERR[1], opportunity_ERR_contributions),
                                     "GSC" = c(GSC[1], opportunity_GSC_contributions))
  rownames(opportunity_decomp_hh) = c("overall", C_hh, "spatial")
  
  write.csv(opportunity_decomp_hh, file = here("Results", "Opportunity decomposition binary HH reduced model.csv"))
  
  
  ######################
  ##### INDIV MODEL ####
  ######################
  
  #Estimate central model
  eq_indiv_reduced =  paste(Outcome_indiv, sep = " ~ ", paste(paste(C_indiv, collapse = " + "), paste(V_indiv, collapse = " + "), sep = " + ")) #Estimate model
  model_indiv_reduced = logistf(eq_indiv_reduced, data = indiv_data)
  model_summary_indiv_reduced = summary(model_indiv_reduced)
  
  # Equalise variation for estimating fitted values
  indiv_data_cbar = indiv_data_pbar =indiv_data
  indiv_data_cbar[C_indiv] = lapply(indiv_data_cbar[C_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))
  indiv_data_pbar[V_indiv] = lapply(indiv_data_pbar[V_indiv], function(x) rep(mean(x, na.rm = TRUE), length(x)))

  fitted_opportunity =  predict(model_indiv_reduced, newdata = indiv_data_pbar, type = 'response' )
  fitted_preferences = predict(model_indiv_reduced, newdata = indiv_data_cbar, type = 'response' )
  fitted_outcomes = predict(model_indiv_reduced, data = indiv_data, type = 'response')
  
  PSY = c(PSY_ineq(fitted_opportunity),
           PSY_ineq(fitted_preferences),
           PSY_ineq(fitted_outcomes))
  ERR = c(erreygers_ineq(fitted_opportunity),
            erreygers_ineq(fitted_preferences),
            erreygers_ineq(fitted_outcomes))
  GSC = c(GSC_ineq(fitted_opportunity),
          GSC_ineq(fitted_preferences),
          GSC_ineq(fitted_outcomes))
  
  # Now retrieve contributions for opportunities      
  opportunity_PSY_contributions = numeric(length(C_indiv)+1)
  opportunity_ERR_contributions = numeric(length(C_indiv)+1)
  opportunity_GSC_contributions = numeric(length(C_indiv)+1)
  
  for (i in 1:length(C_indiv)) {
    
    indiv_data_new = indiv_data_pbar
    indiv_data_new[C_indiv[i]] = indiv_data_cbar[C_indiv[i]]
    
    pred_opportunity_c = predict(model_indiv_reduced, newdata = indiv_data_new, type = 'response')
    
    PSY_c = PSY_ineq(pred_opportunity_c)
    ERR_c = erreygers_ineq(pred_opportunity_c)
    GSC_c = GSC_ineq(pred_opportunity_c)
    
    opportunity_PSY_contributions[i] = ((PSY_c - PSY[1])/PSY[1])
    opportunity_ERR_contributions[i] = ((ERR_c - ERR[1])/ERR[1])
    opportunity_GSC_contributions[i] = ((GSC_c - GSC[1])/GSC[1])
  }
  
  #Spatial decomposition
  indiv_data_new = indiv_data_pbar
  indiv_data_new$DISTANCE_PAVED_ROAD= mean(indiv_data_new$DISTANCE_PAVED_ROAD, na.rm = TRUE)
  indiv_data_new$ALTITUDE = mean(indiv_data_new$ALTITUDE, na.rm = TRUE)
  pred_opportunity_c = predict(model_indiv_reduced, newdata = indiv_data_new, type = 'response')
  PSY_c = PSY_ineq(pred_opportunity_c)
  ERR_c = erreygers_ineq(pred_opportunity_c)
  GSC_c = GSC_ineq(pred_opportunity_c)
  idx = length(opportunity_PSY_contributions)
  opportunity_PSY_contributions[idx] = ((PSY_c - PSY[1])/PSY[1])
  opportunity_ERR_contributions[idx] = ((ERR_c - ERR[1])/ERR[1])
  opportunity_GSC_contributions[idx] = ((GSC_c - GSC[1])/GSC[1])
  
  # Save results of decomposition. 
  opportunity_decomp_indiv = data.frame("PSY" = c(PSY[1], opportunity_PSY_contributions),
                                        "ERR" = c(ERR[1], opportunity_ERR_contributions),
                                        "GSC" = c(GSC[1], opportunity_GSC_contributions))
  rownames(opportunity_decomp_indiv) = c("overall", C_indiv,  "spatial")
  
  write.csv(opportunity_decomp_indiv, file = here("Results", "Opportunity decomposition binary indiv reduced model.csv"))
  
}
# End of function
  





  