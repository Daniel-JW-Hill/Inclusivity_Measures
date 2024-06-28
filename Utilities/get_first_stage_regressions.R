
# This function performs the first stage regressions of circumstances
# on preferences. 
# It returns hh_data and indiv_data with the respective residuals for all in variable vectors P and Z
# Results are also saved in tables for the manuscript. 

get_first_stage_regressions = function(hh_data, 
                                   indiv_Data, 
                                   C_hh,
                                   P_hh, 
                                   Z_hh,
                                   P_hh_omitted,
                                   C_indiv,
                                   P_indiv,
                                   Z_indiv,
                                   P_indiv_omitted){
  
  
  library(stargazer)
  
  ###################
  #### HH models ####
  ###################
  
  Preferences = c(P_hh, P_hh_omitted, Z_hh)
  
  models = list()
  counter = 0
  for (pref in Preferences){
    counter = counter + 1
    eq = paste(pref, sep = " ~ ", paste(paste(C_hh, collapse = " + ")))
    model_est = lm(eq, data = hh_data)
    model_est_summary = model_est
    models[[counter]] = model_est_summary
    hh_data$residuals = model_est_summary$residuals
    colnames(hh_data)[which(colnames(hh_data) == "residuals")] = paste(pref, "RESIDUALS", sep = "_")
  }
  
  stargazer(models, 
            type = "html", 
            title = "First Stage Regressions - HH Model",
            out=here("Results","First stage regressions - HH model.html"), 
            align = TRUE, 
            covariate.labels = NULL, 
            keep.stat = c("n", "rsq", "adj.rsq", "f"))
  
  
  ###########################
  #### Individual models ####
  ###########################
  
  Preferences = c(P_indiv, P_indiv_omitted, Z_indiv)
  
  models = list()
  counter = 0
  for (pref in Preferences){
    counter = counter + 1
    eq = paste(pref, sep = " ~ ", paste(paste(C_indiv, collapse = " + ")))
    model_est = lm(eq, data = indiv_data)
    model_est_summary = model_est
    models[[counter]] = model_est_summary
    indiv_data$residuals = model_est_summary$residuals
    colnames(indiv_data)[which(colnames(indiv_data) == "residuals")] = paste(pref, "RESIDUALS", sep = "_")
  }
  
  stargazer(models, 
            type = "html", 
            title = "First Stage Regressions - Individual Model",
            out=here("Results","First stage regressions - Individual model.html"), 
            align = TRUE, 
            covariate.labels = NULL, 
            keep.stat = c("n", "rsq", "adj.rsq", "f"))
  
  return(list(hh_data, indiv_data))
  
}

# End of function

