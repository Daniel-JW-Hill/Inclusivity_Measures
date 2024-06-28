# This function saves plots for the contributions

get_contribution_plots = function(contributions_of = "GINI"){
  
  library(reshape2)
  library(ggplot2)
  
  #Read in data. 
  
  #Opportunity outcomes
  Opp_binary_hh_full = read.csv(here("Results","Opportunity decomposition binary HH full model.csv"))
  Opp_binary_hh_reduced = read.csv(here("Results","Opportunity decomposition binary HH reduced model.csv"))
  
  Opp_binary_indiv_full = read.csv(here("Results","Opportunity decomposition binary indiv full model.csv"))
  Opp_binary_indiv_reduced = read.csv(here("Results","Opportunity decomposition binary indiv reduced model.csv"))
  
  Opp_continuous_full_zi = read.csv(here("Results","Opportunity decomposition continuous zi full model.csv"))
  Opp_continuous_reduced_zi = read.csv(here("Results","Opportunity decomposition continuous zi reduced model.csv"))
  
  Opp_continuous_full_res = read.csv(here("Results","Opportunity decomposition continuous response full model.csv"))
  Opp_continuous_reduced_res = read.csv(here("Results","Opportunity decomposition continuous response reduced model.csv"))
  
  Opp_continuous_full_cond = read.csv(here("Results","Opportunity decomposition continuous conditional full model.csv"))
  Opp_continuous_reduced_cond = read.csv(here("Results","Opportunity decomposition continuous conditional reduced model.csv"))
  
  # Create and save plots using function call.
  # HH BINARY MODELS
  source(here("Utilities", "plot_chart.R"))
  
  reference_value = Opp_binary_hh_full[Opp_binary_hh_full$X == "overall", contributions_of]
  plot_chart(Opp_binary_hh_full, 
             contributions_of, 
             "Household sample", 
             "Binary participation", 
             "Full model","opportunity",  
             c("Age of head", "Education of head", "Dependency ratio", "Social participation", "Coffee trees", "Tenure", "Assets", "Altitude", "Distance paved road", "Decision disagreements", "Spatial factors"),
             reference_value)
  
  reference_value = Opp_binary_hh_reduced[Opp_binary_hh_reduced$X == "overall", contributions_of]
  plot_chart(Opp_binary_hh_reduced, 
             contributions_of, 
             "Household sample", 
             "Binary participation", 
             "Reduced form model","opportunity",  
             c( "Age of head", "Education of head", "Dependency ratio", "Social participation", "Coffee trees", "Tenure", "Assets", "Altitude", "Distance paved road", "Decision disagreements", "Spatial factors"),
             reference_value)
  
  # INDIV BINARY MODELS
  reference_value = Opp_binary_indiv_full[Opp_binary_indiv_full$X == "overall", contributions_of]
  plot_chart(Opp_binary_indiv_full, 
             contributions_of, 
             "Individual sample", 
             "Binary participation", 
             "Full model","opportunity",  
             c("Female",  "Age of head", "Education of head", "Dependency ratio", "Social participation", "Coffee trees",  "Tenure", "Assets", "Altitude", "Distance paved road", "Decision disagreements", "Spatial factors"),
             reference_value)
  
  reference_value = Opp_binary_indiv_reduced[Opp_binary_indiv_reduced$X == "overall", contributions_of]
  plot_chart(Opp_binary_indiv_reduced, 
             contributions_of, 
             "Individual sample", 
             "Binary participation", 
             "Reduced form model","opportunity",  
             c("Female", "Age of head", "Education of head", "Dependency ratio", "Social participation", "Coffee trees",  "Tenure", "Assets", "Altitude", "Distance paved road", "Decision disagreements", "Spatial factors"),
             reference_value)
  
  
  # HH Continuous MODELS
  reference_value = Opp_continuous_full_zi[Opp_continuous_full_zi$X == "overall", contributions_of]
  plot_chart(Opp_continuous_full_zi, 
             contributions_of, 
             "Household sample", 
             "Continuous participation - zi", 
             "Full model","opportunity",  
             c( "Age of head", "Education of head", "Dependency ratio", "Social participation", "Coffee trees", "Tenure", "Assets", "Altitude", "Distance paved road", "Decision disagreements", "Spatial factors"),
             reference_value)
  
  reference_value = Opp_continuous_reduced_zi[Opp_continuous_reduced_zi$X == "overall", contributions_of]
  plot_chart(Opp_continuous_reduced_zi, 
             contributions_of, 
             "Household sample", 
             "Continuous participation - zi", 
             "Reduced form model","opportunity",  
             c("Age of head", "Education of head", "Dependency ratio", "Social participation", "Coffee trees",  "Tenure", "Assets", "Altitude", "Distance paved road", "Decision disagreements", "Spatial factors"),
             reference_value)
  
  reference_value = Opp_continuous_full_res[Opp_continuous_full_res$X == "overall", contributions_of]
  plot_chart(Opp_continuous_full_res, 
             contributions_of, 
             "Household sample", 
             "Continuous participation - response", 
             "Full model","opportunity",  
             c( "Age of head", "Education of head", "Dependency ratio", "Social participation", "Coffee trees", "Tenure", "Assets", "Altitude", "Distance paved road", "Decision disagreements", "Spatial factors"),
             reference_value)
  
  reference_value = Opp_continuous_reduced_res[Opp_continuous_reduced_res$X == "overall", contributions_of]
  plot_chart(Opp_continuous_reduced_res, 
             contributions_of, 
             "Household sample", 
             "Continuous participation - response", 
             "Reduced form model","opportunity",  
             c("Age of head", "Education of head", "Dependency ratio", "Social participation", "Coffee trees",  "Tenure", "Assets", "Altitude", "Distance paved road", "Decision disagreements", "Spatial factors"),
             reference_value)
  

  reference_value = Opp_continuous_full_cond[Opp_continuous_full_cond$X == "overall", contributions_of]
  plot_chart(Opp_continuous_full_cond, 
             contributions_of, 
             "Household sample", 
             "Continuous participation - conditional", 
             "Full model","opportunity",  
             c( "Age of head", "Education of head", "Dependency ratio", "Social participation", "Coffee trees", "Tenure", "Assets", "Altitude", "Distance paved road", "Decision disagreements", "Spatial factors"),
             reference_value)
  
  reference_value = Opp_continuous_reduced_cond[Opp_continuous_reduced_cond$X == "overall", contributions_of]
  plot_chart(Opp_continuous_reduced_cond, 
             contributions_of, 
             "Household sample", 
             "Continuous participation - conditional", 
             "Reduced form model","opportunity",  
             c( "Age of head", "Education of head", "Dependency ratio", "Social participation", "Coffee trees",  "Tenure", "Assets", "Altitude", "Distance paved road", "Decision disagreements", "Spatial factors"),
             reference_value)

  
}