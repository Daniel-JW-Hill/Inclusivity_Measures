
# This function saves plots for the contributions

get_contribution_plots = function(contributions_of = "GINI"){
  
  library(reshape2)
  library(ggplot2)
  
  #Read in data. 
  
  #Opportunity outcomes
  
  # Individual extensive
  Opp_binary_indiv_direct = read.csv(here("Results","Opportunity decomposition binary indiv direct model.csv"))
  Opp_binary_indiv_reduced = read.csv(here("Results","Opportunity decomposition binary indiv reduced model.csv"))
  
  # Household extensive
  Opp_continuous_direct_zi = read.csv(here("Results","Opportunity decomposition continuous zi direct model.csv"))
  Opp_continuous_reduced_zi = read.csv(here("Results","Opportunity decomposition continuous zi reduced model.csv"))
  
  # Household intensive
  Opp_continuous_direct_cond = read.csv(here("Results","Opportunity decomposition continuous conditional direct model.csv"))
  Opp_continuous_reduced_cond = read.csv(here("Results","Opportunity decomposition continuous conditional reduced model.csv"))
  
  # Household total
  Opp_continuous_direct_res = read.csv(here("Results","Opportunity decomposition continuous response direct model.csv"))
  Opp_continuous_reduced_res = read.csv(here("Results","Opportunity decomposition continuous response reduced model.csv"))
  
  # Create and save plots using function call.
  source(here("Utilities", "plot_chart.R"))
  
  # INDIVIDUAL EXTENSIVE MODELS
  reference_value = Opp_binary_indiv_direct[Opp_binary_indiv_direct$X == "overall", contributions_of]
  plot_chart(Opp_binary_indiv_direct, 
             contributions_of, 
             "Individual sample", 
             "Binary participation", 
             "Direct model","opportunity",  
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
  
  
  # HOUSEHOLD EXTENSIVE MODELS
  reference_value = Opp_continuous_direct_zi[Opp_continuous_direct_zi$X == "overall", contributions_of]
  plot_chart(Opp_continuous_direct_zi, 
             contributions_of, 
             "Household sample", 
             "Continuous participation - zi", 
             "Direct model","opportunity",  
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
  
  # HOUSEHOLD EXTENSIVE MODELS
  reference_value = Opp_continuous_direct_cond[Opp_continuous_direct_cond$X == "overall", contributions_of]
  plot_chart(Opp_continuous_direct_cond, 
             contributions_of, 
             "Household sample", 
             "Continuous participation - conditional", 
             "Direct model","opportunity",  
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
  
  #HOUSEHOLD TOTAL MODELS
  reference_value = Opp_continuous_direct_res[Opp_continuous_direct_res$X == "overall", contributions_of]
  plot_chart(Opp_continuous_direct_res, 
             contributions_of, 
             "Household sample", 
             "Continuous participation - response", 
             "Direct model","opportunity",  
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
}

# END OF FUNCTION
