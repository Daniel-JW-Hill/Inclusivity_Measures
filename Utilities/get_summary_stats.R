
#Saves summary stat tables for manuscript

get_summary_stats = function(hh_data, indiv_data){
  
  library(stargazer)
  
  # HH_data
  participating = subset(hh_data, BINARY_PARTICIPATION == 1)
  non_participating = subset(hh_data, BINARY_PARTICIPATION == 0)
  
  t_test_results = list()
   for (var_name in colnames(hh_data)) {
     if (sd(participating[[var_name]]) != 0 & sd(non_participating[[var_name]]) != 0) {
       t_test_results[[var_name]] <- t.test(participating[[var_name]], non_participating[[var_name]])$p.value
     } else {
       t_test_results[[var_name]] = NA
     }
   }
  
  p_values_df = data.frame(variable = names(t_test_results), p_value = unlist(t_test_results))
  write.csv(p_values_df, file = here("Results", "summary_pvalues_hhdata.csv"), row.names = FALSE)
  
  stargazer(participating, 
            summary = TRUE,
            type = "html", 
            title = "Summary statistics - hh model",
            out=here("Results","Summary statistics - HH model participate.html"))
  
  stargazer(non_participating, 
            summary = TRUE,
            type = "html", 
            title = "Summary statistics - hh model",
            out=here("Results","Summary statistics - HH model non participate.html"))
  
  
  # Indiv_data
  
  participating = subset(indiv_data, BINARY_PARTICIPATION == 1)
  non_participating = subset(indiv_data, BINARY_PARTICIPATION == 0)
  
  t_test_results <- list()
  for (var_name in colnames(indiv_data)) {
    if (sd(participating[[var_name]]) != 0 & sd(non_participating[[var_name]]) != 0) {
      t_test_results[[var_name]] <- t.test(participating[[var_name]], non_participating[[var_name]])$p.value
    } else {
      t_test_results[[var_name]] = NA
    }
  }
  
  p_values_df = data.frame(variable = names(t_test_results), p_value = unlist(t_test_results))
  write.csv(p_values_df, file = here("Results","summary_pvalues_indiv_data.csv"), row.names = FALSE)
  
  stargazer(participating, 
          summary = TRUE,
          type = "html", 
          title = "Summary statistics - indiv model",
          out=here("Results","Summary statistics - indiv model participate.html"))

  stargazer(non_participating, 
          summary = TRUE,
          type = "html", 
          title = "Summary statistics - indiv model",
          out=here("Results","Summary statistics - indiv model non participate.html"))
  
  
}