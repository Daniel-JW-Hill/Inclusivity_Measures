
# This function returns land related information
# Including coffee trees, tenure, and coffee management behaviour. 

clean_land = function(data){
  
  # Tenure proportion 
  data$RENTED_PROPORTION = data$LAND_RENTED_ESTIMATED / data$LAND_TOTAL_ESTIMATED
  
  # Coffee trees and coffee trees squared ('000s)
  data$COFFEE_TREES_N = data$COFFEE_TREES_N/1000
  data$COFFEE_TREES_N_SQ = data$COFFEE_TREES_N ^ 2
  
  # Management practices
  data$COFFEE_TREE_PRUNED = ifelse(data$COFFEE_TREE_PRUNED != "No", 1, 0)
  data$COFFEE_CHEMICALS = ifelse(data$COFFEE_CHEMICALS != "No", 1, 0)
  data$COFFEE_SOILINPUTS = ifelse(data$COFFEE_CHEMICALS != "No", 1, 0)
  data$COFFEE_SHADED = ifelse(data$COFFEE_SHADED != "Yes", 1, 0) #Define partially as not shaded. 

  #Coffee tree age
  data$COFFEE_TREE_AGE = ifelse(data$COFFEE_TREE_AGE == "More than 12 years old", 1, 0)
  names(data)[names(data) == 'COFFEE_TREE_AGE'] <- 'COFFEE_TREE_AGE_OLDER_TWELVE'
  
  return(data)
  
}