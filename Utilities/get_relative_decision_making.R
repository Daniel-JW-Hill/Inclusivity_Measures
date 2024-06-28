
#This function returns relative decision making for each individual relative to their spouse. 

get_relative_decision_making = function(data){
  
  names(data)[names(data) == 'DECISION_ROLES_1'] = 'DECISIONS_ROLE_MAJOR_EXPENDITURE'
  names(data)[names(data) == 'DECISION_ROLES_2'] = 'DECISIONS_ROLE_MINOR_EXPENDITURE'
  names(data)[names(data) == 'DECISION_ROLES_3'] = 'DECISIONS_ROLE_FOOD_PRODUCED'
  names(data)[names(data) == 'DECISION_ROLES_5'] = 'DECISIONS_ROLE_MAJOR_COFFEE'
  names(data)[names(data) == 'DECISION_ROLES_6'] = 'DECISIONS_ROLE_MINOR_COFFEE'
  
  #remove decision making accidentally included twice. 
  data = data[, !colnames(data) %in% c("DECISION_ROLES_4")]
  
  #Adjust one obs of no input to aggregate into single group
  data$DECISIONS_ROLE_MAJOR_COFFEE[data$DECISIONS_ROLE_MAJOR_COFFEE == "No input"] = "Occasional input" 
  
  data$DECISION_OCCASIONAL_MAJOR_COFFEE = ifelse(data$DECISIONS_ROLE_MAJOR_COFFEE == "Occasional input" , 1, 0)
  data$DECISION_EQUAL_MAJOR_COFFEE = ifelse(data$DECISIONS_ROLE_MAJOR_COFFEE == "Equal decision making" , 1, 0)
  data$DECISION_LEADER_MAJOR_COFFEE = ifelse(data$DECISIONS_ROLE_MAJOR_COFFEE == "Leader in decisions" , 1, 0)
  
  #Create disagreement flag - where roles are misaligned (either claim both are leaders, or one a leader and other equal decision making)
  data$DECISION_DISAGREEMENTS_COFFEE_MAJOR = rep(0, nrow(data))
  for (i in 1:nrow(data)){
    hh_idx = data$HHID[i]
    hh_data = subset(data, data$HHID == hh_idx)
    if (nrow(hh_data)>1){
      decision_role_1 = hh_data$DECISIONS_ROLE_MAJOR_COFFEE[1]
      decision_role_2 = hh_data$DECISIONS_ROLE_MAJOR_COFFEE[2]
      if (decision_role_1 == "Equal decision making" & decision_role_2 == "Equal decision making") {
        data$DECISION_DISAGREEMENTS_COFFEE_MAJOR[i] = 0
      } else if (decision_role_1 == "Leader in decisions" & decision_role_2 != "Occasional input") {
        data$DECISION_DISAGREEMENTS_COFFEE_MAJOR[i] = 1
      } else if (decision_role_2 == "Leader in decisions" & decision_role_1 != "Occasional input") {
        data$DECISION_DISAGREEMENTS_COFFEE_MAJOR[i] = 1
      } else if (decision_role_2 == "Occasional_input" & decision_role_1 == "Occasional input") {
        data$DECISION_DISAGREEMENTS_COFFEE_MAJOR[i] = 0
      }
    } else {
      data$DECISION_DISAGREEMENTS_COFFEE_MAJOR[i] = 0
    }
  }
  
  return(data)
   
}