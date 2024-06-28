
#This function performs variable name adjustments for semantic differential style questions

clean_semantic_differential = function(data){
  
  #Agency colnames
  colnames(data)[which(names(data) == "AGENCY_1")] = "agency_can_achieve_goals"
  colnames(data)[which(names(data) == "AGENCY_2")] = "agency_can_overcome_challenges"
  colnames(data)[which(names(data) == "AGENCY_3")] = "agency_cannot_achieve_tasks"
  colnames(data)[which(names(data) == "AGENCY_4")] = "agency_overwhelmed_challenges"
  colnames(data)[which(names(data) == "AGENCY_5")] = "agency_expect_best_when_uncertain"
  
  # Empowerment colnames
  colnames(data)[which(names(data) == "EMPOWERMENT_1")] = "empower_women_right_inherit"
  colnames(data)[which(names(data) == "EMPOWERMENT_2")] = "empower_boys_sent_school_first"
  colnames(data)[which(names(data) == "EMPOWERMENT_3")] = "empower_women_free_divorce"
  colnames(data)[which(names(data) == "EMPOWERMENT_4")] = "empower_men_financial_control"
  colnames(data)[which(names(data) == "EMPOWERMENT_5")] = "empower_girls_secondary_school"
    
  # Attitudes
  colnames(data)[which(names(data) == "ATTITUDES_HIDING_1")] = "Att_hiding_necessary_avoid_arguments"
  colnames(data)[which(names(data) == "ATTITUDES_HIDING_2")] = "Att_hiding_truthful_builds_trust"
  colnames(data)[which(names(data) == "ATTITUDES_HIDING_3")] = "Att_hiding_makes_guilty"
  
  # PBC
  colnames(data)[which(names(data) == "PBC_HIDING_1")] = "PBC_others_allow_personal_spending"
  colnames(data)[which(names(data) == "PBC_HIDING_2")] = "PBC_need_justify_others"
  colnames(data)[which(names(data) == "PBC_HIDING_3")] = "PBC_worried_others_will_spend"

  # Norms
  colnames(data)[which(names(data) == "NORMS_HIDING_1")] = "Norms_argue_income_spouse"
  colnames(data)[which(names(data) == "NORMS_HIDING_2")] = "Norms_share_preferences"
  colnames(data)[which(names(data) == "NORMS_HIDING_3")] = "Norms_disappointed_others_spending"

  #Self control
  colnames(data)[which(names(data) == "SELF_CONTROL_1")] = "self_control_control_short_term_desires"
  colnames(data)[which(names(data) == "SELF_CONTROL_2")] = "self_control_impulsive_when_shopping"
  
  #risk
  colnames(data)[which(names(data) == "RISK_TAKING_1")] = "risk_taking_general"
  colnames(data)[which(names(data) == "RISK_TAKING_2")] = "risk_taking_farm_management"
    
  return(data)
}
