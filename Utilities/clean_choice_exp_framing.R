
#This function cleans up the framing questions for the choice experiment. 

clean_choice_exp_framing = function(data){
  
  data$SAVE_COFF_INC_YESNO[data$SAVE_COFF_INC_YESNO == "Yes"] = 1
  data$SAVE_COFF_INC_YESNO[data$SAVE_COFF_INC_YESNO == "No"] = 0
  data$SAVE_COFF_INC_YESNO = as.numeric(data$SAVE_COFF_INC_YESNO)
  
  data$SAVE_COFFEE_FORWHAT[data$SAVE_COFFEE_FORWHAT == "Other"] = "School fees, uniforms, and materials" #They flag multiple things, but pick first one they mention
  
  data = data[,-which(names(data) == "SAVE_COFFEE_FORWHAT_9_TEXT")]
  
  data$SAVE_COFFEE_GOAL[is.na(data$SAVE_COFFEE_GOAL)] = 0
  data$SAVE_COFFEE_GOAL = as.numeric(data$SAVE_COFFEE_GOAL)
  data$SAVE_COFFEE_GOAL[data$SAVE_COFFEE_GOAL<100] = data$SAVE_COFFEE_GOAL[data$SAVE_COFFEE_GOAL<100] * 1000000
  data$SAVE_COFFEE_GOAL[data$SAVE_COFFEE_GOAL<10000] = data$SAVE_COFFEE_GOAL[data$SAVE_COFFEE_GOAL<10000] * 100
  data$SAVE_COFFEE_GOAL[data$SAVE_COFFEE_GOAL<100000] = data$SAVE_COFFEE_GOAL[data$SAVE_COFFEE_GOAL<100000] * 10
  
  data$SAVE_COFFEE_GOAL_YN[data$SAVE_COFFEE_GOAL_YN == "You believe you will meet this goal"] = 1
  data$SAVE_COFFEE_GOAL_YN[data$SAVE_COFFEE_GOAL_YN == "Yes, you met this goal"] = 1
  data$SAVE_COFFEE_GOAL_YN[is.na(data$SAVE_COFFEE_GOAL_YN)] = "Not applicable"
  data$SAVE_COFFEE_GOAL_YN[data$SAVE_COFFEE_GOAL_YN  == "No, you will not or have not met this goal"] = 0
  
  
  colnames(data)[names(data) == "Choicecard 6"] = "Choicecard6"
  
  return(data)
}






                   

