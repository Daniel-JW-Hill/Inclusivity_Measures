#This function performs some simple manipulations and data cleaning for household characteristics. 

clean_characteristics = function(data){
  
  #Change gender variable to female flag
  data$GENDER = ifelse(data$GENDER == "Female", 1, 0)
  names(data)[names(data) == 'GENDER'] <- 'FEMALE'
  
  #Create household head flag
  data$RESPONDENT_ID= ifelse(data$RESPONDENT_ID == "Household head", 1, 0)
  names(data)[names(data) == 'RESPONDENT_ID'] <- 'HOUSEHOLD_HEAD'
  
  data$HOUSEHOLD_HEAD[data$HHID == 9758 & data$FEMALE == 1] = 0 # one obs where both respondents are flagged as hhead. 
  
  #Create household head flag
  data$AGE = as.numeric(data$AGE)
  
  #Create flags for marriage
  data$MARRIED_MONOGAMOUS = ifelse(data$MARITAL_STATUS == "Married, monogamously", 1,0)
  data$MARRIED_POLYGAMOUS = ifelse(data$MARITAL_STATUS == "Married, polygamous", 1,0)
  data$SINGLE = ifelse(data$MARITAL_STATUS == "Divorced, separated, or widowed", 1,0)
  data$YEARS_MARRIED = as.numeric(data$YEARS_MARRIED)
  
  #Religion flags
  data$CHRISTIAN = ifelse(data$RELIGION == "Anglican" | data$RELIGION == "Pentecostal" | data$RELIGION == "Roman catholic" | data$RELIGION == "Other", 1, 0)
  data$MUSLIM = ifelse(data$RELIGION == "Muslim", 1, 0)
  
  #Born in village
  data$BORN_IN_VILLAGE = ifelse(data$BORN_IN_VILLAGE == "Yes",1,0)
  data$YEARS_INVILLAGE = ifelse(data$BORN_IN_VILLAGE == 1, data$AGE, data$YEARS_INVILLAGE)
  
  #Education - create approx continuous measure. 
  data$EDUCATION = ifelse(data$EDUCATION_CATEGORY == "None", 0, NA)
  data$EDUCATION[data$EDUCATION_CATEGORY == "Nursery"] = 1
  data$EDUCATION[data$EDUCATION_CATEGORY == "P1-P4"] = 4
  data$EDUCATION[data$EDUCATION_CATEGORY == "P5-P7"] = 7
  data$EDUCATION[data$EDUCATION_CATEGORY == "S1-S4"] = 9
  data$EDUCATION[data$EDUCATION_CATEGORY == "S5-S6"] = 12
  data$EDUCATION[data$EDUCATION_CATEGORY == "Tertiary/University"] = 16
  data$EDUCATION[data$EDUCATION_CATEGORY == "Vocational"] = 15
  data$EDUCATION[data$EDUCATION_CATEGORY == "Religious"] = 4
  
  #Dependency ratios and children
  data$CHILDREN_UNDER_12 = as.numeric(data$CHILDREN_UNDER_12)
  data$CHILDREN_OVER_12 = as.numeric(data$CHILDREN_OVER_12)
  data$CHILDREN_SCHOOL = as.numeric(data$CHILDREN_SCHOOL)
  data$HH_MALE_ADULTS = as.numeric(data$HH_MALE_ADULTS)
  data$HH_FEMALE_ADULTS = as.numeric(data$HH_FEMALE_ADULTS)
  data$HH_MALE_WORKING = as.numeric(data$HH_MALE_WORKING)
  data$HH_FEMALE_WORKING = as.numeric(data$HH_FEMALE_WORKING) 
  
  data$HH_MALE_WORKING[is.na(data$HH_MALE_WORKING) == TRUE] = 0
  data$HH_FEMALE_WORKING[is.na(data$FEMALE_WORKING) == TRUE] = 0
  
  data$HH_MALE_ADULTS = ifelse(data$SINGLE == 1, ifelse(data$FEMALE == 1, 
                               data$HH_MALE_ADULTS, data$HH_MALE_ADULTS+1), data$HH_MALE_ADULTS+1)
  data$HH_FEMALE_ADULTS = ifelse(data$SINGLE == 1, ifelse(data$FEMALE == 0, 
                                                        data$HH_FEMALE_ADULTS, data$HH_FEMALE_ADULTS+1), data$HH_FEMALE_ADULTS+1)
  
  data$HH_MALE_WORKING = ifelse(data$SINGLE == 1, ifelse(data$FEMALE == 1, 
                                                        data$HH_MALE_WORKING, data$HH_MALE_WORKING+1), data$HH_MALE_WORKING+1)
  data$HH_FEMALE_WORKING = ifelse(data$SINGLE == 1, ifelse(data$FEMALE == 0, 
                                                          data$HH_FEMALE_WORKING, data$HH_FEMALE_WORKING+1), data$HH_FEMALE_WORKING+1)
  
  data$HH_MALE_WORKING = ifelse(data$HH_MALE_ADULTS < data$HH_MALE_WORKING, data$HH_MALE_WORKING - 1, data$HH_MALE_WORKING)
  data$HH_FEMALE_WORKING = ifelse(data$HH_FEMALE_ADULTS < data$HH_FEMALE_WORKING, data$HH_FEMALE_WORKING - 1, data$HH_FEMALE_WORKING)
  
  data$N_ADULTS = rowSums(cbind(data$HH_MALE_ADULTS, data$HH_FEMALE_ADULTS) , na.rm=TRUE)
  data$N_ADULTS_WORKING  = rowSums(cbind(data$HH_MALE_WORKING, data$HH_FEMALE_WORKING), na.rm=TRUE)
  
  data$N_DEPENDENTS_ADULTS = data$N_ADULTS - data$N_ADULTS_WORKING
 
  data$DEPENDENCY_RATIO = rowSums(cbind(data$CHILDREN_UNDER_12, data$CHILDREN_OVER_12, data$N_DEPENDENTS_ADULTS), na.rm = TRUE) / data$N_ADULTS_WORKING
  
  # Social participation
  # Binary indicators
  data$SOCIAL_PARTICIPATE = ifelse(data$SOCIAL_PARTICIPATE == "An active member" | data$SOCIAL_PARTICIPATE == "A Leader.", 1,0)
  data$COOP_PARTICIPATE = ifelse(data$COOP_PARTICIPATE == "An active member" | data$COOP_PARTICIPATE == "A Leader.", 1,0)
  data$SOCIAL_PARTICIPATION_OVERALL = ifelse(data$SOCIAL_PARTICIPATE == 1 | data$COOP_PARTICIPATE == 1, 1, 0 )
  
  #Continuous measures. 
  data$VILLAGE_ADULTS = gsub(",", "", data$VILLAGE_ADULTS)
  data$VILLAGE_FRIENDS = as.numeric(data$VILLAGE_FRIENDS)
  data$VILLAGE_ADULTS = as.numeric(data$VILLAGE_ADULTS)
  data$VILLAGE_FRIENDS_PROPORTION = round(data$VILLAGE_FRIENDS / data$VILLAGE_ADULTS, digits = 2)
  data$VILLAGE_FRIENDS[data$VILLAGE_FRIENDS > data$VILLAGE_ADULTS] = data$VILLAGE_ADULTS[data$VILLAGE_FRIENDS > data$VILLAGE_ADULTS]
  data$VILLAGE_FRIENDS_PROPORTION[data$VILLAGE_FRIENDS_PROPORTION > 1] = 1
  
  #Social measures clean up
  data$FRIENDS_LOANTO = as.numeric(data$FRIENDS_LOANTO)
  data$FRIENDS_RECEIVE = as.numeric(data$FRIENDS_RECEIVE)
  
  
  return(data)
}
