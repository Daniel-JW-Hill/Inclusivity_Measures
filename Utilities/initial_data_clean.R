# This function performs some initial cleans and checks to the Qualtrics 
# data file download, including fixes to household IDs, GPS values, 
# And structure of the dataframe. 

initial_data_clean = function(data){
  
  #Remove first row with repeated column names
  data = data[-1,]
  
  # Convert end date to new variable (for manipulation of data later)
  library(openxlsx)
  library(lubridate)
  data$date_completed = convertToDateTime(data$EndDate)

  #Remove first 17 columns as defaults from qualtrics system. 
  data = data[,-(1:17)]
  
  #Remove respondent names
  data = data[,-c(2)]
  
  #Remove redundant qualtrics conjoint data (not used for this study)
  data = data[, !colnames(data) %in% c('C1','C2','C3','C4','C5','C6','C7','C8')]
  data = data[, !grepl("CBCONJOINT", colnames(data))]
  
  #Remove HHID 9999 (test observation)
  data = data[-which(data$HHID == 9999),]
  
  #Fix some HHID typos
  data$HHID[data$HHID==7724] = 7742
  data$HHID[data$HHID==9202] = 9702
  data$HHID[data$HHID==4024] = 4021
  data$HHID[data$HHID==4804] = 4904
  data$HHID[data$HHID==5314] = 1537
  data$HHID[which(data$HHID==8626 & data$VILLAGE == 'Cheptilyal')] = 3962
  
  # Create a new variable defining two observations for HH or one. 
  # Two obvs say 'married' but only one obvs. Both likely widowed :(. 
  n_hhmember_obs = data.frame(ID = names(table(data$HHID)), Result = as.vector(table(data$HHID)))
  data$hh_member_observations  = n_hhmember_obs$Result[match(data$HHID, n_hhmember_obs$ID)]
  
  return(data)
}
