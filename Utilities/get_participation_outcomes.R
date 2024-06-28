
#This function creates participation and value chain competition variables

get_participation_outcomes = function(data){
  
  #Binary participation - defined as any proportion of coffee sold to any other
  #buyer than the local commodity buyer Kawakom or a local trader. 
  
  library(tidyr)
  library(dplyr)
  
  # Split the COFFEE_BUYER_SOLD column into separate rows
  data_split = data %>%
    separate_rows(COFFEE_BUYERS_SOLD, sep = ",") %>%
    mutate(flag = 1)  # Create a flag column with value 1
  
  # Pivot the data to wide format to get binary flag columns for each unique buyer
  data_flags = data_split %>%
    pivot_wider(names_from = COFFEE_BUYERS_SOLD, values_from = flag, values_fill = 0)
  
  # Merge the new flag columns back into the original data frame
  data = cbind(data, data_flags[,c(ncol(data):ncol(data_flags))])
  
  data$binary_participation_indiv = rep(0,nrow(data))
  for (i in 1:nrow(data)){
    participation = sum(data$`Great Lakes`[i], 
                        data$`OLAM coffee`[i],
                        data$`Other:`[i],
                        data$`A farmer group/association`[i],
                        data$`Intersection Traders`[i])
    if (participation > 0){
      data$binary_participation_indiv[i] = 1
    }
  }
  
  #Now get measure for household
  data$binary_participation_hh = rep(0,nrow(data))
  for (i in 1:nrow(data)){
    participation_hh = data$binary_participation_indiv[which(data$HHID == data$HHID[i])]
    if (sum(participation_hh) > 0){
      data$binary_participation_hh[i] = 1
    }
  }
  
  #Continuous participation - defined as the proportion of marketed coffee,  
  #parchment included, sold to any buyer other than the local commodity buyer 
  #or a local trader.
  
  # First total household marketed coffee
  # We define this as the sum of marketed coffee reported individually. 
  data$continuous_participation_hhead = rep(0,nrow(data))
  data$continuous_participation_spouse = rep(0,nrow(data))
  data$continuous_participation_average = rep(0,nrow(data))
  
  for (i in 1:nrow(data)){
    data_subset = data[which(data$HHID == data$HHID[i]),]
    hhead_idx = which(data_subset$RESPONDENT_ID == "Household head")
    spouse_idx = which(data_subset$RESPONDENT_ID != "Household head")
    
    sum_kgs_sold_hhead = sum(as.numeric(data_subset$KGS_SOLD_HH_GREATLAKES[hhead_idx]),
                             as.numeric(data_subset$KGS_SOLD_HH_INTERSECTION[hhead_idx]),
                             as.numeric(data_subset$KGS_SOLD_HH_OLAM[hhead_idx]),
                             as.numeric(data_subset$KGS_SOLD_HH_FARMERGROUP[hhead_idx]),
                             as.numeric(data_subset$KGS_SOLD_HH_OTHER[hhead_idx]),
                             na.rm = TRUE)
    
    data$continuous_participation_hhead[i] = ifelse(sum_kgs_sold_hhead == 0, 0, sum_kgs_sold_hhead/data$total_marketed_coffee_hhead[i])
    
    sum_kgs_sold_spouse = sum(as.numeric(data_subset$KGS_SOLD_HH_GREATLAKES[spouse_idx]),
                             as.numeric(data_subset$KGS_SOLD_HH_INTERSECTION[spouse_idx]),
                             as.numeric(data_subset$KGS_SOLD_HH_OLAM[spouse_idx]),
                             as.numeric(data_subset$KGS_SOLD_HH_FARMERGROUP[spouse_idx]),
                             as.numeric(data_subset$KGS_SOLD_HH_OTHER[spouse_idx]),
                             na.rm = TRUE)
    
    data$continuous_participation_spouse[i] = ifelse(sum_kgs_sold_spouse == 0, 0, sum_kgs_sold_spouse/data$total_marketed_coffee_spouse[i])
    
    data$continuous_participation_average[i] =  sum(data$continuous_participation_hhead[i], 
                                                     data$continuous_participation_spouse[i])/nrow(data_subset)
    
    
  }
  
  #### Finally, create metric for level of buyer competition. ####
  #Count the commas in the variable, plus one, gives the number of buyers. 
  num_buyers_option  =  sapply(data$COFFEE_BUYERS_OPTION, function(x) sum(gregexpr(",", x)[[1]] >= 0)) + 1
  names(num_buyers_option) = NULL
  data$num_buyers_option = num_buyers_option 

  return(data)
}
