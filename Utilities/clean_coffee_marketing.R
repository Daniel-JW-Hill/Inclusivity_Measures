
# This function performs checks on coffee marketing behaviour/yields  
# Significant irregularities are addressed where appropriate to do so

clean_coffee_marketing = function(data){
  
  #Adjust plot minutes to numeric
  data$COFFEE_PLOT_MINUTES = as.numeric(data$COFFEE_PLOT_MINUTES)
  
  #remove unnecessary variables
  to_remove =   c("COFF_BUYER_FREQ_RESP_8_TEXT",
                  "COFF_BUYER_FREQ_SP_8_TEXT" ,
                  "COFF_BUYER_FREQ_RESP_9_1",
                  "COFF_BUYER_FREQ_SP_9_2",
                  "COFF_BUYER_FREQ_RESP_9_1",
                  "COFFEE_BUYERS_KGS_8_TEXT",
                  "COFFEE_BUYERS_KGS_9_1",
                  "COFFEE_BUYERS_PRICE_9_2") 
  data = data[,-which(names(data) %in% to_remove)]
  
  # Rename columns for better reference
  colnames(data)[colnames(data) == "COFFEE_BUYERS_KGS_1_1"] <- "KGS_SOLD_HH_KAWAKOM"
  colnames(data)[colnames(data) == "COFFEE_BUYERS_KGS_2_1"] <- "KGS_SOLD_HH_GREATLAKES"
  colnames(data)[colnames(data) == "COFFEE_BUYERS_KGS_3_1"] <- "KGS_SOLD_HH_KYAKALANYI"
  colnames(data)[colnames(data) == "COFFEE_BUYERS_KGS_4_1"] <- "KGS_SOLD_HH_INTERSECTION"
  colnames(data)[colnames(data) == "COFFEE_BUYERS_KGS_5_1"] <- "KGS_SOLD_HH_FARMERGROUP"
  colnames(data)[colnames(data) == "COFFEE_BUYERS_KGS_6_1"] <- "KGS_SOLD_HH_TRADER"
  colnames(data)[colnames(data) == "COFFEE_BUYERS_KGS_7_1"] <- "KGS_SOLD_HH_OLAM"
  colnames(data)[colnames(data) == "COFFEE_BUYERS_KGS_8_1"] <- "KGS_SOLD_HH_OTHER"
  
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_RESP_1_1"] <- "FREQ_SOLD_INDIV_KAWAKOM"
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_RESP_2_1"] <- "FREQ_SOLD_INDIV_GREATLAKES"
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_RESP_3_1"] <- "FREQ_SOLD_INDIV_KYAKALANYI"
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_RESP_4_1"] <- "FREQ_SOLD_INDIV_INTERSECTION"
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_RESP_5_1"] <- "FREQ_SOLD_INDIV_FARMERGROUP"
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_RESP_6_1"] <- "FREQ_SOLD_INDIV_TRADER"
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_RESP_7_1"] <- "FREQ_SOLD_INDIV_OLAM"
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_RESP_8_1"] <- "FREQ_SOLD_INDIV_OTHER"
  
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_SP_1_2"] <- "FREQ_SOLD_OTHERS_KAWAKOM"
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_SP_2_2"] <- "FREQ_SOLD_OTHERS_GREATLAKES"
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_SP_3_2"] <- "FREQ_SOLD_OTHERS_KYAKALANYI"
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_SP_4_2"] <- "FREQ_SOLD_OTHERS_INTERSECTION"
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_SP_5_2"] <- "FREQ_SOLD_OTHERS_FARMERGROUP"
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_SP_6_2"] <- "FREQ_SOLD_OTHERS_TRADER"
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_SP_7_2"] <- "FREQ_SOLD_OTHERS_OLAM"
  colnames(data)[colnames(data) == "COFF_BUYER_FREQ_SP_8_2"] <- "FREQ_SOLD_OTHERS_OTHER"
  
  colnames(data)[colnames(data) == "COFFEE_BUYERS_PRICE_1_2"] <- "PRICE_HH_KAWAKOM"
  colnames(data)[colnames(data) == "COFFEE_BUYERS_PRICE_2_2"] <- "PRICE_HH_GREATLAKES"
  colnames(data)[colnames(data) == "COFFEE_BUYERS_PRICE_3_2"] <- "PRICE_HH_KYAKALANYI"
  colnames(data)[colnames(data) == "COFFEE_BUYERS_PRICE_4_2"] <- "PRICE_HH_INTERSECTION"
  colnames(data)[colnames(data) == "COFFEE_BUYERS_PRICE_5_2"] <- "PRICE_HH_FARMERGROUP"
  colnames(data)[colnames(data) == "COFFEE_BUYERS_PRICE_6_2"] <- "PRICE_HH_TRADER"
  colnames(data)[colnames(data) == "COFFEE_BUYERS_PRICE_7_2"] <- "PRICE_HH_OLAM"
  colnames(data)[colnames(data) == "COFFEE_BUYERS_PRICE_8_2"] <- "PRICE_HH_OTHER"
  
  #Edit non numeric values in coffee parchment kgs and price
  data$COFFEE_PARCH_KGS[data$COFFEE_PARCH_KGS == "300kgs"] = 300
  data$COFFEE_PARCH_KGS = gsub(",", "", data$COFFEE_PARCH_KGS)
  data$COFFEE_PARCH_KGS = as.numeric(data$COFFEE_PARCH_KGS)
  
  data$COFFEE_PARCH_PRICE = gsub(",", "", data$COFFEE_PARCH_PRICE)
  data$COFFEE_PARCH_PRICE = as.numeric(data$COFFEE_PARCH_PRICE)
  
  #Edit large price as appears to be revenues. 
  data$COFFEE_PARCH_PRICE[which(data$COFFEE_PARCH_PRICE == 5700000)] = ceiling(data$COFFEE_PARCH_PRICE[which(data$COFFEE_PARCH_PRICE == 5700000)] / data$COFFEE_PARCH_KGS[which(data$COFFEE_PARCH_PRICE == 5700000)])
  
  #Edit prices under 1000 as missing zeros. Parchment prices will always be 1000 at the very least. 
  data$COFFEE_PARCH_PRICE[which(data$COFFEE_PARCH_PRICE == 1)] = 1000
  data$COFFEE_PARCH_PRICE[which(data$COFFEE_PARCH_PRICE == 8)] = 8000
  data$COFFEE_PARCH_PRICE[which(data$COFFEE_PARCH_PRICE == 100)] = 1000
  data$COFFEE_PARCH_PRICE[which(data$COFFEE_PARCH_PRICE == 200)] = 2000
  data$COFFEE_PARCH_PRICE[which(data$COFFEE_PARCH_PRICE == 700)] = 7000
  data$COFFEE_PARCH_PRICE[which(data$COFFEE_PARCH_PRICE == 800)] = 8000
  data$COFFEE_PARCH_PRICE[which(data$COFFEE_PARCH_PRICE == 950)] = 8000
  
  #Edit down parchment kgs for hhs where they report too high (i.e. 10 times more than reported yield)
  #Both order of magnitude too high, and over report cherries vs parchment. 
  data$COFFEE_PARCH_KGS[which(data$HHID == 9821 & data$RESPONDENT_ID == "Spouse of household head")] = data$COFFEE_PARCH_KGS[which(data$HHID == 9821 & data$RESPONDENT_ID == "Spouse of household head")] /50
  data$COFFEE_PARCH_KGS[which(data$HHID == 4242 & data$RESPONDENT_ID == "Household head")] = data$COFFEE_PARCH_KGS[which(data$HHID == 4242 & data$RESPONDENT_ID == "Household head")] /50
  data$COFFEE_PARCH_KGS[which(data$HHID == 4211 & data$RESPONDENT_ID == "Spouse of household head")] = data$COFFEE_PARCH_KGS[which(data$HHID == 4211 & data$RESPONDENT_ID == "Spouse of household head")] /50
  data$COFFEE_PARCH_KGS[which(data$HHID == 8804 & data$RESPONDENT_ID == "Household head")] = data$COFFEE_PARCH_KGS[which(data$HHID == 8804 & data$RESPONDENT_ID == "Household head")] /50
  data$COFFEE_PARCH_KGS[which(data$HHID == 1506 & data$RESPONDENT_ID == "Spouse of household head")] = data$COFFEE_PARCH_KGS[which(data$HHID == 1506 & data$RESPONDENT_ID == "Spouse of household head")] /50
  data$COFFEE_PARCH_KGS[which(data$HHID == 9867 & data$RESPONDENT_ID == "Household head")] = data$COFFEE_PARCH_KGS[which(data$HHID == 9867 & data$RESPONDENT_ID == "Household head")] /50
  data$COFFEE_PARCH_KGS[which(data$HHID == 4384 & data$RESPONDENT_ID == "Household head")] = data$COFFEE_PARCH_KGS[which(data$HHID == 4384 & data$RESPONDENT_ID == "Household head")] /50
  
  # Add prices to parchment if they marketed coffee as parchment. Average of sample
  # Even if they did not sell them for whatever reason, represents an expected opportunity cost. Only one observation. 
  data$COFFEE_PARCH_PRICE[which(data$COFFEE_PARCH_KGS > 0 & data$COFFEE_PARCH_PRICE == 0)] = ceiling(mean(data$COFFEE_PARCH_PRICE, na.rm = TRUE))
  data$COFFEE_PARCH_PRICE[which(data$COFFEE_PARCH_KGS > 0 & is.na(data$COFFEE_PARCH_PRICE) == TRUE)] = ceiling(mean(data$COFFEE_PARCH_PRICE, na.rm = TRUE))

  # Now we run checks on marketed volumes to each stream of buyer. 
  # And prices for each. 
  data$KGS_SOLD_HH_KAWAKOM = as.numeric(data$KGS_SOLD_HH_KAWAKOM)
  data$KGS_SOLD_HH_GREATLAKES = as.numeric(data$KGS_SOLD_HH_GREATLAKES)
  data$KGS_SOLD_HH_KYAKALANYI = as.numeric(data$KGS_SOLD_HH_KYAKALANYI)
  data$KGS_SOLD_HH_INTERSECTION = as.numeric(data$KGS_SOLD_HH_INTERSECTION)
  data$KGS_SOLD_HH_FARMERGROUP = as.numeric(data$KGS_SOLD_HH_FARMERGROUP)
  data$KGS_SOLD_HH_TRADER= as.numeric(data$KGS_SOLD_HH_TRADER)
  data$KGS_SOLD_HH_OLAM = as.numeric(data$KGS_SOLD_HH_OLAM)
  data$KGS_SOLD_HH_OTHER = as.numeric(data$KGS_SOLD_HH_OTHER)
  
  data$PRICE_HH_KAWAKOM = as.numeric(data$PRICE_HH_KAWAKOM)
  data$PRICE_HH_GREATLAKES = as.numeric(data$PRICE_HH_GREATLAKES)
  data$PRICE_HH_KYAKALANYI = as.numeric(data$PRICE_HH_KYAKALANYI)
  data$PRICE_HH_INTERSECTION = as.numeric(data$PRICE_HH_INTERSECTION )
  data$PRICE_HH_FARMERGROUP = as.numeric(data$PRICE_HH_FARMERGROUP)
  data$PRICE_HH_TRADER = as.numeric(data$PRICE_HH_TRADER)
  data$PRICE_HH_OLAM = as.numeric(data$PRICE_HH_OLAM)
  data$PRICE_HH_OTHER = as.numeric(data$PRICE_HH_OTHER)
  
  data$FREQ_SOLD_INDIV_KAWAKOM = as.numeric(data$FREQ_SOLD_INDIV_KAWAKOM)
  data$FREQ_SOLD_INDIV_GREATLAKES = as.numeric(data$FREQ_SOLD_INDIV_GREATLAKES)
  data$FREQ_SOLD_INDIV_KYAKALANYI = as.numeric(data$FREQ_SOLD_INDIV_KYAKALANYI)
  data$FREQ_SOLD_INDIV_INTERSECTION = as.numeric(data$FREQ_SOLD_INDIV_INTERSECTION)
  data$FREQ_SOLD_INDIV_FARMERGROUP = as.numeric(data$FREQ_SOLD_INDIV_FARMERGROUP)
  data$FREQ_SOLD_INDIV_TRADER = as.numeric(data$FREQ_SOLD_INDIV_TRADER)
  data$FREQ_SOLD_INDIV_OLAM = as.numeric(data$FREQ_SOLD_INDIV_OLAM)
  data$FREQ_SOLD_INDIV_OTHER = as.numeric(data$FREQ_SOLD_INDIV_OTHER)
  
  data$FREQ_SOLD_OTHERS_KAWAKOM = as.numeric(data$FREQ_SOLD_OTHERS_KAWAKOM)
  data$FREQ_SOLD_OTHERS_GREATLAKES = as.numeric(data$FREQ_SOLD_OTHERS_GREATLAKES)
  data$FREQ_SOLD_OTHERS_KYAKALANYI = as.numeric(data$FREQ_SOLD_OTHERS_KYAKALANYI)
  data$FREQ_SOLD_OTHERS_INTERSECTION = as.numeric(data$FREQ_SOLD_OTHERS_INTERSECTION)
  data$FREQ_SOLD_OTHERS_FARMERGROUP = as.numeric(data$FREQ_SOLD_OTHERS_FARMERGROUP)
  data$FREQ_SOLD_OTHERS_TRADER = as.numeric(data$FREQ_SOLD_OTHERS_TRADER)
  data$FREQ_SOLD_OTHERS_OLAM = as.numeric(data$FREQ_SOLD_OTHERS_OLAM)
  data$FREQ_SOLD_OTHERS_OTHER = as.numeric(data$FREQ_SOLD_OTHERS_OTHER)
  
  #If reporting price above 5000 - check if parchment is reported and if so drop (as reported later)
  #Only relevant for Kawakom and traders where parchment is expected. 
  for (i in 1:nrow(data)){
    if (data$PRICE_HH_KAWAKOM[i] > 5000 & is.na(data$PRICE_HH_KAWAKOM[i]) == FALSE){
      data$PRICE_HH_KAWAKOM[i] = data$COFFEE_PARCH_PRICE[i]/5
      data$KGS_SOLD_HH_KAWAKOM[i] = data$COFFEE_PARCH_KGS[i]
    }
  }
    
  for (i in 1:nrow(data)){
    if (data$PRICE_HH_TRADER[i] > 5000 & is.na(data$PRICE_HH_TRADER[i]) == FALSE){
      price = data$PRICE_HH_TRADER[i] 
      kgs = data$KGS_SOLD_HH_TRADER[i] 
      data$PRICE_HH_TRADER[i] = NA
      data$KGS_SOLD_HH_TRADER[i] = NA
      if (data$COFFEE_PARCH_KGS[i] == 0 &  is.na(data$COFFEE_PARCH_KGS[i]) == FALSE) {
        data$COFFEE_PARCH_KGS[i] = kgs
        data$COFFEE_PARCH_PRICE[i] = price
      } 
    }
  }
  
  # Now loop through all coffee buyers.
  # If report kgs but no price, report mean price
  for (i in 1:nrow(data)){
    if (data$PRICE_HH_KAWAKOM[i] == 0 & is.na(data$PRICE_HH_KAWAKOM[i]) ==FALSE){
      if (data$KGS_SOLD_HH_KAWAKOM[i] > 0 & is.na(data$KGS_SOLD_HH_KAWAKOM[i]) ==FALSE){
        data$PRICE_HH_KAWAKOM[i] = mean(data$PRICE_HH_KAWAKOM, na.rm = TRUE)
      }
    }
    
    if (data$PRICE_HH_GREATLAKES[i] == 0 & is.na(data$PRICE_HH_GREATLAKES[i]) ==FALSE){
      if (data$KGS_SOLD_HH_GREATLAKES[i] > 0 & is.na(data$KGS_SOLD_HH_GREATLAKES[i]) ==FALSE){
        data$PRICE_HH_GREATLAKES[i] = mean(data$PRICE_HH_GREATLAKES, na.rm = TRUE)
      }
    }
    
    if (data$PRICE_HH_KYAKALANYI[i] == 0 & is.na(data$PRICE_HH_KYAKALANYI[i]) ==FALSE){
      if (data$KGS_SOLD_HH_KYAKALANYI[i] > 0 & is.na(data$KGS_SOLD_HH_KYAKALANYI[i]) ==FALSE){
        data$PRICE_HH_KYAKALANYI[i] = mean(data$PRICE_HH_KYAKALANYI, na.rm = TRUE)
      }
    }
    
    if (data$PRICE_HH_INTERSECTION[i] == 0 & is.na(data$PRICE_HH_INTERSECTION[i]) ==FALSE){
      if (data$KGS_SOLD_HH_INTERSECTION[i] > 0 & is.na(data$KGS_SOLD_HH_INTERSECTION[i]) ==FALSE){
        data$PRICE_HH_INTERSECTION[i] = mean(data$PRICE_HH_INTERSECTION, na.rm = TRUE)
      }
    }
    
    if (data$PRICE_HH_FARMERGROUP[i] == 0 & is.na(data$PRICE_HH_FARMERGROUP[i]) ==FALSE){
      if (data$KGS_SOLD_HH_FARMERGROUP[i] > 0 & is.na(data$KGS_SOLD_HH_FARMERGROUP[i]) ==FALSE){
        data$PRICE_HH_FARMERGROUP[i] = mean(data$PRICE_HH_FARMERGROUP, na.rm = TRUE)
      }
    }
    
    if (data$PRICE_HH_TRADER[i] == 0 & is.na(data$PRICE_HH_TRADER[i]) ==FALSE){
      if (data$KGS_SOLD_HH_TRADER[i] > 0 & is.na(data$KGS_SOLD_HH_TRADER[i]) ==FALSE){
        data$PRICE_HH_TRADER[i] = mean(data$PRICE_HH_TRADER, na.rm = TRUE)
      }
    }
    
    if (data$PRICE_HH_OLAM[i] == 0 & is.na(data$PRICE_HH_OLAM[i]) ==FALSE){
      if (data$KGS_SOLD_HH_OLAM[i] > 0 & is.na(data$KGS_SOLD_HH_OLAM[i]) ==FALSE){
        data$PRICE_HH_OLAM[i] = mean(data$PRICE_HH_OLAM, na.rm = TRUE)
      }
    }
    
    if (data$PRICE_HH_OTHER[i] == 0 & is.na(data$PRICE_HH_OTHER[i]) ==FALSE){
      if (data$KGS_SOLD_HH_OTHER[i] > 0 & is.na(data$KGS_SOLD_HH_OTHER[i]) ==FALSE){
        data$PRICE_HH_OTHER[i] = mean(data$PRICE_HH_OTHER, na.rm = TRUE)
      }
    }
    
  }
  
  # Now repeat for others in the household. 
  for (i in 1:nrow(data)){
    if (data$FREQ_SOLD_INDIV_KAWAKOM[i] >= 50 & is.na(data$FREQ_SOLD_INDIV_KAWAKOM[i]) == FALSE){
      if (data$FREQ_SOLD_OTHERS_KAWAKOM[i]>0 & data$FREQ_SOLD_OTHERS_KAWAKOM[i]<50 & is.na(data$FREQ_SOLD_INDIV_KAWAKOM[i]) == FALSE) {
      data$FREQ_SOLD_INDIV_KAWAKOM[i] = data$FREQ_SOLD_OTHERS_KAWAKOM[i]
    } else if (data$KGS_SOLD_HH_KAWAKOM[i] / 80 <= 50) {
      data$FREQ_SOLD_INDIV_KAWAKOM[i] = data$KGS_SOLD_HH_KAWAKOM[i] / 80
    } else { 
      data$FREQ_SOLD_INDIV_KAWAKOM[i] = data$KGS_SOLD_HH_KAWAKOM[i] / 240
    }
    }
    
    if (data$FREQ_SOLD_INDIV_GREATLAKES[i] >= 50 & is.na(data$FREQ_SOLD_INDIV_GREATLAKES[i]) == FALSE){
      if (data$FREQ_SOLD_OTHERS_GREATLAKES[i]>0 & data$FREQ_SOLD_OTHERS_GREATLAKES[i]<50 & is.na(data$FREQ_SOLD_INDIV_GREATLAKES[i]) == FALSE){
        data$FREQ_SOLD_INDIV_GREATLAKES[i] = data$FREQ_SOLD_OTHERS_GREATLAKES[i]
    } else {
      data$FREQ_SOLD_INDIV_GREATLAKES[i] = data$KGS_SOLD_HH_GREATLAKES[i] / 80
    }
    }
    
    if (data$FREQ_SOLD_INDIV_KYAKALANYI[i] >= 50 & is.na(data$FREQ_SOLD_INDIV_KYAKALANYI[i]) == FALSE){
      if (data$FREQ_SOLD_OTHERS_KYAKALANYI[i]>0 & data$FREQ_SOLD_OTHERS_KYAKALANYI[i]<50 & is.na(data$FREQ_SOLD_INDIV_KYAKALANYI[i]) == FALSE){
        data$FREQ_SOLD_INDIV_KYAKALANYI[i] = data$FREQ_SOLD_OTHERS_KYAKALANYI[i]
    } else {
      data$FREQ_SOLD_INDIV_KYAKALANYI[i] = data$KGS_SOLD_HH_KYAKALANYI[i] / 80
    }
    }
    
    if (data$FREQ_SOLD_INDIV_INTERSECTION[i] >= 50 & is.na(data$FREQ_SOLD_INDIV_INTERSECTION[i]) == FALSE){
      if (data$FREQ_SOLD_OTHERS_INTERSECTION[i]>0 & data$FREQ_SOLD_OTHERS_INTERSECTION[i]<50 & is.na(data$FREQ_SOLD_INDIV_INTERSECTION[i]) == FALSE){
        data$FREQ_SOLD_INDIV_INTERSECTION[i] = data$FREQ_SOLD_OTHERS_INTERSECTION[i]
    } else {
      data$FREQ_SOLD_INDIV_INTERSECTION[i] = data$KGS_SOLD_HH_INTERSECTION[i] / 80
    }
    }
    
    if (data$FREQ_SOLD_INDIV_FARMERGROUP[i] >= 50 & is.na(data$FREQ_SOLD_INDIV_FARMERGROUP[i]) == FALSE){
      if (data$FREQ_SOLD_OTHERS_FARMERGROUP[i]>0 & data$FREQ_SOLD_OTHERS_FARMERGROUP[i]<50 & is.na(data$FREQ_SOLD_INDIV_FARMERGROUP[i]) == FALSE){
        data$FREQ_SOLD_INDIV_FARMERGROUP[i] = data$FREQ_SOLD_OTHERS_FARMERGROUP[i]
    } else {
      data$FREQ_SOLD_INDIV_FARMERGROUP[i] = data$KGS_SOLD_HH_FARMERGROUP[i] / 80
    }
    }
    
    if (data$FREQ_SOLD_INDIV_TRADER[i] >= 50 & is.na(data$FREQ_SOLD_INDIV_TRADER[i]) == FALSE){
      if (data$FREQ_SOLD_OTHERS_TRADER[i]>0 & data$FREQ_SOLD_OTHERS_TRADER[i]<50 & is.na(data$FREQ_SOLD_INDIV_TRADER[i]) == FALSE) {
        data$FREQ_SOLD_INDIV_TRADER[i] = data$FREQ_SOLD_OTHERS_TRADER[i]
    } else {
      data$FREQ_SOLD_INDIV_TRADER[i] = data$KGS_SOLD_HH_TRADER[i] / 80
    }
    }
    
    if (data$FREQ_SOLD_INDIV_OLAM[i] >= 50 & is.na(data$FREQ_SOLD_INDIV_OLAM[i]) == FALSE){
      if (data$FREQ_SOLD_OTHERS_OLAM[i]>0 & data$FREQ_SOLD_OTHERS_OLAM[i]<50 & is.na(data$FREQ_SOLD_INDIV_OLAM[i]) == FALSE){
        data$FREQ_SOLD_INDIV_OLAM[i] = data$FREQ_SOLD_OTHERS_OLAM[i]
    } else {
      data$FREQ_SOLD_INDIV_OLAM[i] = data$KGS_SOLD_HH_OLAM[i] / 80
    }
    }
    
    if (data$FREQ_SOLD_INDIV_OTHER[i] >= 50 & is.na(data$FREQ_SOLD_INDIV_OTHER[i]) == FALSE){
      if (data$FREQ_SOLD_OTHERS_OTHER[i]>0 & data$FREQ_SOLD_OTHERS_OTHER[i]<50 & is.na(data$FREQ_SOLD_INDIV_OTHER[i]) == FALSE){
        data$FREQ_SOLD_INDIV_OTHER[i] = data$FREQ_SOLD_OTHERS_OTHER[i]
    } else {
      data$FREQ_SOLD_INDIV_OTHER[i] = data$KGS_SOLD_HH_OTHER[i] / 80
    }
    }
  }
  
  # NOTE - DO NOT NEED TO REPEAT FOR OTHERS AS ALL BELOW 50.  
  
  #Aggregate up frequency for checks. 
  data$FREQ_SOLD_TOTAL_KAWAKOM = rowSums(cbind(data$FREQ_SOLD_INDIV_KAWAKOM, data$FREQ_SOLD_OTHERS_KAWAKOM), na.rm = TRUE)
  data$FREQ_SOLD_TOTAL_GREATLAKES = rowSums(cbind(data$FREQ_SOLD_INDIV_GREATLAKES, data$FREQ_SOLD_OTHERS_GREATLAKES), na.rm = TRUE)
  data$FREQ_SOLD_TOTAL_KYAKALANYI = rowSums(cbind(data$FREQ_SOLD_INDIV_KYAKALANYI, data$FREQ_SOLD_OTHERS_KYAKALANYI), na.rm = TRUE)
  data$FREQ_SOLD_TOTAL_INTERSECTION = rowSums(cbind(data$FREQ_SOLD_INDIV_INTERSECTION, data$FREQ_SOLD_OTHERS_INTERSECTION), na.rm = TRUE)
  data$FREQ_SOLD_TOTAL_FARMERGROUP = rowSums(cbind(data$FREQ_SOLD_INDIV_FARMERGROUP, data$FREQ_SOLD_OTHERS_FARMERGROUP), na.rm = TRUE)
  data$FREQ_SOLD_TOTAL_TRADER = rowSums(cbind(data$FREQ_SOLD_INDIV_TRADER, data$FREQ_SOLD_OTHERS_TRADER), na.rm = TRUE)
  data$FREQ_SOLD_TOTAL_OLAM = rowSums(cbind(data$FREQ_SOLD_INDIV_OLAM, data$FREQ_SOLD_OTHERS_OLAM), na.rm = TRUE)
  data$FREQ_SOLD_TOTAL_OTHER = rowSums(cbind(data$FREQ_SOLD_INDIV_OTHER, data$FREQ_SOLD_OTHERS_OTHER), na.rm = TRUE)
  data$TOTAL_TIMES_SOLD = rowSums(cbind(data$FREQ_SOLD_TOTAL_KAWAKOM, 
                                  data$FREQ_SOLD_TOTAL_GREATLAKES,
                                  data$FREQ_SOLD_TOTAL_KYAKALANYI,
                                  data$FREQ_SOLD_TOTAL_INTERSECTION,
                                  data$FREQ_SOLD_TOTAL_FARMERGROUP,
                                  data$FREQ_SOLD_TOTAL_TRADER,
                                  data$FREQ_SOLD_TOTAL_OLAM,
                                  data$FREQ_SOLD_TOTAL_OTHER))
  
  # First run some tests on yields per tree
  data$COFFEE_YIELD_TOTAL = as.numeric(data$COFFEE_YIELD_TOTAL )
  data$COFFEE_TREES_N = as.numeric(data$COFFEE_TREES_N)
  data$COFFEE_YIELD_PER_TREE =   data$COFFEE_YIELD_TOTAL  / data$COFFEE_TREES_N

  #More than 25000 trees appear to be extra zero typos. 
  data$COFFEE_TREES_N[data$COFFEE_TREES_N > 22000] =  data$COFFEE_TREES_N[data$COFFEE_TREES_N>22000]/10
  
  #Clear typo in HHID 1180 for yields.
  data$COFFEE_YIELD_TOTAL[data$HHID == 1180 & data$GENDER == 'Female'] = data$COFFEE_YIELD_TOTAL[data$HHID == 1180 & data$GENDER == 'Female']/10
  
  # Both respondents for 7622 report a very large amount relative to their land and trees. 
  # Likely they have reported their income from coffee instead (which brings yield per tree to average)
  data$COFFEE_YIELD_TOTAL[data$HHID == 7622] = data$COFFEE_YIELD_TOTAL[data$HHID == 7622]/2500 #price they report per kg
  
  #Reporting zero yields for hhid 5820 - likely due to only parchment sold. 300kgs = 300*5 = 1500 kgs cherries harvested
  data$COFFEE_YIELD_TOTAL[data$HHID == 5820] = 1500
  
  # Other yields per tree appear okay - because rather than typo it looks to be a unequal understanding of coffee trees
  # Which is interesting information we want to preserve. 
  
  #### Now relative to land areas. 
  ##### First derive land area estimate (owned, rented, total)
  data$LAND_OWNED_ESTIMATED = rep(0, nrow(data))
  data$LAND_RENTED_ESTIMATED = rep(0, nrow(data))
  
  #All in m2
  data$LAND_OWNED_ESTIMATED[data$LAND_LANDAMOUNT == "Up to half an acre"] = 1010
  data$LAND_OWNED_ESTIMATED[data$LAND_LANDAMOUNT == "Half an acre to 1 acre"] = 3030
  data$LAND_OWNED_ESTIMATED[data$LAND_LANDAMOUNT == "1 acre - 2 acres"] = 6070
  data$LAND_OWNED_ESTIMATED[data$LAND_LANDAMOUNT == "2 - 5 acres"] = 14164
  
  data$LAND_RENTED_ESTIMATED[data$LAND_RENTED == "Up to half an acre rented"] = 1010
  data$LAND_RENTED_ESTIMATED[data$LAND_RENTED == "Half an acre to 1 acre rented"] = 3030
  data$LAND_RENTED_ESTIMATED[data$LAND_RENTED == "1 - 2 acres rented"] = 6070
  data$LAND_RENTED_ESTIMATED[data$LAND_RENTED == "2-5 acres rented"] = 14164
  
  #More than 5 acres we must estimate on coffee trees, assuming trees are on owned land
  #and rented land typically used for annuals, livestock
  for (i in 1:nrow(data)){
    if (data$LAND_LANDAMOUNT[i] == 'More than 5 acres'){
      if (data$LAND_RENTED[i] != 'More than 5 acres'){
        data$LAND_OWNED_ESTIMATED[i] = (data$COFFEE_TREES_N[i])*5
      } else {
        data$LAND_OWNED_ESTIMATED[i] = (data$COFFEE_TREES_N[i])*5
        data$LAND_RENTED_ESTIMATED[i] = data$LAND_OWNED_ESTIMATED[i]
      }
    }
  }
  
  data$LAND_TOTAL_ESTIMATED = rowSums(cbind(data$LAND_OWNED_ESTIMATED, data$LAND_RENTED_ESTIMATED))
  plot(data$LAND_TOTAL_ESTIMATED ~ data$COFFEE_TREES_N)
  
  # typos in the number of trees for 7261 and 1881 - clear from spouse and yields
  data$COFFEE_TREES_N[data$HHID == 7261 & data$GENDER == 'Female'] = data$COFFEE_TREES_N[data$HHID == 7261 & data$GENDER == 'Female']/10
  data$COFFEE_TREES_N[data$HHID == 1881 & data$GENDER == 'Female'] = data$COFFEE_TREES_N[data$HHID == 1881 & data$GENDER == 'Female']/10
  
  # Run check to see if there if yields, trees, are 2 times or more than spouse
  HHID_trees = HHID_yields = numeric(0)
  for (i in 1:nrow(data)){
    hhidx = data$HHID[i]
    data_subset = subset(data, data$HHID == hhidx)
    if (nrow(data_subset)>1){
      yield_1 = data_subset$COFFEE_YIELD_TOTAL[1]
      yield_2 = data_subset$COFFEE_YIELD_TOTAL[2]
      tree_1 = data_subset$COFFEE_TREES_N[1]
      tree_2 = data_subset$COFFEE_TREES_N[2]
      
      if (abs(yield_1 - yield_2) / ((yield_1 + yield_2) / 2) > 1.5){
        HHID_yields = c(HHID_yields, hhidx)
      }
      
      if (abs(tree_1 - tree_2) / ((tree_1 + tree_2) / 2) > 1.5){
        HHID_trees = c(HHID_yields, hhidx)
      }
      
    }
  }
  
  # Yield issues with 8767 (both spouses with just parchment)
  data$COFFEE_YIELD_TOTAL[data$HHID == 8767] = data$COFFEE_PARCH_KGS[data$HHID == 8767 & data$GENDER == 'Male'] * 5
  # Yield issues with 8767 (both spouses aligned when yields per buyer so use spouses info)
  data$COFFEE_YIELD_TOTAL[data$HHID == 9925] = 850
  # Yield issues with 4211 derives from likely typo in yields for female (missing zero)
  data$COFFEE_YIELD_TOTAL[data$HHID == 4211 & data$GENDER == 'female'] = data$COFFEE_YIELD_TOTAL[data$HHID == 4211 & data$GENDER == 'female']*10
  # Yield issues with 1821 due to parchment confusion
  data$COFFEE_YIELD_TOTAL[data$HHID == 1821] = data$COFFEE_PARCH_KGS[data$HHID == 1821 & data$GENDER == 'Male'] * 5 + 100 #Also has 100 sold in cherries
  #Yield of 0.89 - adjust to use total yields reported for each buyer for this observation, including parchment. 
  data$COFFEE_YIELD_TOTAL[data$HHID == 9747] = 700 + 150*5
  #Yield for 7196 - missing two zeros best explanation. Aligns with trees, spouse yields, and somewhat with parchment
  data$COFFEE_YIELD_TOTAL[data$HHID == 7196] = 2000 #Matches spouse with 2000 reported already
  #Yield for 4021 - align with parchment to avoid zero entry - same for both spouse
  data$COFFEE_YIELD_TOTAL[data$HHID == 4021] =  data$COFFEE_PARCH_KGS[data$HHID == 4021] * 5
  
  #As we will make some adjustments to the individual marketing streams and drop some parchment obs. 
  # We create a new parchment proportion variable which retains this information for other analysis. 
  # Better as an indicator of relative volumes of parchment rather than an accurate measure.
  data$PARCHMENT_PROPORTION = data$COFFEE_PARCH_KGS*5 / data$COFFEE_YIELD_TOTAL

  #If report times sold, but no kgs, fix this. 
  #Edits for most derive from parchment misallocations which we will fix (convert to cherries for this one as focus of this study is not concerned about parchment/cherry split)
  data$KGS_SOLD_HH_KAWAKOM[data$HHID == 1821] = data$COFFEE_PARCH_KGS[data$HHID == 1821] * 5
  data$COFFEE_YIELD_TOTAL[data$HHID == 1821] = data$COFFEE_PARCH_KGS[data$HHID == 1821] * 5 + 100 # for yields with trader
  data$COFFEE_PARCH_KGS[data$HHID == 1821] = 0
  data$COFFEE_PARCH_PRICE[data$HHID == 1821] = 0
  
  data$COFFEE_PARCH_KGS[data$HHID == 2307] = 0
  data$COFFEE_PARCH_PRICE[data$HHID == 2307] = 0
  
  data$KGS_SOLD_HH_KAWAKOM[data$HHID == 8767] = data$COFFEE_PARCH_KGS[data$HHID == 8767] * 5
  data$PRICE_HH_KAWAKOM[data$HHID == 8767] = median(data$PRICE_HH_KAWAKOM, na.rm = TRUE)
  data$COFFEE_YIELD_TOTAL[data$HHID == 8767] = data$COFFEE_PARCH_KGS[data$HHID == 8767] * 5
  data$COFFEE_PARCH_KGS[data$HHID == 8767] = 0
  data$COFFEE_PARCH_PRICE[data$HHID == 8767] = 0
  
  data$KGS_SOLD_HH_KAWAKOM[data$HHID == 4021] = data$COFFEE_PARCH_KGS[data$HHID == 4021] * 5
  data$PRICE_HH_KAWAKOM[data$HHID == 4021] = median(data$PRICE_HH_KAWAKOM, na.rm = TRUE)
  data$COFFEE_YIELD_TOTAL[data$HHID == 4021] = data$COFFEE_PARCH_KGS[data$HHID == 4021] * 5
  data$COFFEE_PARCH_KGS[data$HHID == 4021] = 0
  data$COFFEE_PARCH_PRICE[data$HHID == 4021] = 0
  
  data$KGS_SOLD_HH_KAWAKOM[data$HHID == 5820] = (data$COFFEE_PARCH_KGS[data$HHID == 5820] * 5)/2
  data$KGS_SOLD_HH_OTHER[data$HHID == 5820] = (data$COFFEE_PARCH_KGS[data$HHID == 5820] * 5)/2
  data$PRICE_HH_OTHER[data$HHID == 5820] = 2000 # adjust confusion between cherry and parchment price to match spouse. 
  data$COFFEE_YIELD_TOTAL[data$HHID == 5820] = data$PRICE_HH_KAWAKOM[data$HHID == 5820] + 50 # to add in trader volumes. 
  data$COFFEE_PARCH_KGS[data$HHID == 5820] = 0
  data$COFFEE_PARCH_PRICE[data$HHID == 5820] = 0
  
  data$KGS_SOLD_HH_KAWAKOM[data$HHID == 6321] = data$COFFEE_PARCH_KGS[data$HHID == 6321] * 5
  data$COFFEE_YIELD_TOTAL[data$HHID == 6321] = data$COFFEE_PARCH_KGS[data$HHID == 6321] * 5 + 300  #to add in trader volumes. 
  data$COFFEE_PARCH_KGS[data$HHID == 6321] = 0
  data$COFFEE_PARCH_PRICE[data$HHID == 6321] = 0
  
  data$KGS_SOLD_HH_KAWAKOM[data$HHID == 3962] = data$COFFEE_PARCH_KGS[data$HHID == 3962] * 5
  data$PRICE_HH_KAWAKOM[data$HHID == 3962] = median(data$PRICE_HH_KAWAKOM, na.rm = TRUE)
  data$COFFEE_YIELD_TOTAL[data$HHID == 3962] = data$COFFEE_PARCH_KGS[data$HHID == 3962] * 5
  data$COFFEE_PARCH_KGS[data$HHID == 3962] = 0
  data$COFFEE_PARCH_PRICE[data$HHID == 3962] = 0
  
  data$KGS_SOLD_HH_KAWAKOM[data$HHID == 9179] = data$COFFEE_PARCH_KGS[data$HHID == 9179] * 5
  data$COFFEE_YIELD_TOTAL[data$HHID == 9179] = data$COFFEE_PARCH_KGS[data$HHID == 9179] * 5 + 300 # to add in trader volumes. 
  data$COFFEE_PARCH_KGS[data$HHID == 9179] = 0
  data$COFFEE_PARCH_PRICE[data$HHID == 9179] = 0
  
  data$KGS_SOLD_HH_KAWAKOM[data$HHID == 7196] = data$COFFEE_PARCH_KGS[data$HHID == 7196] * 5
  data$PRICE_HH_KAWAKOM[data$HHID == 7196] = median(data$PRICE_HH_KAWAKOM, na.rm = TRUE)
  data$COFFEE_YIELD_TOTAL[data$HHID == 7196] = data$COFFEE_PARCH_KGS[data$HHID == 7196] * 5 + 300 # to add in trader volumes. 
  data$COFFEE_PARCH_KGS[data$HHID == 7196] = 0
  data$COFFEE_PARCH_PRICE[data$HHID == 7196] = 0
  
  data$KGS_SOLD_HH_KAWAKOM[data$HHID == 5584 & data$GENDER == "Male"] = data$COFFEE_PARCH_KGS[data$HHID == 5584 & data$GENDER == "Male"] * 5
  data$KGS_SOLD_HH_KAWAKOM[data$HHID == 5584 & data$GENDER == "Female"] = data$COFFEE_PARCH_KGS[data$HHID == 5584 & data$GENDER == "Female"] * 5 + data$KGS_SOLD_HH_KAWAKOM[data$HHID == 5584 & data$GENDER == "Female"]
  data$COFFEE_YIELD_TOTAL[data$HHID == 5584] = data$COFFEE_PARCH_KGS[data$HHID == 5584] * 5 
  data$COFFEE_PARCH_KGS[data$HHID == 5584] = 0
  data$COFFEE_PARCH_PRICE[data$HHID == 5584] = 0
  
  data$KGS_SOLD_HH_GREATLAKES[data$HHID == 2670 & data$GENDER == "Male"] = data$COFFEE_YIELD_TOTAL[data$HHID == 2670 & data$GENDER == "Male"] - data$KGS_SOLD_HH_TRADER[data$HHID == 2670 & data$GENDER == "Male"]

  data$KGS_SOLD_HH_KYAKALANYI[data$HHID == 1131] = data$COFFEE_PARCH_KGS[data$HHID == 5584] * 5
  data$COFFEE_YIELD_TOTAL[data$HHID == 1131] = rowSums(cbind(data$COFFEE_PARCH_KGS[data$HHID == 1131]*5, data$KGS_SOLD_HH_TRADER[data$HHID == 1131]), na.rm = TRUE)
  data$COFFEE_PARCH_KGS[data$HHID == 1131] = 0
  data$COFFEE_PARCH_PRICE[data$HHID == 1131] = 0
  
  #Now aggregate reported marketed yields per buyer and parchment.
  data$total_marketed_coffee_hhead = rep(0,nrow(data)) #may want household head, spouse, or average 
  data$total_marketed_coffee_spouse = rep(0,nrow(data))  
  data$total_marketed_coffee_average = rep(0,nrow(data))
  
  for (i in 1:nrow(data)){
    data_subset = data[which(data$HHID == data$HHID[i]),]
    hhead_idx = which(data_subset$RESPONDENT_ID == "Household head")
    spouse_idx = which(data_subset$RESPONDENT_ID != "Household head")
    
    #Total also includes parchment, converted to cherries with ratio 1/5
    data$total_marketed_coffee_hhead[i] = sum(as.numeric(data_subset$KGS_SOLD_HH_KAWAKOM[hhead_idx]),
                                              as.numeric(data_subset$KGS_SOLD_HH_GREATLAKES[hhead_idx]),
                                              as.numeric( data_subset$KGS_SOLD_HH_KYAKALANYI[hhead_idx]),
                                              as.numeric(data_subset$KGS_SOLD_HH_INTERSECTION[hhead_idx]),
                                              as.numeric(data_subset$KGS_SOLD_HH_OLAM[hhead_idx]),
                                              as.numeric(data_subset$KGS_SOLD_HH_FARMERGROUP[hhead_idx]),
                                              as.numeric(data_subset$KGS_SOLD_HH_TRADER[hhead_idx]),
                                              as.numeric(data_subset$KGS_SOLD_HH_OTHER[hhead_idx]),
                                              as.numeric(data_subset$COFFEE_PARCH_KGS[hhead_idx])*5,
                                              na.rm = TRUE)
    
    data$total_marketed_coffee_spouse[i] = sum(as.numeric(data_subset$KGS_SOLD_HH_KAWAKOM[spouse_idx]),
                                               as.numeric(data_subset$KGS_SOLD_HH_GREATLAKES[spouse_idx]),
                                               as.numeric( data_subset$KGS_SOLD_HH_KYAKALANYI[spouse_idx]),
                                               as.numeric(data_subset$KGS_SOLD_HH_INTERSECTION[spouse_idx]),
                                               as.numeric(data_subset$KGS_SOLD_HH_OLAM[spouse_idx]),
                                               as.numeric(data_subset$KGS_SOLD_HH_FARMERGROUP[spouse_idx]),
                                               as.numeric(data_subset$KGS_SOLD_HH_TRADER[spouse_idx]),
                                               as.numeric(data_subset$KGS_SOLD_HH_OTHER[spouse_idx]),
                                               as.numeric(data_subset$COFFEE_PARCH_KGS[spouse_idx])*5,  
                                               na.rm = TRUE)
    
    data$total_marketed_coffee_average[i] = mean(data$total_marketed_coffee_hhead[i], 
                                                 data$total_marketed_coffee_spouse[i])
  }
  
  #Final edits to price to avoid mean issues later
  data$PRICE_HH_KYAKALANYI[data$PRICE_HH_KYAKALANYI == 0 & !is.na(data$PRICE_HH_KYAKALANYI)] = median(data$PRICE_HH_KYAKALANYI, na.rm = TRUE)
  data$PRICE_HH_OLAM[data$PRICE_HH_OLAM == 0 & !is.na(data$PRICE_HH_OLAM)] = median(data$PRICE_HH_OLAM, na.rm = TRUE)
  
  return(data)

}


                                                                 
