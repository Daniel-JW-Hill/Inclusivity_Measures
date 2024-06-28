
# This function returns relative price differentials between the price they accept 
# vs the median price in the village. If they opt into participation, we use this buyer
# Else it is the average of commodity buyers. 
  
  get_relative_prices = function(data){

  data$RELATIVE_PRICE_DIFFERENTIAL = rep(0, nrow(data))
  
  other_prices = c("PRICE_HH_INTERSECTION", "PRICE_HH_FARMERGROUP", "PRICE_HH_GREATLAKES" , "PRICE_HH_OLAM", "PRICE_HH_OTHER")
  commodity_prices = c("PRICE_HH_KAWAKOM", "PRICE_HH_KYAKALANYI", "PRICE_HH_TRADER")
  all_prices = c(other_prices, commodity_prices)
  
  for (i in 1:nrow(data)){
    
    village = data$VILLAGE[i]
    village_subset = subset(data, data$VILLAGE == village)
    
    #Mean price accepted 
    if (data$binary_participation_hh[i] == 1){
      price_intersection = data$PRICE_HH_INTERSECTION[i]
      price_farmergroup =  data$PRICE_HH_FARMERGROUP[i]
      price_greatlakes = data$PRICE_HH_GREATLAKES[i]
      price_olam =  data$PRICE_HH_OLAM[i]
      price_other =  data$PRICE_HH_OTHER[i]
      other_vec = c(price_intersection, price_farmergroup, price_greatlakes, price_olam, price_other)
      median_price_accepted = median(other_vec, na.rm = TRUE)
      if (is.na(median_price_accepted)){
        hh_subset = data[data$HHID == data$HHID[i], ]
        price_intersection = mean(hh_subset$PRICE_HH_INTERSECTION, na.rm = TRUE)
        price_farmergroup =  mean(hh_subset$PRICE_HH_FARMERGROUP, na.rm = TRUE)
        price_greatlakes = mean(hh_subset$PRICE_HH_GREATLAKES, na.rm = TRUE)
        price_olam =  mean(hh_subset$PRICE_HH_OLAM, na.rm = TRUE)
        price_other =  mean(hh_subset$PRICE_HH_OTHER, na.rm = TRUE)
        other_vec = c(price_intersection, price_farmergroup, price_greatlakes, price_olam, price_other)
        median_price_accepted = median(other_vec, na.rm = TRUE)
      }
      
    } else {
      price_kawakom = data$PRICE_HH_KAWAKOM[i]
      price_kyakalanyi = data$PRICE_HH_KYAKALANYI[i]
      price_trader = data$PRICE_HH_TRADER[i]
      commodity_vec = c(price_kawakom, price_kyakalanyi, price_trader)
      median_price_accepted = median(commodity_vec, na.rm = TRUE)
      
      if (is.na(median_price_accepted)){
        hh_subset = data[data$HHID == data$HHID[i], ]
        price_kawakom = mean(hh_subset$PRICE_HH_KAWAKOM, na.rm = TRUE)
        price_kyakalanyi =  mean(hh_subset$PRICE_HH_KYAKALANYI, na.rm = TRUE)
        price_trader = mean(hh_subset$PRICE_HH_TRADER, na.rm = TRUE)
        commodity_vec = c(price_kawakom, price_kyakalanyi, price_trader)
        median_price_accepted = median(commodity_vec, na.rm = TRUE)
      }
    }
    
    # Now for median price in the village
    price_mat = as.vector(unlist(village_subset[,names(data) %in% all_prices]))
    median_price_village = median(price_mat, na.rm = TRUE)
    
    data$RELATIVE_PRICE_DIFFERENTIAL[i] = (median_price_accepted -  median_price_village)/1000
     
  }
  
  return(data)
     
  }
    