
# This script retrieves proxy asset values for respondents. 

get_asset_values = function(data){
 
  price = c(0.12,0.4,4,0.2,0.1,0.01)
  
  # price_cellphones = 0.12 #'000,000s UGX
  # price_smartphones = 0.4 #'000,000s UGX
  # price_boda = 4 #'000,000s UGX
  # price_largelivestock = 0.2 #'000,000s UGX
  # price_medium_livestock = 0.1 #'000,000s UGX
  # price_poultry  = 0.01 #'000,000s UGX

  #Edit column names to indicate what asset each is. 
  # Edit column names to indicate what asset each is in the data frame 'data'
  colnames(data)[colnames(data) == "HH_ASSETS_1_1"] <- "n_cellphones_others"
  colnames(data)[colnames(data) == "HH_ASSETS_2_1"] <- "n_smartphones_others"
  colnames(data)[colnames(data) == "HH_ASSETS_3_1"] <- "n_bodas_others"
  colnames(data)[colnames(data) == "HH_ASSETS_4_1"] <- "n_coffeeequip_others"
  colnames(data)[colnames(data) == "HH_ASSETS_5_1"] <- "n_largelivestock_others"
  colnames(data)[colnames(data) == "HH_ASSETS_6_1"] <- "n_goats_others"
  colnames(data)[colnames(data) == "HH_ASSETS_7_1"] <- "n_poultry_others"
  
  colnames(data)[colnames(data) == "INDIV_ASSETS_1_1"] <- "n_cellphones_indiv"
  colnames(data)[colnames(data) == "INDIV_ASSETS_2_1"] <- "n_smartphones_indiv"
  colnames(data)[colnames(data) == "INDIV_ASSETS_3_1"] <- "n_bodas_indiv"
  colnames(data)[colnames(data) == "INDIV_ASSETS_4_1"] <- "n_coffeeequip_indiv"
  colnames(data)[colnames(data) == "INDIV_ASSETS_5_1"] <- "n_largelivestock_indiv"
  colnames(data)[colnames(data) == "INDIV_ASSETS_6_1"] <- "n_goats_indiv"
  colnames(data)[colnames(data) == "INDIV_ASSETS_7_1"] <- "n_poultry_indiv"
  
  #Loop through and create values AND as.numeric all columns. 
  HH_assets = c("n_cellphones_others","n_smartphones_others","n_bodas_others","n_largelivestock_others","n_goats_others","n_poultry_others")
  Indiv_assets = c("n_cellphones_indiv","n_smartphones_indiv","n_bodas_indiv","n_largelivestock_indiv","n_goats_indiv","n_poultry_indiv")
  
  counter = 0
  for (col in HH_assets) {
    counter = counter + 1
    # Convert column values to numeric
    data[[col]] =  as.numeric(data[[col]])
    # Create new variable named "value" by multiplying the column by the relevant price
    data[[paste0("value_", col)]] =  data[[col]] * price[counter]
  }
  
  counter = 0
  for (col in Indiv_assets) {
    counter = counter + 1
    # Convert column values to numeric
    data[[col]] =  as.numeric(data[[col]])
    # Create new variable named "value" by multiplying the column by the relevant price
    data[[paste0("value_", col)]] =  data[[col]] * price[counter]
  }
  
  data$value_assets_indiv = rowSums(cbind(data$value_n_cellphones_indiv, 
                                data$value_n_smartphones_indiv,
                                data$value_n_bodas_indiv,
                                data$value_n_largelivestock_indiv,
                                data$value_n_goats_indiv,
                                data$value_n_poultry_indiv), na.rm = TRUE) 
    
  data$value_assets_others = rowSums(cbind(data$value_n_cellphones_others, 
                                          data$value_n_smartphones_others,
                                          data$value_n_bodas_others,
                                          data$value_n_largelivestock_others,
                                          data$value_n_goats_others,
                                          data$value_n_poultry_others), na.rm = TRUE) 
  
  data$value_assets_HH =  rowSums(cbind(data$value_assets_indiv,data$value_assets_others))
  
  return(data)
}