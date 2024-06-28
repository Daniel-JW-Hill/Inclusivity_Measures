
#Simple function to clean ranking names
clean_preference_ranks  = function(data){
  
  colnames(data)[which(names(data) == "RANKING_KAWAKOM_8")] = 'RANKING_KAWAKOM_7'
  colnames(data)[which(names(data) == "RANKING_KAWAKOM_9")] = 'RANKING_KAWAKOM_8'
    
  colnames(data)[which(names(data) == "RANKING_KYAKALANI_8")] = 'RANKING_KYAKALANI_7'
  colnames(data)[which(names(data) == "RANKING_KYAKALANI_9")] = 'RANKING_KYAKALANI_8'
    
  colnames(data)[which(names(data) == "RANKING_INTERSECTION_8")] = 'RANKING_INTERSECTION_7'
  colnames(data)[which(names(data) == "RANKING_INTERSECTION_9")] = 'RANKING_INTERSECTION_8'
    
  colnames(data)[which(names(data) == "RANKING_GREATLAKES_8")] = 'RANKING_GREATLAKES_7'
  colnames(data)[which(names(data) == "RANKING_GREATLAKES_9")] = 'RANKING_GREATLAKES_8'
    
  colnames(data)[which(names(data) == "RANKING_FARMGROUP_8")] = 'RANKING_FARMGROUP_7'
  colnames(data)[which(names(data) == "RANKING_FARMGROUP_9")] = 'RANKING_FARMGROUP_8'
    
  colnames(data)[which(names(data) == "RANKING_TRADER_8")] = 'RANKING_TRADER_7'
  colnames(data)[which(names(data) == "RANKING_TRADER_9")] = 'RANKING_TRADER_8'
    
  colnames(data)[which(names(data) == "RANKING_OLAM_8")] = 'RANKING_OLAM_7'
  colnames(data)[which(names(data) == "RANKING_OLAM_9")] = 'RANKING_OLAM_8'
    
  colnames(data)[which(names(data) == "RANKING_OTHER_8")] = 'RANKING_OTHER_7'
  colnames(data)[which(names(data) == "RANKING_OTHER_9")] = 'RANKING_OTHER_8'
    
  return(data)
}