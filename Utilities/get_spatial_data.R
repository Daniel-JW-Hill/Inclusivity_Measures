
# This script retrieves the altitude and any other relevant spatial information
# Based on the village GPS coordinates. 

get_spatial_data = function(data){
  
  #Load in GPS coordinates of points of interest
  library(sf)
  GPS_vils  = st_read(here("data", "Village gps coordinates.kml" ))
  GPS_vils$Name = tolower(GPS_vils$Name)
  
  # First we obtain altitude information based on village GPS information. 
  library(geonames)
  options(geonamesUsername="danielhill")
  data$altitude= rep(0, nrow(data))
  for (i in 1:nrow(data)){
    data$altitude[i] = unlist(GNsrtm3(data$vil_location_lat[i],data$vil_location_lng[i])[1])
  }
 
  ##### Distance to paved road using road network. #####
  # These were mapped manually for each village on google earth, 
  # and information saved as kml file. 

  GPS_road_lines = GPS_vils[grepl("path_road", GPS_vils$Name), ]
  GPS_road_lines$Name = sub("_path_road$", "", GPS_road_lines$Name)
  
  GPS_road_lines$Name[GPS_road_lines$Name == "tulowo"] = "tulolwo"
  GPS_road_lines$Name[GPS_road_lines$Name == "kapweiminy"] = "kapkweiminy"
  GPS_road_lines$Name[GPS_road_lines$Name == "kamorok"] = "kamarok"
  
  data$distance_road = rep(0, nrow(data))
  for (i in 1:nrow(data)){
    village_idx = data$VILLAGE[i]
    idx = which(GPS_road_lines$Name == village_idx)
    data$distance_road[i] = unlist(st_length(GPS_road_lines$geometry[idx]))
  }
  
  
  ##### Distance to Kapchorwa.#####
  ##### Distance to Kawakom. #####
  
  #Note - if we are interested in other distance measures such as these, we
  # can map manually or use nodal points on tarmac road. 
  
  return(data)
  
  }