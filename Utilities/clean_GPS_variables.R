
#This function repairs GPS coordinate information for hosueholds
#Given inconsistency in GPS accuracy based on enumerator phones, browsers, and
#Permissions

clean_GPS_variables = function(data){

  
  #Numeric vectors for GPS location
  #Note we do not use the Qualtrics GPS info as it is incorrect. 
  data$geo_lat = as.numeric(data$geo_lat)
  data$geo_lng = as.numeric(data$geo_lng)
  
  #Inspect non empty GPS locations
  # library(mapview)
  # data_nonempty = data[!is.na(data$geo_lat),]
  # mapview(data_nonempty , xcol = 'geo_lng', ycol = 'geo_lat', crs = 4269, grid = FALSE)
  #About 50% look correct, some far away and some clustering in Kapchorwa. 
  
  #Loop through each village and manually adjust. 
  #First amend case sensitive issues with villages
  data$VILLAGE = tolower(data$VILLAGE)
  
  #Now different spellings of the same villages. 
  data$VILLAGE[which(data$VILLAGE == "aniokumui")] = "oniokumu"
  data$VILLAGE[which(data$VILLAGE == "annio kumui")] = "oniokumu"
  data$VILLAGE[which(data$VILLAGE == "anniokumui")] = "oniokumu"
  data$VILLAGE[which(data$VILLAGE == "anyokumui")] = "oniokumu"
  data$VILLAGE[which(data$VILLAGE == "onio- kumui")] = "oniokumu"
  data$VILLAGE[which(data$VILLAGE == "onio kumui")] = "oniokumu"
  data$VILLAGE[which(data$VILLAGE == "onio-kumui")] = "oniokumu"
  
  data$VILLAGE[which(data$VILLAGE == "chemosong.")] = "chemosong"
  
  data$VILLAGE[which(data$VILLAGE == "chemron")] = "chemuron"
  data$VILLAGE[which(data$VILLAGE == "chemwron")] = "chemuron"
  
  data$VILLAGE[which(data$VILLAGE == "chepkwutwo")] = "chepkutwo"
  
  data$VILLAGE[which(data$VILLAGE == "cheptiyal")] = "cheptilyal"
  data$VILLAGE[which(data$VILLAGE == "cheptyal")] = "cheptilyal"
  
  data$VILLAGE[which(data$VILLAGE == "gamatui village")] = "gamatui"
  
  data$VILLAGE[which(data$VILLAGE == "kapchesomba")] = "kapchesombe"
  
  data$VILLAGE[which(data$VILLAGE == "kapkeen")] = "kapkwen"
  data$VILLAGE[which(data$VILLAGE == "kapween")] = "kapkwen"
  data$VILLAGE[which(data$VILLAGE == "kapkween")] = "kapkwen"
  
  data$VILLAGE[which(data$VILLAGE == "kukumai village")] = "kukumai"
  data$VILLAGE[which(data$VILLAGE == "kukumai,")] = "kukumai"
  
  data$VILLAGE[which(data$VILLAGE == "towoi")] = "towei"
  data$VILLAGE[which(data$VILLAGE == "towei village")] = "towei"
  
  Tapchor = c("9758","1984","7203","9402","8375","4230","2382","1607","7647","3873","3327","4866","1246","2626")
  for (t in 1:length(Tapchor)){
    HHIDX = Tapchor[t]
    row_idx = which(data$HHID == HHIDX)
    if (length(row_idx >0 )){
      for (r in 1:length(row_idx)){
        if (data$VILLAGE[row_idx[r]] != 'tapchor'){
          data$VILLAGE[row_idx[r]] = 'kokwomawe'
        }
      }
    }
  }
  
  data$VILLAGE[which(data$VILLAGE == "tigirim cell")] = "tigrim"
  data$VILLAGE[which(data$VILLAGE == "tigirim")] = "tigrim"
  data$VILLAGE[which(data$VILLAGE == "tukurum")] = "tigrim"

  data$VILLAGE[which(data$VILLAGE == "sirmityo")] = "sirimityo"
  
  data$VILLAGE[which(data$VILLAGE == "kurumbwo")] = "kurumbowo"
  
  data$VILLAGE[which(data$VILLAGE == "kumi")] = "kukumai"
  data$VILLAGE[which(data$VILLAGE == "kumui")] = "kukumai"
  data$VILLAGE[which(data$VILLAGE == "kumoi")] = "kukumai"
  data$VILLAGE[which(data$VILLAGE == "kumoi")] = "kukumai"
  
  data$VILLAGE[which(data$VILLAGE == "tuwola")] = "tulolwo"
  
  data$VILLAGE[which(data$VILLAGE == "kamstarit")] = "kamasitarit"
  data$VILLAGE[which(data$VILLAGE == "kamasistarit")] = "kamasitarit"
  
  data$VILLAGE[which(data$VILLAGE == "chesyo")] = "chesiyo"
  
  data$VILLAGE[which(data$VILLAGE == "dangwen")] = "tangwen"
  
  data$VILLAGE[which(data$VILLAGE == "kapmorok")] = "kamarok"
  data$VILLAGE[which(data$VILLAGE == "kamorok")] = "kamarok"
  data$VILLAGE[which(data$VILLAGE == "chesakwa")] =  "kamarok"
  
  data$VILLAGE[which(data$VILLAGE == "kameido")] = "kamaido"
  data$VILLAGE[which(data$VILLAGE == "kumiedo")] = "kamaido"
  
  data$VILLAGE[which(data$VILLAGE == "kapkweminy")] = "kapkweiminy"
  data$VILLAGE[which(data$VILLAGE == "kakwiminy")] = "kapkweiminy"
  
  data$VILLAGE[which(data$VILLAGE == "wiri")] = "wirr"
  
  data$VILLAGE[which(data$VILLAGE == "smarton")] = "sumaton"
  data$VILLAGE[which(data$VILLAGE == "smaton")] = "sumaton"
  
  data$VILLAGE[which(data$VILLAGE == "kapsajem")] = "kapsojom"
  data$VILLAGE[which(data$VILLAGE == "kapsogon")] = "kapsojom"
  data$VILLAGE[which(data$VILLAGE == "kapsojon")] = "kapsojom"
  
  data$VILLAGE[which(data$VILLAGE == "kaplelko ward")] = "kaplelko"
  
  data$VILLAGE[which(data$VILLAGE == "chesagat")] = "chesakat"
  
  data$VILLAGE[which(data$VILLAGE == "kokomosowe")] = "kakwomasewe"
  
  data$VILLAGE[which(data$VILLAGE == "kapngaramu")] = "kapmwaryomwo"
  data$VILLAGE[which(data$VILLAGE == "kapngarwm")] = "kapmwaryomwo"
  
  data$VILLAGE[which(data$VILLAGE == "bwat")] = "nkwat"
  data$VILLAGE[which(data$VILLAGE == "mbwat")] = "nkwat"
  
  data$VILLAGE[which(data$VILLAGE == "kortokwet")] = "kartokwet"
  
  data$VILLAGE[which(data$VILLAGE == "kapchebinyeny")] =  "kapchebunyai"
  
  data$VILLAGE[which(data$VILLAGE == "chepkutwo")] =  "kapkwai"
 
  data$VILLAGE[which(data$VILLAGE == "Kamasistarit")] =  "Kamasitarit"
  
  data$VILLAGE[which(data$VILLAGE == "askenuo")] =  "asikenwo"
  
  data$VILLAGE[which(data$VILLAGE == "kowowo")] =  "kawowo"
  data$VILLAGE[which(data$VILLAGE == "kwowo")] =  "kawowo"
  
  data$VILLAGE[which(data$VILLAGE == "chesongok")] =  "kukumai"
  data$VILLAGE[which(data$VILLAGE == "kobur")] =  "kukumai"
  data$VILLAGE[which(data$VILLAGE == "kukukai")] =  "kukumai"
  
  data$VILLAGE[which(data$VILLAGE == "kapchepbut")] =  "kapchebut"
  
  data$VILLAGE[which(data$VILLAGE == "42")] =  "kabukose"
  
  # Small distances for neighbouring villages. Pick one as minor differences. 
  data$VILLAGE[which(data$HHID == '1872')] = 'chepterit'
  data$VILLAGE[which(data$HHID == '9984')] = 'kaplelko'
  data$VILLAGE[which(data$HHID == '8281')] = 'sukut'
  data$VILLAGE[which(data$HHID == '3768')] = 'sukut'
  data$VILLAGE[which(data$HHID == '4230')] = 'kakwomasewe'
  data$VILLAGE[which(data$HHID == '9758')] = 'kakwomasewe'
  
  # Some are errors. Trust recruitment docs. 
  data$VILLAGE[which(data$HHID == '9747')] = 'kawowo'
  data$VILLAGE[which(data$HHID == '3475')] = 'oniokumu'
  data$VILLAGE[which(data$HHID == '8102')] = 'oniokumu'
  
  # Now read in spatial .kml file that maps villages to gps coords 
  # This was constructed manually based on administrative data available
  # on openstreetmaps.org. Then mapped to Google Earth KML file. 
  library(sf)
  GPS_vils  = st_read(here("data", "Village gps coordinates.kml" ))
  GPS_vils$Name = tolower(GPS_vils$Name)
  
  #Now create new GPS location variable to match village observations.
  vil_location_lat = vil_location_lng =  rep(NA, nrow(data))
  for (i in 1:nrow(data)){
    village_idx = data$VILLAGE[i]
    vil_loc = unlist(GPS_vils$geometry[which(GPS_vils$Name == village_idx)])
    if (length(vil_loc)!=0){
      vil_location_lat[i] = vil_loc[2]
      vil_location_lng[i] = vil_loc[1]
    }
  }
  
  #Now for those with empty locations, match with their spouse.  
  for (i in 1:nrow(data)){
    HH_idx = data$HHID[i]
    if (is.na(vil_location_lat[i])){
      vil_loc_lat = vil_location_lat[which(data$HHID == HH_idx)]
      vil_loc_lng = vil_location_lng[which(data$HHID == HH_idx)]
      if (all(is.na(vil_loc_lat)) == FALSE){
        vil_location_lat[i] = vil_loc_lat[which(is.na(vil_loc_lat) == FALSE)]
        vil_location_lng[i] = vil_loc_lng[which(is.na(vil_loc_lng) == FALSE)]
        }
    }
  }
  
  data$vil_location_lat = vil_location_lat
  data$vil_location_lng = vil_location_lng
  
  # #For remaining NA values, loop through and check where we were each of these days. 
  # NA_idx = which(is.na(vil_location_lat))
  # village = village_others = list()
  # for (i in 1:length(NA_idx)){
  #   idx = NA_idx[i]
  #   date_complete = data$date_completed_rounded[idx]
  #   matched_data = subset(data, data$date_completed_rounded == date_complete)
  #   village[[i]] = data$VILLAGE[idx]
  #   village_others[[i]] = unique(matched_data$VILLAGE)
  # } 
  
  # # Finally, perform checks to ensure spouses have the same or near 
  # # GPS location. 
  # library(geosphere)
  # unique_hhs = unique(data$HHID)
  # distances_spouses = rep(0, length(unique_hhs))
  # counter = 0
  # for (u in unique_hhs){
  #   counter = counter+1
  #   idx = which(data$HHID == u)
  #   if (length(idx) > 1){
  #    lon1 = data$vil_location_lng[idx[1]]
  #    lon2 = data$vil_location_lng[idx[2]]
  #    lat1 = data$vil_location_lat[idx[1]]
  #    lat2 = data$vil_location_lat[idx[2]]
  #    distances_spouses[counter] = distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
  #   }
  # }
  # 
  # distances_spouses = cbind(unique_hhs, distances_spouses)
  # distances_spouses = subset(distances_spouses, distances_spouses[,2] >0)
  # 
  # vil_1 = rep(0, nrow(distances_spouses))
  # vil_2 = rep(0, nrow(distances_spouses))
  # for (d in 1:nrow(distances_spouses)){
  #   villages = data$VILLAGE[which(data$HHID == distances_spouses[d,1])]
  #   vil_1[d] = villages[1]
  #   vil_2[d] = villages[2]
  # }
  # distances_spouses = cbind(distances_spouses, vil_1, vil_2)
  
  return(data)
  
  
}





