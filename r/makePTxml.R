###################################
#### MAKE TRANSIT SCHEDULE XML ####
###################################

name <- "equilPTschedule"
sink(paste0("txt/", name, ".xml"))


cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>","\n")
cat("<!DOCTYPE transitSchedule SYSTEM \"http://www.matsim.org/files/dtd/transitSchedule_v2.dtd\">","\n","\n")

cat("<transitSchedule>","\n")
cat("   <transitStops>","\n")
for (i in 1:nrow(stops)){
  
  cat(paste0("     <stopFacility ", "id=\"", stops$stop_id[i], 
             "\" x=\"", stops$X[i], 
             "\" y=\"", stops$Y[i], 
             "\" linkRefId=\"", stops$linkRefId[i],
             "\" name=\"", stops$name[i],
             "\" isBlocking=\"", "false\"", 
             "/>","\n"))
  
} #end of stops for loop
cat("   </transitStops>","\n","\n","\n")

for (j in 1:nrow(routes)){
  
  cat( paste0("   <transitLine id=\"", routes$route_id[j],"\">","\n") )
  cat( paste0("      <attributes>","\n") )
  cat( paste0("        <attribute ", "name=\"gtfs_agency_id\"", " class=\"java.lang.String\">", routes$agency_id[j],"</attribute>","\n"))
  cat( paste0("        <attribute ", "name=\"gtfs_route_short_name\"", " class=\"java.lang.String\">", routes$route_short_name[j],"</attribute>","\n"))
  cat( paste0("        <attribute ", "name=\"gtfs_route_type\"", " class=\"java.lang.String\">", routes$route_type[j],"</attribute>","\n"))
  cat( paste0("      </attributes>","\n","\n") ) #close attributes
  
  services2 <- services %>% filter(route_id == routes$route_id[j])
  
  for (m in 1:nrow(services2)) {
    
    cat( paste0( "      <transitRoute ", "id=\"", services2$service_id3[m], "\">","\n") )
    cat( paste0("       <transportMode>pt</transportMode>","\n","\n") ) 
    cat("       <routeProfile>","\n")
    
    stopTimes2 <- stopTimes %>% filter(service_id3 == services2$service_id3[m]) %>% 
      arrange(arrival_time) %>% distinct(stop_sequence, .keep_all = TRUE)
    
    for (k in 1:nrow(stopTimes2)) {
      
      cat(paste0("          <stop ", 
                 "refId=\"", stopTimes2$stop_id[k], 
                 "\" arrivalOffset=\"", stopTimes2$arrivalOffset[k], 
                 "\" departureOffset=\"", stopTimes2$departureOffset[k], 
                 "\" awaitDeparture=\"", "true\""), 
          "/>","\n")
      
    }
    cat("        </routeProfile>","\n","\n")
    
    cat("        <route>","\n")
    
    pathLinks2 <- pathLinks %>% filter(service_id3 == services2$service_id3[m])
    
    for (n in 1:nrow(pathLinks2)) {
      
      cat(paste0("         <link ", "refId=\"", pathLinks2$ID[n],"\"/>", "\n"))
      
    }
    cat("        </route>","\n","\n")
    
    departures2 <- departures %>% filter(service_id3 == services2$service_id3[m])  
    
    cat("        <departures>","\n")
    for (p in 1:nrow(departures2)) {
      
      cat(paste0("         <departure id=\"", departures2$trip_id[p], 
                 "\" departureTime=\"", departures2$departure_time[p],
                 "\" vehicleRefId=\"", departures2$vehicle[p],"\"/>", "\n"))
      
    }
    cat("        </departures>","\n")
    cat("      </transitRoute>","\n","\n","\n")
  }
  
  cat("   </transitLine>","\n")
}

cat("</transitSchedule>","\n")

sink()
