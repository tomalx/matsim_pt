#TO DO:
#10/3/21 add in service_id_3
#10/3/21 use pathLinks to get link data for <route> node


#### SETUP                         ####
setwd("~/matsim/pt")


# helper functions
source('~/matsim/pt/rScript/pathOfLinks2.R')
#source('~/matsim/pt/rScript/formatGTFS.R')
#source('~/matsim/pt/rScript/formatGTFS.R')

departures <- masterDepartures
pathLinks <- masterPathLinks
stopTimes <- masterStopTimes
services <- masterServices
routes <- masterRoutes
stops <- masterStops


oneStopTimes2 <- stopTimes %>% left_join(network_links, by=c("linkRefId" = "ID")) %>% # make sure unjittered links are joined
  #select(linkRefId,service_id2,stop_sequence,Nfrom,Nto) %>% 
  get_dupes(linkRefId,service_id2,stop_sequence) %>% # mutate(service_id3 = paste0(service_id2,"_",dupe_count)) %>% 
  unique() %>% arrange(service_id3,stop_sequence)  #%>% 
  #group_by(service_id3) %>%  
  #select(linkRefId,service_id3,stop_sequence,Nfrom,Nto)

# filter for testing...
#oneStopTimes2 <- oneStopTimes2 %>% filter(service_id3 %in% selectServiceIDs)

## above pipe creates object oneStopTimes2:
## use this in xml tree creation for route/departure etc
## service_id_3 column gives unique reference for each -
## service route pattern.
## can modify pathOfLinks.R to remove this step.

sink("txt/testScheduleXMLx.xml")

#routes <- routes
#services <- services %>% filter(service_id3 %in% selectServiceIDs)
#stopTimes <- oneStopTimes2 %>%  filter(service_id3 %in% selectServiceIDs)
#departures <- departures %>%  filter(service_id3 %in% selectServiceIDs)
#stops <- oneStops

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
