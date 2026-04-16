#### create custom departure board ####

#### identify bus service with most frequent number of departures ####
#### this service will be used as the 'template' for the custom departure board ####

# gets two most frequently departing services

custom_bus_serv <- departures %>% count(service_id3, across(route_id)) %>% rename(count = n)
custom_bus_serv <- custom_bus_serv %>% group_by(route_id) %>% slice_max(count, n=2) %>% ungroup()


####

services <- services %>% filter(service_id3 %in% custom_bus_serv$service_id3)

################################################

## create a custom stopTimes data frame

## find one example departure for each route in each direction

exampleTrip <- stopTimes %>% filter(service_id3 %in% custom_bus_serv$service_id3) %>% group_by(service_id3) %>% 
  sample_n(1) %>% ungroup() %>% select(trip_id) %>% as.vector()

exampleTrip <- stopTimes %>% filter(trip_id %in% exampleTrip$trip_id)

exampleTrip <- exampleTrip %>% 
  
  select(trip_id, route_short_name,linkRefId,service_id2,service_id3,stop_sequence,stop_id,Nfrom = fromID, Nto = toID,
         arrivalOffset, departureOffset)
  
  #select(trip_id, route_short_name, stop_sequence, arrivalOffset, departureOffset) 
####### load("bus6and7env.RData")
## create departure times df

# function takes three parameters (startTime,endTime and headway) and returns
# list of start times written as characters
tripStartTimes <- function(startTime = 5.5, endTime = 23, headway = 60){
  sequenceInSeconds <- seq(from = startTime * 60 * 60, to = endTime * 60 * 60, by = headway * 60)
  as.character(as.difftime(as_hms(sequenceInSeconds)))
}

trip_id <- exampleTrip %>% select(trip_id, stop_sequence)
trip_start_time <- tripStartTimes(headway = 30)   # select headway (and trip start/end times)
trip_start_time <- data.frame(trip_start_time = trip_start_time)
trip_start_time <- merge(trip_id, trip_start_time)

## join/merge with example trips so that there is one trip start time 
## for each example trip

exampleTrip <- exampleTrip %>% left_join(trip_start_time) %>% 
  group_by(trip_id, trip_start_time) %>% 
  mutate(trip_id = paste0(route_short_name,stri_rand_strings(1,4) )) %>% 
  ungroup()

## create stop arrival / departure times by combining the 
## trip start time and dep/arr offset fields

exampleTrip <- exampleTrip %>% mutate(arrival_time = as.difftime(as_hms(trip_start_time)) + as.difftime(as_hms(arrivalOffset))) %>% 
  mutate(arrival_time = as.character(as_hms(arrival_time)))

exampleTrip <- exampleTrip %>% mutate(departure_time = as.difftime(as_hms(trip_start_time)) + as.difftime(as_hms(departureOffset))) %>% 
  mutate(departure_time = as.character(as_hms(departure_time)))

stopTimes <- exampleTrip

## create new departure df using the exampleTrip DF as a template

departures <- exampleTrip %>% filter(stop_sequence == 0) %>% 
  select(route_short_name,trip_id,service_id3, departure_time) 
departures <- departures %>% mutate(vehicle = paste0("bus",route_short_name,stri_rand_strings(nrow(departures),6) ))

#################################################

pathLinks <- stopTimes %>% 
  select(trip_id,linkRefId,service_id2,service_id3,stop_sequence,Nfrom, Nto) %>% 
  #group_by(trip_id) %>% add_tally() %>% ungroup() %>% 
  #mutate(service_id4 = paste0(service_id3,n)) %>% 
  arrange(service_id3,stop_sequence) %>% distinct(service_id3, stop_sequence, .keep_all = T) %>% 
  group_by(service_id3) 

pathLinks <- pathLinks %>% mutate(pathStart = lag(Nto)) %>% 
  mutate( pathStart = ifelse(is.na(pathStart),Nfrom,pathStart) ) %>% 
  mutate( pathEnd = Nto)  # this finds the nodes corresponding to the links with bus stops

pathLinks <- pathLinks %>% filter(service_id3 %in% selectServiceIDs)

myPath <- tibble()
tic("make path links:")
for(i in 1:nrow(pathLinks)){
  myPath2 <- st_network_paths(network,from = pathLinks$pathStart[i], to = pathLinks$pathEnd[i], weights = "timeOnLink")
  #myPath2 <- myPath2$edge_paths
  myPath <- rbind(myPath,myPath2)
  #pathLinks1[i,] <- pathLinks1[i,] %>% mutate(path = myPath2)
}
toc()
pathLinks <- cbind(pathLinks,myPath) %>% select(service_id3,stop_sequence,edge_paths)
pathLinks <- unnest(pathLinks, cols = c(edge_paths))
pathLinks <- pathLinks %>% left_join(network_links %>% select(ID,edge_paths), by="edge_paths")

oneStopTimes2 <- stopTimes %>% left_join(network_links, by=c("linkRefId" = "ID")) %>% # make sure unjittered links are joined
  #select(linkRefId,service_id2,stop_sequence,Nfrom,Nto) %>% 
  get_dupes(linkRefId,service_id2,stop_sequence) %>% # mutate(service_id3 = paste0(service_id2,"_",dupe_count)) %>% 
  unique() %>% arrange(service_id3,stop_sequence)


