
#### SETUP                         ####


library(readr)
library(tidyverse)
library(sf)
library(hms)
library(janitor)

source('~/matsim/pt/rScript/makeNetwork.R')
setwd("~/matsim/pt")

#### IMPORTING ####

#import data from gtfs feed https://data.bus-data.dft.gov.uk/timetable/download/

# imported gtfs files are read into R from gtfs folder
stopTimes <- read_csv("gtfs/stop_times.txt", col_names = TRUE)
agency <- read_csv("gtfs/agency.txt", col_names = TRUE)
routes <- read_csv("gtfs/routes.txt", col_names = TRUE)
trips <- read_csv("gtfs/trips.txt", col_names = TRUE)
calendar <- read_csv("gtfs/calendar.txt", col_names = TRUE)
#stops <- read_csv("gtfs/stops.txt") #gtfs stops file doesn't include bearing which is needed to allocate link
stops <- readRDS("naptan/naptanStops20231011.Rds")



#### FILTERING  ####

calendar <- calendar %>% filter(monday == 1 & tuesday == 1 & wednesday == 1 & thursday == 1 & friday == 1)
mfServices <- calendar$service_id

routes <- routes %>% filter( route_id %in% WofEroutes) # gets 1 & 7 route info

#routes <- routes %>% filter( route_id == 10230 |route_id == 3275 | route_id == 7097)
trips <- trips %>% filter( route_id %in% WofEroutes) %>% filter(service_id %in% mfServices)
trips <- trips %>% group_by(route_id) %>% mutate(vehicle = n_distinct(trip_id))
suppressWarnings( trips <- trips %>%  mutate(vehicle = 1:vehicle) )
trips <- trips %>% mutate(vehicle = str_pad(vehicle,3,side = "left",pad = "0")) %>% 
  mutate(vehicle = paste0("bus_",route_id,"_",vehicle))

tripIDs <- trips$trip_id
stopTimes <- stopTimes %>% filter(trip_id %in% tripIDs)
rm(tripIDs)

stops <- stopTimes %>% select(stop_id) %>% unique() %>% left_join(stops, by="stop_id")
stops <- st_as_sf(stops, crs = 27700)



# join. other route info
stopTimes <- left_join(stopTimes, trips %>% select(route_id, service_id, trip_id, trip_headsign), by = "trip_id")
stopTimes <- left_join(stopTimes, routes %>% select(-route_long_name), by = "route_id")
stopTimes <- left_join(stopTimes, stops %>% select(stop_id, linkRefId, fromID, toID), by = "stop_id")
stopTimes <- stopTimes %>% mutate(service_id2 = paste0(route_id,"_",route_short_name,"_",service_id,str_replace_all(trip_headsign, " ","")))
stopTimes <- stopTimes %>% select(-stop_headsign, -pickup_type, -drop_off_type, -timepoint, -geometry)

#stopTimes <- stopTimes %>% filter(trip_headsign == "Bristol")

stopTimes <- stopTimes %>% 
  
  #get_dupes(linkRefId,service_id2,stop_sequence) %>% mutate(service_id3 = paste0(service_id2,"_",dupe_count)) %>% 
  
  # remove service_id3s where service only calls at one stop
  
  group_by(trip_id) %>% add_tally() %>% ungroup() %>% 
  mutate(service_id3 = paste0(service_id2,n)) %>% 
  unique() %>% arrange(service_id3,stop_sequence) %>% 
  select(-n)

stopTimes <- stopTimes %>% group_by(trip_id) %>%  
  mutate(arrivalOffset = arrival_time - first(arrival_time)) %>% 
  mutate(arrivalOffset = as_hms(as.numeric(arrivalOffset))) %>% 
  mutate(departureOffset = departure_time - first(departure_time)) %>%
  mutate(departureOffset = as_hms(as.numeric(departureOffset)))%>% 
  ungroup()

stopTimes$departureOffset <- as.character(stopTimes$departureOffset)
stopTimes$arrivalOffset <- as.character(stopTimes$arrivalOffset)

services <- stopTimes %>% select(route_id, service_id, route_short_name, service_id3) %>% distinct()

departures <- stopTimes %>% filter(stop_sequence == 0) %>% select(route_id, trip_id, service_id,  service_id3, departure_time)
departures$departure_time <- as.character(departures$departure_time)
departures <- departures %>% group_by(route_id) %>% add_tally()%>% mutate(vehicle = 1:n) %>% 
  mutate(vehicle = str_pad(vehicle,3,side = "left",pad = "0")) %>% mutate(vehicle = paste0("bus_",route_id,"_",vehicle)) %>% ungroup()
departures[is.na(departures)] <- "00:00:00"
#stopLinks <- st_join(oneStops, links, join = st_is_within_distance, dist = 4)

#plot(nearestLink["ID"], reset = FALSE)
#plot(oneStops["stop_id"], add = TRUE)

#library(tmap)
#tmap_mode("view")
#tm_shape(nearestLink) + tm_lines() +
#tm_shape(oneStops) + tm_markers(col = "black", clustering = FALSE)
#tm_shape(l) +tm_lines(col = "red")








