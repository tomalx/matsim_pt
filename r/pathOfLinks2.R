## script to create a path of links for each bus route
## needs network file df
## needs stop times df

## 10/3/21 - modified to remove mutate/service_id_3
## (service_id_3 is now created in )

# created path with link/node indexes
# need to convert indexes to node/link ref id
# find a way to join multiple paths - between stops.
# also need to find way of calculating path from first node before bus stop, to last node after bus stop,
# this will ensure path contains the link where the bus stop is situated.

######### function: list links for route #########
### formatGTFS? : create a paths DF instead of function
library(tictoc)
library(sfnetworks)

WofEroutes <- c(6515, 6753) #14781,14780
  #(6515,3860,10230,7097,3144, 5265, 4420, 7044, 5835, 6753, 8675,7068,18502,3271,3272,3273,3274,3275,3276)
#WofEroutes <- c(3860)


#selectServiceIDs <- c("6515_7_264StapleHill_2", "3860_1_266BroomHill_49", "6515_7_265StapleHill_13","10230_X39_285Bristol_41")

#selectServiceIDs <- services$service_id3[c(1,2,4)]

# get network objects including links and nodes DFs
# if file does not exist, run makeNetwork script to create it.
if(file.exists("rds/network/network.rds")){
  network <- readRDS("rds/network/network.rds")
} else {
source('r/makeNetwork.R')
}
source('r/extractLinks.R')
source('r/extractNodes.R')


# get GTFS data
source('r/formatGTFS.R') # imports and formats GTFS txt files

selectServiceIDs <- stopTimes %>% group_by(service_id3, trip_id) %>% count() %>% filter(n >= 2)
selectServiceIDs <- selectServiceIDs$service_id3 %>% unique()
services <- services %>% filter(service_id3 %in% selectServiceIDs) #remove services that only have 1 calling point

pathLinks <- stopTimes %>% 
  select(trip_id,linkRefId,service_id3,stop_sequence,Nfrom = fromID, Nto = toID) %>% 
  #group_by(trip_id) %>% add_tally() %>% ungroup() %>% 
  #mutate(service_id4 = paste0(service_id3,n)) %>% 
  arrange(service_id3,stop_sequence) %>% distinct(service_id3, stop_sequence, .keep_all = T) %>% 
  group_by(service_id3) 

# above pipe uses janitor::get_dupes to ensure one set of stop sequences per service_id3
# might need to do this earlier in process?? UPDATE - most of this pipe moved to formatGTFS.


pathLinks <- pathLinks %>% mutate(pathStart = lag(Nto)) %>% 
  mutate( pathStart = ifelse(is.na(pathStart),Nfrom,pathStart) ) %>% 
  mutate( pathEnd = Nto)  # this finds the nodes corresponding to the links with bus stops

# function: take row of pathLink df, use pathStart and pathEnd as arguments...
# ... to the sfnetworks::st_network_paths function
# ... add one column of output (list of link indexes) onto pathLink df row.
# ... rbind all rows together?


#myPath <- vector()
#pathLink1row <- sample_n(pathLinks, 1)
#myPath2 <- st_network_paths(network,from = pathLink1row$pathStart, to = pathLink1row$pathEnd)
#myPath2 <- myPath2$edge_paths[[1]]
#myPath <- c(myPath, myPath2)

#myPathLinks <- links[myPath,] #use vector to index rows from links df (i.e. filter links df)

#function of above

#getPath <- function(pathLinks){

#myPath <- vector()

#myPath2 <- st_network_paths(network,from = pathLinks$pathStart, to = pathLinks$pathEnd)
#myPath2 <- myPath2$edge_paths[[1]]
#myPath2
#myPath <- links[myPath,]

#}

pathLinks <- pathLinks %>% filter(service_id3 %in% selectServiceIDs)
#pathLinks1 <- pathLinks1 %>% mutate(path = getPath(pathLinks1))

#pathLinks1 <- pathLinks1 %>% mutate(path = c(0))
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

#route_1CC <- pathLinks %>% filter(service_id3 == "3860_1_394CribbsCauseway_41")
#route_1CC <- st_as_sf(route_1CC)

#route_7 <- pathLinks2 %>% filter(service_id3 == "6515_7_264StapleHill_2")
#route_7 <- route_7$ID
#route_7 <- network_links[network_links$ID %in% route_7,]

#route_Y2Bris <- pathLinks %>% filter(service_id3 == "3272_Y2_310Bristol_834")
#route_Y2Yate <- pathLinks %>% filter(service_id3 == "3272_Y2_310Yate_736")
#route_49 <- route_49$ID
#route_49 <- network_links[network_links$ID %in% route_49,]
#st_write(route_49, "shp/test/route_49.shp", overwrite=T)


#setwd("~/matsim/pt")
#st_write(route_1CC, "shp/test/route_1CC.shp", overwrite=T)
#st_write(route_7, "shp/test/route_7.shp", overwrite=T)

#turn pathLinks2 into df with 2 cols:
#col1 = service_id3
#col2 = list of links / edge_paths
# OR df with 3 cols:
#col1 = service_id3
#col2 = linkIdRef
#col3 = pathOrder


#pathLinks <- pathLinks %>% mutate(path = st_network_paths(network,pathStart,pathEnd))
#pathList <- setNames(split(pathLinks, seq(nrow(pathLinks))), rownames(pathLinks))

 myRouteShp <- pathLinks %>% ungroup() %>% select(-service_id3, -edge_paths) %>% unique()
 st_write(myRouteShp, "shp/test/route_39.shp", overwrite=T)
 
 st_write(stops, "shp/test/stops_39.shp" )
 