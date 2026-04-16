

library(tidyverse)
library(sfnetworks)
library(sf)
library(janitor)
library(REdaS)
library(stringi)

########## load LINKS and NODES ########### ###########

#setwd("~/matsim/dataPrep/network")
#nodesCSV <- read.csv("csv/nodes15apr.csv") # or 8feb?
#names(nodesCSV)[1] <- "ID"     # rename ID column
#editNodes <- function(nodes) {
#  nodes <- nodesCSV %>% select(ID, X, Y)
#  nodes$X <- round(nodes$X, 0)
#  nodes$Y <- round(nodes$Y, 0)
#  nodes
#}
#nodes <- editNodes(nodes = nodesCSV)

#setwd("~/matsim/dataPrep/network/shp")
nodes <- read_sf("shp/nodes15APR.shp")
editNodes <- function(nodes) {
  nodes <- nodes %>% select(ID, X, Y)
  nodes$X <- round(nodes$X, 0)
  nodes$Y <- round(nodes$Y, 0)
  nodes
}
nodes <- editNodes(nodes = nodes) # nodes df with id,x,y cols
st_geometry(nodes) <- NULL # remove sf geometry

#setwd("~/matsim/dataPrep/network/qgis")
links <- read_sf("shp/links22APR.shp")
links <- links %>% filter(bus != "no")  #remove links which buses can't travel along


editLinks <- function(links) {
  links <- links %>% select(ID, fromID, toID, lanes, length, maxspeed, highway, direction, car, bus, bike, walk)
  links$length <- ceiling(links$length)                  # roundup length
  links$maxspeed <- links$maxspeed %>% str_replace(" [a-z]*", "") %>% as.numeric()         # remove "mph"
  links$maxspeed <- ifelse(!is.na(links$maxspeed), links$maxspeed,
                           ifelse(links$highway == "motorway", 70,                  # replace NA with value dependent
                                  ifelse(links$highway == "motorway_link", 60, 30)))    # on highway type
  links <- links %>% mutate(maxspeed =ceiling(maxspeed*0.44704)) # convert to metres per second
  links <- links %>% mutate(timeOnLink = length/maxspeed)
  links <- links %>% mutate(lanes = as.numeric(links$lanes))
  links$permlanes <- ifelse(!is.na(links$lanes), links$lanes,1)
  links <- links %>% select(-lanes, -highway)
  links <- links %>% mutate(capacity = 1000*permlanes) # guess capacity based on lanes
  links <- links %>% mutate(mode = paste0(ifelse(car == "yes","car, ",""),
                                          ifelse(bus == "yes","pt, ",""),
                                          ifelse(bike == "yes","bike, ",""),
                                          ifelse(walk == "yes","walk, ","")
  ))
  links <- links %>% mutate(mode = stringi::stri_replace_last(mode, fixed = ", ", ""))
  
}

links <- editLinks(links = links)

#source('~/matsim/pt/rScript/noBusLinks.R') # now obsolete - set bus == no in qgis links layer
#links <- links %>% filter(ID %notin% noBusLinks) # remove some links for correct pt routing

rm(editLinks,editNodes, noBusLinks)

########## join nodes XY onto links ####### ##############################

fromNodes <- nodes %>% rename(fromX = X) %>% rename(fromY = Y) %>% rename(from = ID)
toNodes <- nodes %>% rename(toX = X) %>% rename(toY = Y) %>% rename(to = ID)

links <- links %>% rename(from = fromID) %>% rename(to = toID)
links <- links %>% left_join(fromNodes,by = "from") %>% left_join(toNodes,by = "to")
links <- links %>% rename(Nfrom = from) %>% rename(Nto = to)
links <- na.omit(links)
rm(toNodes,fromNodes)

########## prepare links ################## #####

library(data.table)
library(sfheaders)
linksDT <- as.data.table(links)
## To use `sfheaders` the data needs to be in long form
linksDT1 <- linksDT[, .(ID,Nfrom,Nto,timeOnLink, x = fromX, y = fromY)]
linksDT2 <- linksDT[, .(ID,Nfrom,Nto,timeOnLink, x = toX, y = toY)]

## Add on a 'sequence' variable so we know which one comes first
linksDT1[, seq := 1L ]
linksDT2[, seq := 2L ]

## put back together
links <- rbindlist(list(linksDT1, linksDT2), use.names = TRUE)
setorder(links, ID, seq)

links <- sfheaders::sf_linestring(
  obj = links
  , x = "x"
  , y = "y"
  , linestring_id = "ID"
  , keep = TRUE
)

links <- links %>% select(-seq)
st_crs(links) <- 27700
links <- st_transform(links, 4326)
rm(linksDT,linksDT1,linksDT2)

########## prepare nodes ################## #########



nodes <- sf_point(nodes, x="X", y="Y", keep = TRUE)
st_crs(nodes) <- 27700
nodes <- st_transform(nodes, 4326)
#nodes <- nodes %>% rename(name = ID)

#network <- sfnetwork(nodes, links, node_key = "name")

########## create network ##############################################
network <- sfnetworks::as_sfnetwork(links,directed = TRUE) %>%
        sfnetworks::activate("edges") %>%
        mutate(weight = sfnetworks::edge_length()) %>% 
        mutate(azimuth = sfnetworks::edge_azimuth())
# join nodes IDs onto network
nodes <- nodes %>% rename(name = ID)
network <- network %>% sfnetworks::activate("nodes") %>% st_join(nodes, join=st_intersects)

saveRDS(network, "rds/network/network.rds")

rm(links, nodes)
  
# paths <- st_network_paths(network, from = "N0GRNF", to = "N0QCRT")

#network_links <- st_crs(network_links, 27700)
#network_links <- st_transform(network_links, 27700)

# created path with link/node indexes
# need to convert indexes to node/link ref id
# find a way to join multiple paths - between stops.
# also need to find way of calculating path from first node before bus stop, to last node after bus stop,
# this will ensure path contains the link where the bus stop is situated.

######### function: list links for route #########
### formatGTFS? : create a paths DF instead of function


