########## create nodes and links DFs from network ######### 
network_links <- network %>% sfnetworks::activate("edges") %>% st_as_sf() %>% mutate(edge_paths = row_number())

#calculate bearing
network_links <- network_links %>% mutate(bearing = (as.numeric(azimuth))*(180/pi))

#transform back to GB grid crs
network_links <- st_transform(network_links, 27700)

#use FILTER to mutate bearing into 8 compass points
lN <- network_links %>% filter(bearing >= -22.5 & bearing < 22.5) %>% mutate(compass = "N")
lNE <- network_links %>% filter(bearing >= 22.5 & bearing < 67.5) %>% mutate(compass = "NE")
lE <- network_links %>% filter(bearing >= 67.5 & bearing < 112.5) %>% mutate(compass = "E")
lSE <- network_links %>% filter(bearing >= 112.5 & bearing < 157.5) %>% mutate(compass = "SE")
lS <- network_links %>% filter(bearing >= 157.5 & bearing <= 180 | bearing >= -180 & bearing <= -157.5 ) %>% mutate(compass = "S")
lSW <- network_links %>% filter(bearing >= -157.5 & bearing < -112.5) %>% mutate(compass = "SW")
lW <- network_links %>% filter(bearing >= -112.5 & bearing < -67.5) %>% mutate(compass = "W")
lNW <- network_links %>% filter(bearing >= -67.5 & bearing < -22.5) %>% mutate(compass = "NW")

#depending on bearing, move sf object accordingly - affine transformation.
lN <- lN %>% mutate(geometry = st_geometry(lN)+ c(-1,0)) %>% st_set_crs(27700)
lNE <- lNE %>% mutate(geometry = st_geometry(lNE)+ c(-1,1)) %>% st_set_crs(27700)
lE <- lE %>% mutate(geometry = st_geometry(lE)+ c(0,1)) %>% st_set_crs(27700)
lSE <- lSE %>% mutate(geometry = st_geometry(lSE)+ c(1,1)) %>% st_set_crs(27700)
lS <- lS %>% mutate(geometry = st_geometry(lS)+ c(1,0)) %>% st_set_crs(27700)
lSW <- lSW %>% mutate(geometry = st_geometry(lSW)+ c(1,-1)) %>% st_set_crs(27700)
lW <- lW %>% mutate(geometry = st_geometry(lW)+ c(0,-1)) %>% st_set_crs(27700)
lNW <- lNW %>% mutate(geometry = st_geometry(lNW)+ c(-1,-1)) %>% st_set_crs(27700)

network_links <- rbind(lN,lNE,lE,lSE,lS,lSW,lW,lNW)
rm(lN,lNE,lE,lSE,lS,lSW,lW,lNW)

#network_links <- st_crs(network_links, 27700)
#network_links <- st_transform(network_links, 27700)