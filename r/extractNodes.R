
########## create nodes and links DFs from network ######### 

network_nodes <- network %>% sfnetworks::activate("nodes") %>% st_as_sf()

