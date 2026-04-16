
#### SETUP                         ####
setwd("~/matsim/pt")


# helper functions
#source('~/matsim/pt/rScript/formatGTFS.R')


sink("txt/vehicles2bph.xml")

cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>","\n")
cat("<vehicleDefinitions xmlns=\"http://www.matsim.org/files/dtd\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.matsim.org/files/dtd http://www.matsim.org/files/dtd/vehicleDefinitions_v2.0.xsd\">","\n","\n")

cat(" <vehicleType id=\"defaultTransitVehicleType\">", "\n")
cat("   <capacity seats=\"101\" standingRoomInPersons=\"0\">", "\n")    
cat("   </capacity>", "\n")    
cat("   <length meter=\"7.5\"/>", "\n")    
cat("   <width meter=\"1.0\"/>", "\n")    
cat("   <costInformation>", "\n")    
cat("   </costInformation>", "\n")    
cat("   <passengerCarEquivalents pce=\"0.0\"/>", "\n")    
cat("   <networkMode networkMode=\"car\"/>", "\n")    
cat("   <flowEfficiencyFactor factor=\"1.0\"/>", "\n")    
cat(" </vehicleType>", "\n", "\n")    
  
for (i in 1:nrow(departures)){
  
  
  
  cat(paste0(" <vehicle id=\"",departures$vehicle[i],"\" type=\"defaultTransitVehicleType\"/>", "\n"))
  
}

cat("</vehicleDefinitions>")

sink()

