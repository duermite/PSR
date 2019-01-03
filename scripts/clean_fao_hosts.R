##ALL CLEANING FOR FAO_HOSTS FILE
library(dplyr)

#Tasks
##1-Change host type to something recognizeable
##2-



#1
#function loop for host type
levels(decapod_hosts_clean$ISSCAAP_Group) #this doesn't work bcuzz fw crustaceans are from all orders
#but could be used for aquatic habitat parameter
#create function
calling_host_type <- function(order){
  if(order=="REPTANTIA"){
    host_type <- "Lobster/Crayfish"
  } else if (order=="NATANTIA"){
    host_type <- "Shrimp"
  } else if (order=="BRACHYURA"){
    host_type <- "Brachyuran crab"
  } else if (order=="ANOMURA"){
    host_type <- "Anomuran crab"
  } else {
    host_type <- NA
  }
  return(host_type)
}
#adjusting family and host type
decapod_hosts_clean <- decapod_hosts_clean %>% 
  mutate(family=tolower(Family_Group)) %>% 
  mutate(host_type=unlist(lapply(Order_Group,calling_host_type))) #need lapply so work for each row separately
