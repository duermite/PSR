##ALL CLEANING FOR FAO_HOSTS FILE
library(dplyr)
hosts <- read.csv("raw_data/fao_hosts.csv")
db_search <- read.csv("raw_data/db_search.csv")
host_depth <- read.csv("raw_data/host_depth.csv")
host_extent <- read.csv("raw_data/host_extent.csv")
host_fishery <- read.csv("raw_data/host_fishery.csv")
host_size <- read.csv("raw_data/host_size.csv")
invasives <- read.csv("raw_data/invasives.csv")
sociality <- read.csv("raw_data/sociality.csv")

#Tasks
##1-Change host type to something recognizeable
##2-Add data
###db_search
###host_depth
###host_extent
###host_fishery
###host_size
###invasives
###sociality
##3-save to clean data folder



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
hosts <- hosts %>% 
  mutate(family=tolower(Family_Group)) %>% 
  mutate(host_type=unlist(lapply(Order_Group,calling_host_type))) #need lapply so work for each row separately
#haven't deleted original two columns yet

write.csv(hosts,"clean_data/hosts_clean.csv",
          row.names=FALSE)