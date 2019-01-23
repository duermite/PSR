##ALL CLEANING FOR FAO_HOSTS FILE
library(dplyr)
hosts <- read.csv("raw_data/fao_hosts.csv")
db_search <- read.csv("raw_data/db_search.csv")
host_depth <- read.csv("raw_data/host_depth.csv")
host_extent <- read.csv("raw_data/host_extent.csv")
host_fishery <- read.csv("clean_data/host_fishery_clean.csv")
host_size <- read.csv("raw_data/host_size.csv")
invasives <- read.csv("clean_data/invasives_clean.csv")
sociality <- read.csv("clean_data/sociality_clean.csv")


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
##final-save to clean data folder



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
hosts2 <- hosts %>% 
  mutate(family=tolower(Family_Group)) %>% 
  mutate(host_type=unlist(lapply(Order_Group,calling_host_type))) %>% #need lapply so work for each row separately
  select(1,16:17,4:15)

##2-
#Add data
hosts3 <- left_join(hosts2,db_search,by="host_genus_species") %>% 
  left_join(host_depth,by="host_genus_species") %>% 
  left_join(host_extent,by="sp_code") %>% 
  left_join(host_fishery,by="sp_code") %>% 
  left_join(host_size,by="sp_code") %>% 
  left_join(invasives,by="host_genus_species") %>% 
  left_join(sociality,by="host_genus_species") %>% 
  unite(ref,c("ref.x","ref.y","ref.x.x","ref.y.y","ref.x.x.x","ref.y.y.y"),sep=", ") %>% 
  select(1:10,21:22,24:25,28,31:35,37:38,41,47:48,11:14) %>% 
  rename(sp_code=sp_code.x)

#SKIP#
#############################
#tried and failed to remove NAs before combining refs, so just combining in script above
#if i decide to make this work, have to stop before unite() in previous script
#remove NAs
hosts3$ref.x[is.na(hosts3$ref.x)] <- ""
hosts3$ref.y[is.na(hosts3$ref.y)] <- ""
hosts3$ref.x.x[is.na(hosts3$ref.x.x)] <- ""
hosts3$ref.y.y[is.na(hosts3$ref.y.y)] <- ""
hosts3$ref.x.x.x[is.na(hosts3$ref.x.x.x)] <- ""
hosts3$ref.y.y.y[is.na(hosts3$ref.y.y.y)] <- ""
#combine ref columns
hosts4 <- hosts3 %>%  
  unite(ref,c("ref.x","ref.y","ref.x.x","ref.y.y","ref.x.x.x","ref.y.y.y"),sep=", ")
############################### 

#RESUME
##final-save to clean data folder
write.csv(hosts3,"clean_data/hosts_clean.csv",
          row.names=FALSE)
