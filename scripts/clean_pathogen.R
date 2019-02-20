#Cleaning for pathogen table

library(dplyr)
library(tidyr)
pathogen <- read.csv("raw_data/pathogen.csv")
path_transmission <- read.csv("raw_data/path_transmission.csv")
path_hosts <- read.csv("clean_data/path_hosts_clean.csv")
path_hosts_wild <- read.csv("clean_Data/path_hosts_clean_wild.csv")


#Tasks
#1-Search ways each pathogen type is transmitted
#2-Add path_transmission, pathogen requirements, and pathogen size into pathogen table
#3-check for path_abbr dups
#4-complete virus_type and QAQC
#5-Limit to only those pathogens that have hosts in fao_hosts
#final-Save in clean data folder

#1-
#Search ways each pathogen type is transmitted
#doing this in the excel sheet

#2-
#Add path_transmission, pathogen requirements, and pathogen size into pathogen table
levels(pathogen$pathogen_type)

pathogen2 <- left_join(pathogen,path_transmission,by="pathogen_type") %>% 
  unite(ref,c("ref.x","ref.y"),sep=", ") %>% 
  select(1:6,13:15,7:10)

#3-
#check for path_abbr dups
dup_search <- data.frame(table(pathogen2$path_abbr))
dup_search <- dup_search[dup_search$Freq>1,]
dup_search
#no duplicates!!

#4-
#complete virus_type and QAQC
#completing this in the excel spreadsheet
#did this to a minor extent
#note: virus_type is sometimes ORder and sometimes Family

#5-Limit to only those pathogens that have hosts in fao_hosts
pathogen3 <- semi_join(pathogen2,path_hosts,by="path_abbr")

#6-pathogen sheet with those from wild only
pathogen4 <- semi_join(pathogen2,path_hosts_wild,by="path_abbr")

#final-
#Save in clean data folder
write.csv(pathogen3,"clean_data/pathogen_clean.csv",
          row.names=FALSE)

write.csv(pathogen4,"clean_data/pathogen_clean_wild.csv")




