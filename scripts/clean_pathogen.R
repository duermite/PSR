#Cleaning for pathogen table

library(dplyr)
pathogen <- read.csv("raw_data/pathogen.csv")
path_transmission <- read.csv("raw_data/path_transmission.csv")


#Tasks
#1-Search ways each pathogen type is transmitted
#2-Add path_transmission into pathogen table
#3-check for path_abbr dups
#4-complete virus_type and QAQC
#final-Save in clean data folder

#1-
#Search ways each pathogen type is transmitted
#doing this in the excel sheet

#2-
#Add path_transmission into pathogen table
pathogen2 <- left_join(pathogen,path_transmission,by="pathogen_type") %>% 
  unite(ref,c("ref.x","ref.y"),sep=", ") %>% 
  select(1:6,13,7:10)

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




