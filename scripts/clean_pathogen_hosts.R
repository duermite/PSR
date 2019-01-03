###Checking for duplicates##
library(dplyr)

#import and check for duplicates in pathogen_hosts file

pathogen_hosts <- read.csv("raw_data/pathogen_hosts.csv")

dup_search <- data.frame(table(pathogen_hosts$path_abbr, 
                               pathogen_hosts$host_genus_species))
dup_search <- dup_search[dup_search$Freq>1,]
dup_search

#if there are dups, run to get distinct cases
pathogen_hosts_clean <- pathogen_hosts %>% 
  distinct(path_abbr,host_genus_species,.keep_all=TRUE)

#test dups
dup_search_test <- data.frame(table(pathogen_hosts_clean$path_abbr, 
                              pathogen_hosts_clean$host_genus_species))
dup_search_test <- dup_search_test[dup_search_test$Freq>1,]
dup_search_test

#save final file in clean_data folder
write.csv(pathogen_hosts_clean,"clean_data/pathogen_hosts_clean.csv",
          row.names=FALSE) #this line prevents adding a new column each time
