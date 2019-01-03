###ALL CLEANING TASKS FOR PATHOGEN_HOSTS FILE###
library(dplyr)
pathogen_hosts <- read.csv("raw_data/pathogen_hosts.csv")
pathogen <- read.csv("raw_data/pathogen.csv")
fao_hosts <- read.csv("raw_data/fao_hosts.csv")

#Tasks
##1-Dup check
##2-Make sure all path_abbr are in pathogen
##3-Verify all host_genus_species in fao_hosts, remove if not
##4-complete in_wild to best of ability??

#1
###Checking for duplicates##
#import and check for duplicates in pathogen_hosts file



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

##2
###Make sure all path_abbr are in pathogen####
#two columns that contain all unique path_abbr in each table
path_in_pathogen <- pathogen %>% 
  arrange(path_abbr) %>% 
  distinct(path_abbr) 
path_in_path_hosts <- pathogen_hosts %>% 
  arrange(path_abbr) %>% 
  distinct(path_abbr)
#now I need to make all the abbr in pathogen_hosts are also in pathogen
#setdiff(x,y) gives the elements in x but not in y
setdiff(path_in_path_hosts,path_in_pathogen)



#save final file in clean_data folder
write.csv(pathogen_hosts_clean,"clean_data/pathogen_hosts_clean.csv",
          row.names=FALSE) #this line prevents adding a new column each time
