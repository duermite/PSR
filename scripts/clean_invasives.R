#ALL CLEANING TASKS FOR INVASIVES

library(dplyr)
library(tidyr)

invasives <- read.csv("raw_data/invasives_raw.csv")
fao_hosts <- read.csv("raw_data/fao_hosts.csv") #is this needed?

#Tasks
#0.5-limit list to species in fao_hosts
#1-Find duplicates and combine info
#2-Create new status/impact to denote only high impact spp?
#3-Fix sources and add to reference list
#4-Check how many hosts are listed and ensure list is complete
#5-Use Environment data to check fao_hosts?
#final-Save to clean data folder

#0.5
#limit list to species in fao_hosts
#adapted from clean_pathogen_hosts script:
#Verify all Taxon in fao_hosts, remove if not
hosts_in_inv_hosts <- invasives %>% 
  arrange(Taxon) %>% 
  separate(Taxon, c("genus","species"),sep=" ") %>% 
  unite("host_genus_species",c("genus","species"),sep="_") %>% 
  select(host_genus_species)
hosts_in_fao_hosts <-  fao_hosts %>% 
  arrange(host_genus_species) %>% 
  distinct(host_genus_species)
#get hosts in invasives that are not in fao_hosts
setdiff(hosts_in_inv_hosts,hosts_in_fao_hosts)
#remove these extra hosts from pathogen_hosts
#fix species name in original invasives file
invasives <- invasives %>% 
  arrange(Taxon) %>% 
  separate(Taxon, c("genus","species"),sep=" ") %>% 
  unite("host_genus_species",c("genus","species"),sep="_")
#join the two tables
invasives_clean <- right_join(invasives,fao_hosts, by="host_genus_species") %>% 
  filter(Source!="NA") %>% 
  select(1:8) 
#test that extras are really gone
hosts_in_clean_invasives <- invasives_clean %>% 
  arrange(host_genus_species) %>% 
  distinct(host_genus_species)
setdiff(hosts_in_clean_invasives,hosts_in_fao_hosts) #empty

#1-
#Find duplicates and combine info
dup_search <- data.frame(table(invasives$Taxon))
dup_search <- dup_search[dup_search$Freq>1,]
dup_search
