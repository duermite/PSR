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
invasives2 <- right_join(invasives,fao_hosts, by="host_genus_species") %>% 
  filter(Source!="NA") %>% 
  select(1:8) 
#test that extras are really gone
hosts_in_clean_invasives <- invasives2 %>% 
  arrange(host_genus_species) %>% 
  distinct(host_genus_species)
setdiff(hosts_in_clean_invasives,hosts_in_fao_hosts) #empty

#1-
#Find duplicates and combine info
dup_search <- data.frame(table(invasives2$host_genus_species))
dup_search <- dup_search[dup_search$Freq>1,]
dup_search
#remove NAs
invasives2[is.na(invasives2)] <- ""
invasives3 <- invasives2 %>% 
  group_by(host_genus_species) %>% 
  summarize(inv_in=paste(Invasive.In,collapse="; "),
            impact=paste(Status.Impact,collapse="; "),
            initial_dist=paste(Initial.Distribution,collapse="; "),
            latest_dist=paste(Latest.update.distribution,collapse="; "),
            source=paste(Source,collapse="; "),
            env=paste(Environment,collapse="; ")) 
#remove ; where not needed
rem_semicolon <- function(x){
  substr(x, 1 + (1 * as.numeric(substr(x,1,1)==';')), 
         nchar(x) - (1 * as.numeric(substr(x, nchar(x), 
                                           nchar(x)) == ';')))
}
test <- c("words; words", ";", ";words","words;")
rem_semicolon(test) #it works! 
#apply this function throughout the dataframe...
invasives4 <- invasives3 %>% 
  rowwise() %>% 
  mutate(inv_in=rem_semicolon(inv_in),
         impact=rem_semicolon(impact),
         initial_dist=rem_semicolon(initial_dist),
         latest_dist=rem_semicolon(latest_dist),
         env=rem_semicolon(env))
#nearly there, a few random ; but they will have to stay for now