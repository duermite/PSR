#ALL CLEANING TASKS FOR INVASIVES

library(dplyr)
library(tidyr)

invasives <- read.csv("raw_data/invasives_raw.csv")
fao_hosts <- read.csv("raw_data/fao_hosts.csv") #is this needed?

#Tasks
#1-Find duplicates and combine info
#2-Create new status/impact to denote only high impact spp?
#3-Fix sources and add to reference list
#4-Check how many hosts are listed and ensure list is complete
#5-Use Environment data to check fao_hosts?
#final-Save to clean data folder

#1-
#Find duplicates and combine info