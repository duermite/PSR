library(dplyr)

decapod_hosts <- read.csv("clean_data/fao_hosts_clean.csv")
pathogen_hosts_clean <- read.csv("clean_data/pathogen_hosts_clean.csv")
pathogen <- read.csv("clean_data/pathogen_clean.csv") 

#Fill in columns for number of pathogens and viruses on decapod_hosts spreadsheet

#Subtasks
##count cases of each host_genus_species
##join pathogen type to pathogen_hosts
##count number of viruses for each host_genus_species

#count cases of each host_genus_species
num_pathogen <- data.frame(table(pathogen_hosts_clean$host_genus_species))
num_pathogen <- num_pathogen[num_pathogen$Freq>0,]
colnames(num_pathogen) <- c("host_genus_species","num_pathogens")
#can do these steps in dplyr somehow

#add pathogen type to pathogen_host sheet
path_type_join_hosts <- inner_join(pathogen_hosts_clean,pathogen,by="path_abbr")

#count number of viruses per host
num_viruses <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="virus"]))
colnames(num_viruses) <- c("host_genus_species","num_viruses")

#count number of isopods per host
num_isopods <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="isopod"]))
colnames(num_isopods) <- c("host_genus_species","num_isopods")

#count number of rhizocephalan barnacles per host
num_rhiz <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="rhizocephalan barnacle"]))
colnames(num_rhiz) <- c("host_genus_species","num_rhiz")

#add data to decapod_hosts file - only if host in fao db
decapod_hosts_clean <- left_join(decapod_hosts,num_pathogen,by="host_genus_species")
decapod_hosts_clean <- left_join(decapod_hosts_clean,num_viruses,by="host_genus_species")
decapod_hosts_clean <- left_join(decapod_hosts_clean,num_isopods,by="host_genus_species")
decapod_hosts_clean <- left_join(decapod_hosts_clean,num_rhiz,by="host_genus_species")

##can add more counts here##

#order by species name
decapod_hosts_clean <- decapod_hosts_clean %>% 
  arrange(host_genus_species)

#save to new file name
write.csv(decapod_hosts_clean,"clean_data/hosts_clean_pathcounts.csv",
          row.names=FALSE)
