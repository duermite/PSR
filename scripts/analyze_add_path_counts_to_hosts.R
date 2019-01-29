library(dplyr)

decapod_hosts <- read.csv("clean_data/hosts_clean.csv")
pathogen_hosts_clean <- read.csv("clean_data/path_hosts_clean.csv")
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
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="barnacle"]))
colnames(num_rhiz) <- c("host_genus_species","num_rhiz")

#count number of bacteria per host
num_bacteria <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="bacteria_extra"|path_type_join_hosts$pathogen_type=="bacteria_intra"]))
colnames(num_bacteria) <- c("host_genus_species","num_bacteria")

#count number of intracellular bacteria per host
num_bacteria_intra <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="bacteria_intra"]))
colnames(num_bacteria_intra) <- c("host_genus_species","num_bacteria_intra")

#count number of microsporidians per host
num_microsp <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="microsporidian"]))
colnames(num_microsp) <- c("host_genus_species","num_microsp")

#count by path transmission type
num_direct <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$transmission=="direct"|
                                            path_type_join_hosts$transmission=="both"]))
colnames(num_direct) <- c("host_genus_species","num_direct")
num_complex <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$transmission=="complex"|
                                            path_type_join_hosts$transmission=="both"]))
colnames(num_complex) <- c("host_genus_species","num_complex")

#count by path requirements
num_obligate <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$requirement=="obligate"]))
colnames(num_obligate) <- c("host_genus_species","num_obligate")
num_opportunist <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$requirement=="opportunist"]))
colnames(num_opportunist) <- c("host_genus_species","num_opportunist")

#count by path size
num_macro <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$size=="macro"]))
colnames(num_macro) <- c("host_genus_species","num_macro")
num_micro <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$size=="micro"]))
colnames(num_micro) <- c("host_genus_species","num_micro")

#add data to decapod_hosts file - only if host in fao db
decapod_hosts2 <- left_join(decapod_hosts,num_pathogen,by="host_genus_species") %>% 
  left_join(num_viruses,by="host_genus_species") %>% 
  left_join(num_isopods,by="host_genus_species") %>% 
  left_join(num_rhiz,by="host_genus_species") %>% 
  left_join(num_bacteria,by="host_genus_species") %>%
  left_join(num_bacteria_intra,by="host_genus_species") %>% 
  left_join(num_microsp,by="host_genus_species") %>% 
  left_join(num_direct,by="host_genus_species") %>% 
  left_join(num_complex,by="host_genus_species") %>% 
  left_join(num_obligate,by="host_genus_species") %>% 
  left_join(num_opportunist,by="host_genus_species") %>% 
  left_join(num_macro,by="host_genus_species") %>% 
  left_join(num_micro,by="host_genus_species") 

##can add more counts here##

#order by species name
decapod_hosts3 <- decapod_hosts2 %>% 
  arrange(host_genus_species)

#save to new file name
write.csv(decapod_hosts3,"analyze_data/hosts_clean_pathcounts.csv",
          row.names=FALSE)
