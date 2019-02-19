library(dplyr)

decapod_hosts <- read.csv("clean_data/hosts_clean.csv")
pathogen_hosts_clean <- read.csv("clean_data/path_hosts_clean.csv")
pathogen <- read.csv("clean_data/pathogen_clean.csv") 
path_hosts_wild <- read.csv("clean_data/path_hosts_clean_wild.csv")

#Fill in columns for number of pathogens and viruses on decapod_hosts spreadsheet

#Subtasks
##count cases of each host_genus_species
##join pathogen type to pathogen_hosts
##count number of viruses for each host_genus_species
##count other pathogen types
#pathogen index
#virus index
#save 100 spp list
#save 162 spp list

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
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="bacteria_extra"|
                                            path_type_join_hosts$pathogen_type=="bacteria_intra"]))
colnames(num_bacteria) <- c("host_genus_species","num_bacteria")

#count number of intracellular bacteria per host
num_bacteria_intra <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="bacteria_intra"]))
colnames(num_bacteria_intra) <- c("host_genus_species","num_bacteria_intra")

#count number of extracellular bacteria per host
num_bacteria_extra <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="bacteria_extra"]))
colnames(num_bacteria_extra) <- c("host_genus_species","num_bacteria_extra")

#count number of microsporidians per host
num_microsp <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="microsporidian"]))
colnames(num_microsp) <- c("host_genus_species","num_microsp")

#count apicomplexans
num_api <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="apicomplexan"]))
colnames(num_api) <- c("host_genus_species","num_api")

#count cestodes
num_cestode <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="cestode"]))
colnames(num_cestode) <- c("host_genus_species","num_cestode")

#count fungi
num_fungi <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="fungi"]))
colnames(num_fungi) <- c("host_genus_species","num_fungi")

#count nematodes
num_nematode <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="nematode"]))
colnames(num_nematode) <- c("host_genus_species","num_nematode")

#count trematodes
num_trematode <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="trematode"]))
colnames(num_trematode) <- c("host_genus_species","num_trematode")

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
  left_join(num_micro,by="host_genus_species") %>% 
  left_join(num_api,by="host_genus_species") %>% 
  left_join(num_bacteria_extra,by="host_genus_species") %>% 
  left_join(num_cestode,by="host_genus_species") %>% 
  left_join(num_fungi,by="host_genus_species") %>% 
  left_join(num_nematode,by="host_genus_species") %>% 
  left_join(num_trematode,by="host_genus_species")

#calculate proportions of pathogen types
decapod_hosts3 <- decapod_hosts2 %>% 
  mutate(prop_direct=num_direct/(num_direct+num_complex)) %>% 
  mutate(prop_obligate=num_obligate/(num_obligate+num_opportunist)) %>% 
  mutate(prop_macro=num_macro/(num_macro+num_micro))

###############
#FUNCTION: if.na()
if.na <- function(data){
  if(is.na(data)){
    output <- 0
  }else{
    output <- data
  }
  return(output)
}
################

#changes NAs to 0s for all EXCEPT num_pathogens
decapod_hosts4 <- decapod_hosts3 %>%
  rowwise() %>% 
  mutate(num_viruses=if.na(num_viruses)) %>% 
  mutate(num_isopods=if.na(num_isopods)) %>%
  mutate(num_rhiz=if.na(num_rhiz)) %>% 
  mutate(num_bacteria=if.na(num_bacteria)) %>% 
  mutate(num_bacteria_intra=if.na(num_bacteria_intra)) %>% 
  mutate(num_bacteria_extra=if.na(num_bacteria_extra)) %>% 
  mutate(num_microsp=if.na(num_microsp)) %>% 
  mutate(num_direct=if.na(num_direct)) %>% 
  mutate(num_complex=if.na(num_complex)) %>% 
  mutate(num_obligate=if.na(num_obligate)) %>% 
  mutate(num_opportunist=if.na(num_opportunist)) %>% 
  mutate(num_macro=if.na(num_macro)) %>% 
  mutate(num_micro=if.na(num_micro)) %>% 
  mutate(num_api=if.na(num_api)) %>%
  mutate(num_cestode=if.na(num_cestode)) %>%
  mutate(num_fungi=if.na(num_fungi)) %>%
  mutate(num_nematode=if.na(num_nematode)) %>%
  mutate(num_trematode=if.na(num_trematode)) %>%
  mutate(prop_direct=if.na(prop_direct)) %>%
  mutate(prop_obligate=if.na(prop_obligate)) %>%
  mutate(prop_macro=if.na(prop_macro)) %>% 
  arrange(host_genus_species)

#pathogen and virus indices
#change 0s in path_search_results to 0.5
fn_no_zeros <- function(citations){
  if(citations==0){
    new_cit <- 0.5
  } else{
    new_cit <- citations
  }
  return(new_cit)
}
fn_no_zeros(5) #test

decapod_hosts5 <- decapod_hosts4 %>% 
  rowwise() %>% 
  mutate(path_search_results=fn_no_zeros(path_search_results))%>% 
  mutate(path_index=num_pathogens/path_search_results) %>% 
  mutate(virus_index=num_viruses/path_search_results)

#limit to the 100 sp with pathogens
decapod_hosts6 <- decapod_hosts5 %>% 
  filter(!is.na(num_pathogens))

decapod_hosts5 %>% 
  filter(is.na(num_pathogens)) %>% 
  group_by(host_type) %>% 
  count(host_type)

#change sp with NA pathogens to 0 to include in 162 count
decapod_hosts7 <- decapod_hosts5 %>% 
  rowwise() %>% 
  mutate(num_pathogens=if.na(num_pathogens))

#save 100 sp to new file name
write.csv(decapod_hosts6,"analyze_data/hosts_clean_pathcounts100.csv",
          row.names=FALSE)

#save 162 sp to new file name
write.csv(decapod_hosts7,"analyze_data/hosts_clean_pathcounts162.csv",
          row.names=FALSE)


