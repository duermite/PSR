library(dplyr)
library(tidyr)

decapod_hosts <- read.csv("clean_data/hosts_clean.csv")
pathogen_hosts_clean <- read.csv("clean_data/path_hosts_clean_wild.csv")
pathogen <- read.csv("clean_data/pathogen_clean.csv") 

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

path_types <- as.data.frame(levels(pathogen$pathogen_type))

#count number of acanthocephalan per host
num_acanth <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="acanthocephalan"]))
colnames(num_acanth) <- c("host_genus_species","num_acanth")

#count number of amoeba per host
num_amoeba <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="amoeba"]))
colnames(num_amoeba) <- c("host_genus_species","num_amoeba")

#count apicomplexans
num_api <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="apicomplexan"]))
colnames(num_api) <- c("host_genus_species","num_api")

#count number of extracellular bacteria per host
num_bacteria_extra <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="bacteria_extra"]))
colnames(num_bacteria_extra) <- c("host_genus_species","num_bacteria_extra")

#count number of intracellular bacteria per host
num_bacteria_intra <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="bacteria_intra"]))
colnames(num_bacteria_intra) <- c("host_genus_species","num_bacteria_intra")

#count number of rhizocephalan barnacles per host
num_rhiz <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="barnacle"]))
colnames(num_rhiz) <- c("host_genus_species","num_rhiz")

#count number of branchiobdellid per host
num_branch <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="branchiobdellid"]))
colnames(num_branch) <- c("host_genus_species","num_branch")

#count cestodes
num_cestode <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="cestode"]))
colnames(num_cestode) <- c("host_genus_species","num_cestode")

#count ciliate
num_ciliate <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="ciliate"]))
colnames(num_ciliate) <- c("host_genus_species","num_ciliate")

#count copepod
num_copepod <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="copepod"]))
colnames(num_copepod) <- c("host_genus_species","num_copepod")

#count dinoflagellate
num_dino <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="dino"]))
colnames(num_dino) <- c("host_genus_species","num_dino")

#count fungi
num_fungi <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="fungi"]))
colnames(num_fungi) <- c("host_genus_species","num_fungi")

#count halacarid
num_mite <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="halacarid"]))
colnames(num_mite) <- c("host_genus_species","num_mite")

#count haplosporidian
num_haplo <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="haplosporidian"]))
colnames(num_haplo) <- c("host_genus_species","num_haplo")

#count number of isopods per host
num_isopods <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="isopod"]))
colnames(num_isopods) <- c("host_genus_species","num_isopods")

#count number of leech per host
num_leech <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="leech"]))
colnames(num_leech) <- c("host_genus_species","num_leech")

#count number of mesomycetozoan per host
num_meso <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="mesomycetozoan"]))
colnames(num_meso) <- c("host_genus_species","num_meso")

#count number of microsporidians per host
num_microsp <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="microsporidian"]))
colnames(num_microsp) <- c("host_genus_species","num_microsp")

#count nematodes
num_nematode <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="nematode"]))
colnames(num_nematode) <- c("host_genus_species","num_nematode")

#count nematomorpha
num_nematomorpha <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="nematomorpha"]))
colnames(num_nematomorpha) <- c("host_genus_species","num_nematomorpha")

#count nemertean
num_nemertean <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="nemertean"]))
colnames(num_nemertean) <- c("host_genus_species","num_nemertean")

#count oomycete
num_oomycete <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="oomycete"]))
colnames(num_oomycete) <- c("host_genus_species","num_oomycete")

#count paramixid
num_paramixid <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="paramixid"]))
colnames(num_paramixid) <- c("host_genus_species","num_paramixid")

#count trematodes
num_trematode <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="trematode"]))
colnames(num_trematode) <- c("host_genus_species","num_trematode")

#count number of viruses per host
num_viruses <- data.frame(table(
  path_type_join_hosts$host_genus_species[path_type_join_hosts$pathogen_type=="virus"]))
colnames(num_viruses) <- c("host_genus_species","num_viruses")

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
  left_join(num_acanth,by="host_genus_species") %>%
  left_join(num_amoeba,by="host_genus_species") %>%
  left_join(num_api,by="host_genus_species") %>% 
  left_join(num_bacteria_extra,by="host_genus_species") %>% 
  left_join(num_bacteria_intra,by="host_genus_species") %>% 
  left_join(num_rhiz,by="host_genus_species") %>%
  left_join(num_branch,by="host_genus_species") %>%
  left_join(num_cestode,by="host_genus_species") %>% 
  left_join(num_ciliate,by="host_genus_species") %>%
  left_join(num_copepod,by="host_genus_species") %>%
  left_join(num_dino,by="host_genus_species") %>%
  left_join(num_fungi,by="host_genus_species") %>%
  left_join(num_mite,by="host_genus_species") %>%
  left_join(num_haplo,by="host_genus_species") %>%
  left_join(num_isopods,by="host_genus_species") %>% 
  left_join(num_leech,by="host_genus_species") %>% 
  left_join(num_meso,by="host_genus_species") %>% 
  left_join(num_microsp,by="host_genus_species") %>% 
  left_join(num_nematode,by="host_genus_species") %>% 
  left_join(num_nematomorpha,by="host_genus_species") %>% 
  left_join(num_nemertean,by="host_genus_species") %>% 
  left_join(num_oomycete,by="host_genus_species") %>% 
  left_join(num_paramixid,by="host_genus_species") %>% 
  left_join(num_trematode,by="host_genus_species") %>% 
  left_join(num_viruses,by="host_genus_species") %>% 
  left_join(num_direct,by="host_genus_species") %>% 
  left_join(num_complex,by="host_genus_species") %>% 
  left_join(num_obligate,by="host_genus_species") %>% 
  left_join(num_opportunist,by="host_genus_species") %>% 
  left_join(num_macro,by="host_genus_species") %>% 
  left_join(num_micro,by="host_genus_species")
  
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
  mutate_at(c("num_acanth","num_amoeba","num_api","num_bacteria_extra",
              "num_bacteria_intra","num_rhiz","num_branch","num_cestode",
              "num_ciliate","num_copepod","num_dino","num_fungi",
              "num_mite","num_haplo","num_isopods","num_leech",
              "num_meso","num_microsp","num_nematode","num_nematomorpha",
              "num_nemertean","num_oomycete","num_paramixid",
              "num_trematode","num_viruses","num_direct","num_complex",
              "num_obligate","num_opportunist","num_macro","num_micro",
              "prop_direct","prop_obligate","prop_macro"), if.na)

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

#change sp with NA pathogens to 0 to include in 162 count
decapod_hosts7 <- decapod_hosts5 %>% 
  rowwise() %>% 
  mutate(num_pathogens=if.na(num_pathogens))

#save 100 sp to new file name
write.csv(decapod_hosts6,"analyze_data/hosts_clean_pathcounts100_wild.csv",
          row.names=FALSE)

#save 162 sp to new file name
write.csv(decapod_hosts7,"analyze_data/hosts_clean_pathcounts162_wild.csv",
          row.names=FALSE)


