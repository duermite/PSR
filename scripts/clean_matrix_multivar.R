#Making matrices for multivariate analysis
library(dplyr)


hosts <- read.csv("analyze_data/hosts_clean_pathcounts100_wild.csv")
pathogens <- read.csv("clean_data/pathogen_clean_wild.csv")

#select needed variables into 2 new dataframes
host_path <- hosts %>% 
  select(num_pathogens,num_viruses,num_isopods,num_rhiz,num_bacteria,
         num_bacteria_extra,num_bacteria_intra,num_microsp, num_api,
         num_cestode,num_fungi,num_nematode,num_trematode,
         num_complex,num_direct,num_obligate,num_opportunist,
         num_macro,num_micro)
colnames(host_path) <- gsub("num_","",colnames(host_path)) 

  
host_char <- hosts %>% 
  select(family,host_type,
         longev_max,aq_hab,path_search_results,aquaculture,capture,
         max_size_mm,invasive,fam_soc_score)

#################
#create dummy variables for host_char
#################
#host_type
host_char_dum <- as.data.frame(model.matrix(~host_char$host_type+0))
colnames(host_char_dum) <- gsub("host_char$host_type","",colnames(host_char_dum)) #doesn't work
col_types <- c(levels(host_char$host_type))
colnames(host_char_dum) <- col_types


#family
host_char_dum2 <- as.data.frame(model.matrix(~host_char$family+0))
col_fams <- c(levels(host_char$family))
colnames(host_char_dum2) <- col_fams

#invasive
host_char_dum3 <- as.data.frame(model.matrix(~host_char$invasive+0))
col_inv <- c(levels(host_char$invasive))
colnames(host_char_dum3) <- c("inv","not_inv")

#aq_hab
dummy_hab <- function(aq_hab){
  if(aq_hab=="fw"){
    hab_new=0
  } else if (aq_hab=="euryhaline"){
    hab_new=1
  }else {
    hab_new=2
  }
  return(hab_new)
}
#testint
dummy_hab("fw")
dummy_hab("euryhaline")
dummy_hab("marine")

#fam_soc_score
levels(host_char$fam_soc_score)
dummy_social <- function(fam_soc_score){
  if(fam_soc_score=="solitary"){
    soc_score=0
  } else if (fam_soc_score=="gregarious"){
    soc_score=2
  } else {
    soc_score=1
  }
  return(soc_score)
}
#testing
dummy_social("solitary")
dummy_social("gregarious")
dummy_social("ontogenetic_gregarious")

#bind all only data frame

host_char2 <- host_char %>% 
  cbind(host_char_dum) %>% 
  cbind(host_char_dum2) %>% 
  cbind(host_char_dum3[1]) %>% 
  rowwise() %>% 
  mutate(habitat=dummy_hab(aq_hab)) %>% 
  mutate(sociality=dummy_social(fam_soc_score)) %>% 
  dplyr::rename(Anomuran="Anomuran crab") %>% 
  dplyr::rename(Brachyuran="Brachyuran crab") %>% 
  select(longev_max,path_search_results,
         aquaculture,capture,max_size_mm,
         Anomuran:sociality)
  
#save new datasets
write.csv(host_char2,"analyze_data/host_char_mult.csv",
          row.names=FALSE)
write.csv(host_path,"analyze_data/host_path_mult.csv",
          row.names=FALSE)

  