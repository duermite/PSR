##ALL CLEANING TASKS FOR SOCIALITY HERE##

library(dplyr)
library(tidyr)
sociality_sp <- read.csv("raw_data/sociality_sp.csv")
sociality_fam <- read.csv("raw_data/sociality_fam.csv")
social_aqua <- read.csv("raw_data/social_aquanis.csv")


#TASKS
##1-Create a scale for sociality and define
##2-Fill in species from fam, indicate which are interpolated somehow
##3-change numbers to something meaningful
#final-save to clean data folder

##1-
#Create a scale for sociality and define, 0-4
#0: unknown
#1: completely solitary, can include dense pops that are aggressive
#2: reproductive aggregations
#3: ontonetic agg., some life stage is gregarious
#4: completely (or mostly) gregarious
##doing this in excel, with version control in Git



##2-
#Fill in species from fam, indicate which are interpolated somehow
interpolate_social <- function(social_score,fam_score){
  if(social_score>0){
    fam_soc_score <- social_score
  } else{
    fam_soc_score <- fam_score
  }
  return(fam_soc_score)
}

sociality_sp2 <- left_join(sociality_sp,sociality_fam,by="Family_Group") %>% 
  rename(fam_score=social_score.y) %>% 
  rowwise() %>% 
  mutate(fam_soc_score=interpolate_social(social_score.x,fam_score)) %>% 
  select(1:4,13,5:6) %>% 
  rename(sociality=sociality.x,
         social_score=social_score.x,
         notes=notes.x,
         ref=ref.x)

##3-change numbers to something meaningful
social_naming <- function(social_score){
  if(social_score==1){
    social_name <- "solitary"
  } else if(social_score==2){
    social_name <- "repro_aggregate"
  } else if(social_score==3){
    social_name <- "ontogenetic_gregarious"
  } else if(social_score==4){
    social_name <- "gregarious"
  } else{
    social_name <- "unknown"
  }
  return(social_name)
}
#test function
social_naming(0)

sociality_sp3 <- sociality_sp2 %>% 
  rowwise() %>% 
  mutate(social_score=social_naming(social_score)) %>% 
  mutate(fam_soc_score=social_naming(fam_soc_score))

#final-save to clean data folder
write.csv(sociality_sp3,"clean_data/sociality_clean.csv",
          row.names=FALSE)
