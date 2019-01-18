##ALL CLEANING TASKS FOR SOCIALITY HERE##

library(dplyr)
library(tidyr)
sociality_sp <- read.csv("raw_data/sociality_sp.csv")
sociality_fam <- read.csv("raw_data/sociality_fam.csv")
social_aqua <- read.csv("raw_data/social_aquanis.csv")


#TASKS
##1-Create a scale for sociality and define
##2-Fill in species from fam, indicate which are interpolated somehow

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






write.csv(sociality,"clean_data/sociality_clean.csv",
          row.names=FALSE)