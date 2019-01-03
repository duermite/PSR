##ALL CLEANING TASKS FOR SOCIALITY HERE##

library(dplyr)
sociality_sp <- read.csv("raw_data/sociality_sp.csv")
sociality_fam <- read.csv("raw_data/sociality_fam.csv")


#TASKS
##1-Create a scale for sociality and define
##2-Fill in species from fam, indicate which are interpolated somehow







write.csv(sociality,"clean_data/sociality_clean.csv",
          row.names=FALSE)