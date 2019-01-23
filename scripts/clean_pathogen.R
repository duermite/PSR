#Cleaning for pathogen table

library(dplyr)
pathogen <- read.csv("raw_data/pathogen.csv")
path_transmission <- read.csv("raw_data/path_transmission.csv")


#Tasks
#1-Search ways each pathogen type is transmitted
#2-Add path_transmission into pathogen table
#3-check for path_abbr dups
#4-complete virus_type and QAQC
#final-Save in clean data folder

#1-
#Search ways each pathogen type is transmitted
#doing this in the excel sheet
