library(ggplot2)
library(dplyr)
library(cowplot)


decapods <- read.csv("analyze_data/hosts_clean_pathcounts.csv")
pathogens <- read.csv("clean_data/path_hosts_clean.csv")
