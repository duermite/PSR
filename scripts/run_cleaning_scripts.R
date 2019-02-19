#RUN SCRIPTS
#These need to be run, in this order, for final cleaning

source("scripts/clean_pathogen_hosts.R")

source("scripts/clean_host_fishery.R")

source("scripts/clean_invasives.R")

source("scripts/clean_sociality.R")

source("scripts/clean_pathogen.R")

source("scripts/clean_fao_hosts.R") #this one must be done AFTER several of above

source("scripts/analyze_add_path_counts_to_hosts.R")
#warnings (19) are from joining, shouldn't be an issue

source("scripts/analyze_add_path_counts_inwildonly.R")

