##ALL CLEANING TASKS FOR HOST_FISHERY###

library(dplyr)
source <- read.csv("raw_data/CL_FI_PRODUCTION_SOURCE.csv")
production <- read.csv("raw_data/TS_FI_PRODUCTION.csv")
host_fishery <- read.csv("~/Google Drive/PSR/raw_data/host_fishery.csv")

#TASKS
##1-delete unneeded data from TS_FI_PRODUCTION
##2-Change source code to something useful
##3-Remove rows where Quantitiy=0
##4-Group by sp-code and source
##5-...


#1
##Delete unneeded data from production
#right join below includes all observations in fao_hosts matched to those in production by sp_code
production_join <- right_join(production,fao_hosts, by="sp_code") %>% 
  select(1:7)



#This is incomplete but something to start from
#Has not been edited recently

#determine how species are used (capture vs aquaculture)

source <- read.csv("raw_data/fao_global_production/CL_FI_PRODUCTION_SOURCE.csv")
product <- read.csv("raw_data/fao_global_production/TS_FI_PRODUCTION.csv")

#make source codes useful
source_codes <- source %>% 
  mutate(source_code=Code) %>% 
  mutate(product_orig=Name_E) %>% 
  select(source_code,product_orig) %>% 
  separate(col=product_orig,
           into=c("production","habitat"),
           sep=" ",
           remove=FALSE) %>% 
  mutate(habitat=c(NA,"freshwater","brackish","marine",NA))

write.csv(source_codes,"clean_data/source_codes.csv",
          row.names = FALSE)

#make production useful
production <- product %>% 
  rename(source_code=Source) %>% 
  rename(sp_code=Species) %>% 
  select(source_code,sp_code)

#from production, make table of production and habitat 
#try joining to source_code

prod_source <- production %>% 
  inner_join(source_codes,by="source_code") %>% 
  distinct(sp_code,product_orig,production,habitat)

##still working on this##
