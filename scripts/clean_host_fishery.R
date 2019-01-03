##ALL CLEANING TASKS FOR HOST_FISHERY###

library(dplyr)

#TASKS
##1-
##

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
