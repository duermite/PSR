##ALL CLEANING TASKS FOR HOST_FISHERY###

library(dplyr)
library(tidyr)
source <- read.csv("raw_data/CL_FI_PRODUCTION_SOURCE.csv")
production <- read.csv("raw_data/TS_FI_PRODUCTION.csv")
host_fishery <- read.csv("~/Google Drive/PSR/raw_data/host_fishery.csv")

#TASKS
##1-delete unneeded data from TS_FI_PRODUCTION
##2-Change source code to something useful
##3-Remove rows where Quantitiy=0
##4-Group by sp-code and source
##5-Complete aquaculture column in host_fishery
##6-Complete comm_fish column in host_fishery
##7-Add columns for 0/1 Aquaculture and Capture
##8-Column designating "capture" "aquaculture" or "both"
##final-save host_fishery in clean data


#1
##Delete unneeded data from production
#right join below includes all observations in fao_hosts matched to those in production by sp_code
production_join <- right_join(production,fao_hosts, by="sp_code") %>% 
  select(1:7)

##2-
#Change source code to something useful
source_codes <- source %>% 
  mutate(Source=Code) %>% 
  mutate(product_orig=Name_E) %>% 
  select(Source,product_orig) %>% 
  separate(col=product_orig,
           into=c("production","aq_habitat"),
           sep=" ",
           remove=FALSE) %>% 
  mutate(aq_habitat=c(NA,"freshwater","brackish","marine",NA))
#import source code into production
production_join <- left_join(production_join,source_codes,by="Source") %>% 
  select(1:7,9,10) %>% 
  filter(Source!="NA")

##3-
#Remove rows where Quantitiy=0
production_join <- production_join %>% 
  filter(Quantity>0)

##4-
#Group by sp-code and source
production_grouped <- production_join %>% 
  distinct(sp_code,Source,.keep_all=TRUE) %>% 
  select(4,6,8,9)
#Spread aquaculture and capture into 2 columns
production_spread <- production_grouped %>% 
  group_by(sp_code) %>% #this line isn't doing anything
  spread(production,Quantity)

##5-
#Complete aquaculture column in host_fishery
prod_aq <- production_spread %>% 
  filter(Aquaculture!="NA") %>% 
  group_by(sp_code) %>% 
  summarize(Aq_prod=sum(Aquaculture))
host_fishery_aq <- left_join(host_fishery,prod_aq,by="sp_code") %>% 
  select(1,2,5) 

##6-
#Complete comm_fish column in host_fishery
prod_cap <- production_spread %>% 
  filter(Capture!="NA")
host_fishery_aq_cap <- left_join(host_fishery_aq,prod_cap,by="sp_code") %>% 
  arrange(sp_code) %>% 
  select(1:3,6) %>% 
  rename(Cap_prod=Capture)

##7-
#Add columns for 0/1 Aquaculture and Capture
convert_to_01 <- function(production){
  if(is.na(production)){
    print("0")
  } else {
    print("1")
  }
}
convert_to_01(500) #test function

host_fishery_clean <- host_fishery_aq_cap %>% 
  rowwise() %>% #this iterates a function over many rows!! don't need for loops!!!!!
  mutate(aquaculture=as.numeric(convert_to_01(Aq_prod))) %>% 
  mutate(capture=as.numeric(convert_to_01(Cap_prod)))

sum(host_fishery_clean$aquaculture) #32 species in aquaculture
sum(host_fishery_clean$capture) #82 species captured commercially

##8-
#Column designating "capture" "aquaculture" or "both"
combo_prod <- function(aquaculture,capture){
  if(aquaculture==1&capture==1){
    print("both")
  } else if (aquaculture==1&capture==0){
    print("aquaculture")
  } else if (aquaculture==0&capture==1){
    print("capture")
  } else {
    print("neither")
  }
}

combo_prod(1,0) #testing new function

host_fishery_clean <- host_fishery_clean %>% 
  rowwise() %>% #this iterates a function over many rows!! don't need for loops!!!!!
  mutate(production_type=combo_prod(aquaculture,capture))

host_fishery_clean$production_type <- as.factor(host_fishery_clean$production_type)

host_fishery_clean %>% 
  group_by(production_type) %>% 
  count(production_type)


##final-save host_fishery in clean data
write.csv(host_fishery_clean,"clean_data/host_fishery_clean.csv",
          row.names=FALSE)
