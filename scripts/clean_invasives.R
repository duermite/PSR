#ALL CLEANING TASKS FOR INVASIVES

library(dplyr)
library(tidyr)

invasives <- read.csv("raw_data/invasives_raw.csv")
fao_hosts <- read.csv("raw_data/fao_hosts.csv") #is this needed?

#Tasks
#0.5-limit list to species in fao_hosts
#1-Find duplicates and combine info
#2-Create new status/impact to denote only high impact spp?
#3-Fix sources and add to reference list
#4-Check how many hosts are listed and ensure list is complete <--
#5-Use Environment data to check fao_hosts?
#final-Save to clean data folder

#0.5
#limit list to species in fao_hosts
#adapted from clean_pathogen_hosts script:
#Verify all Taxon in fao_hosts, remove if not
hosts_in_inv_hosts <- invasives %>% 
  arrange(Taxon) %>% 
  separate(Taxon, c("genus","species"),sep=" ") %>% 
  unite("host_genus_species",c("genus","species"),sep="_") %>% 
  select(host_genus_species)
hosts_in_fao_hosts <-  fao_hosts %>% 
  arrange(host_genus_species) %>% 
  distinct(host_genus_species)
#get hosts in invasives that are not in fao_hosts
setdiff(hosts_in_inv_hosts,hosts_in_fao_hosts)
#remove these extra hosts from pathogen_hosts
#fix species name in original invasives file
invasives <- invasives %>% 
  arrange(Taxon) %>% 
  separate(Taxon, c("genus","species"),sep=" ") %>% 
  unite("host_genus_species",c("genus","species"),sep="_")
#join the two tables
invasives2 <- right_join(invasives,fao_hosts, by="host_genus_species") %>% 
  filter(Source!="NA") %>% 
  select(1:8) 
#test that extras are really gone
hosts_in_clean_invasives <- invasives2 %>% 
  arrange(host_genus_species) %>% 
  distinct(host_genus_species)
setdiff(hosts_in_clean_invasives,hosts_in_fao_hosts) #empty


#1-
#Find duplicates and combine info
dup_search <- data.frame(table(invasives2$host_genus_species))
dup_search <- dup_search[dup_search$Freq>1,]
dup_search
#remove NAs
invasives2[is.na(invasives2)] <- ""
invasives3 <- invasives2 %>% 
  group_by(host_genus_species) %>% 
  summarize(inv_in=paste(Invasive.In,collapse="; "),
            impact=paste(Status.Impact,collapse="; "),
            initial_dist=paste(Initial.Distribution,collapse="; "),
            latest_dist=paste(Latest.update.distribution,collapse="; "),
            source=paste(Source,collapse="; "),
            env=paste(Environment,collapse="; ")) 
#remove ; where not needed
rem_semicolon <- function(x){
  substr(x, 1 + (1 * as.numeric(substr(x,1,1)==';')), 
         nchar(x) - (1 * as.numeric(substr(x, nchar(x), 
                                           nchar(x)) == ';')))
}
test <- c("words; words", ";", ";words","words;")
rem_semicolon(test) #it works! 
#apply this function throughout the dataframe...
invasives4 <- invasives3 %>% 
  rowwise() %>% 
  mutate(inv_in=rem_semicolon(inv_in),
         impact=rem_semicolon(impact),
         initial_dist=rem_semicolon(initial_dist),
         latest_dist=rem_semicolon(latest_dist),
         env=rem_semicolon(env))
#nearly there, a few random ; but they will have to stay for now

#2-
#Create new status/impact to denote only high impact spp?
#Rule: If "High" OR "Established", new impact is "high", otherwise "Low/Unk"
impact_names <- function(original){
  if(grepl("High",original)){
    impact <- "High"
  } else if (grepl("Established",original)){
    impact <- "High"
  } else {
    impact <- "Low/Unk"
  }
  return(impact)
}
#testing function:
impact_names("High")
impact_names("High; Low")
impact_names("Low")
impact_names("Established")
#fix this column
invasives5 <- invasives4 %>% 
  rowwise() %>% 
  mutate(impact=impact_names(impact))

#3-
#Fix sources and add to reference list
#add sources to reference list
#EASIN = 521
#AquaNIS = 522
#http://www.ciesm.org/atlas/appendix2.html = 527
#http://issg.org/database/species/search.asp?sts=sss&st=sss&fr=1&x=26&y=13&sn=&rn=&hci=-1&ei=156&lang=EN
##=611
source_to_ref <- function(source){
  if(source=="EASIN"){
    ref <- "521"
  }else if(source=="AquaNIS"){
    ref <- "522"
  }else if(source=="AquaNIS; EASIN"){
    ref <- "522, 521"
  } else if(source=="http://www.ciesm.org/atlas/appendix2.html"){
    ref <- "527"
  } else if(source=="http://issg.org/database/species/search.asp?sts=sss&st=sss&fr=1&x=26&y=13&sn=&rn=&hci=-1&ei=156&lang=EN"|
            source=="GISD"){
    ref <- "611"
  } else {
    ref <- source
  }
  return(ref)
}
#testing function
source_to_ref("EASIN")
source_to_ref("AquaNIS")
source_to_ref("AquaNIS; EASIN")
source_to_ref("GISD")
source_to_ref("689")
source_to_ref(NA)
#fix refs

invasives6 <- invasives5 %>% 
  separate(source, c("s1","s2","s3","s4","s5"),sep="; ") 
invasives6[is.na(invasives6)] <- ""
invasives7 <- invasives6 %>% 
  rowwise() %>% 
  mutate(s1=source_to_ref(s1),
         s2=source_to_ref(s2),
         s3=source_to_ref(s3),
         s4=source_to_ref(s4),
         s5=source_to_ref(s5)) %>% 
  unite("ref",c("s1","s2","s3","s4","s5"),sep=", ")#too many commas!!
#function from stackoverflow to trim any character:
#https://stackoverflow.com/questions/23274035/removing-multiple-commas-and-trailing-commas-using-gsub
TrimMult <- function(x, char=" ") {
  return(gsub(paste0("^", char, "*|(?<=", char, ")", char, "|", char, "*$"),
              "", x, perl=T))
}
invasives8 <- invasives7 %>% 
  rowwise() %>% 
  mutate(ref=TrimMult(ref,char=", "))
#there's still a trailing comma, but whatever

#4-
#Check how many hosts are listed and ensure list is complete 
#AquaNIS website
#ADD:
#Crangon_crangon, established low prevelence, Iceland, origin Atlantic, 2003
#Penaeus_stylirostris, China, 2002, level of certainty: possible
#Penaeus_vannamei, 2005, China, level of certainty: possible
#Palaemon_adspersus, to Canada and Russia, 2011 and 1936, 
colnames(invasives8)
Cc <- c("Crangon_crangon", "Iceland","Low/Unk","Atlantic","",522,"")
Ps <- c("Penaeus_stylirostris","China","Low/Unk","","",522,"")
Pv <- c("Penaeus_vannamei","China","Low/Unk","","",522,"")
Pa <- c("Palaemon_adspersus","Canada; Russia","Low/Unk","","",522,"")
invasives9 <- rbind(invasives8,Cc,Ps,Pv,Pa)

#5-
#Use Environment data to check fao_hosts?

#final-
#Save to clean data folder
write.csv(invasives9,"clean_data/invasives_clean.csv",
          row.names=FALSE)
