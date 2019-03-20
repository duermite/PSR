library(dplyr)

decapods <- read.csv("analyze_data/hosts_clean_pathcounts100.csv")
path_hosts <- read.csv("clean_data/path_hosts_clean.csv")
pathogens <- read.csv("clean_data/pathogen_clean.csv")
decapods_wild <- read.csv("analyze_data/hosts_clean_pathcounts100_wild.csv")
path_hosts_wild <- read.csv("clean_data/path_hosts_clean_wild.csv")

path_hosts %>% 
  group_by(in_wild) %>% 
  count(in_wild)

path1 <- aov(num_pathogens~host_type,data=decapods)
summary(path1)

path2 <- aov((path_index)~host_type,
             data=decapods)
summary(path2)

path_wild <- aov(num_pathogens~host_type,data=decapods_wild)
summary(path_wild)

path_wild2 <- aov((num_pathogens/path_search_results)~host_type,
             data=decapods_wild)
summary(path_wild2)


vir1 <- aov((num_viruses)~host_type,
            data=decapods)
summary(vir1)
TukeyHSD(vir1)

vir2 <- aov((num_viruses/path_search_results)~host_type,
            data=decapods)
summary(vir2)
TukeyHSD(vir2)

vir_wild <- aov((num_viruses)~host_type,
            data=decapods_wild)
summary(vir_wild)
TukeyHSD(vir_wild)
decapods_wild %>% 
  group_by(host_type) %>% 
  summarize(mean(num_viruses))

vir_wild2 <- aov((num_viruses/path_search_results)~host_type,
            data=decapods_wild)
summary(vir_wild2)
TukeyHSD(vir_wild2)
decapods_wild %>% 
  group_by(host_type) %>% 
  summarize(mean(virus_index))

iso1 <- aov((num_isopods/path_search_results)~host_type,
            data=decapods)
summary(iso1)


barnacles <- aov((num_rhiz/path_search_results)~host_type,
                 data=decapods)
summary(barnacles)

bacteria <- aov((num_bacteria/path_search_results)~host_type,
                data=decapods)
summary(bacteria)

microsp <- aov((num_microsp/path_search_results)~host_type,
               data=decapods)
summary(microsp)

api <- aov((num_api/path_search_results)~host_type,
           data=decapods)
summary(api)

trematode <- aov((num_trematode/path_search_results)~host_type,
                 data=decapods_wild)
summary(trematode)
TukeyHSD(trematode)

trem_list <- decapods_wild %>% 
  select(host_genus_species, host_type, num_trematode,path_search_results) %>% 
  mutate(trem_index=num_trematode/path_search_results) %>% 
  arrange(desc(trem_index)) #doesn't work

fungi <- aov((num_fungi)~host_type,
             data=decapods)
summary(fungi)

prop_obligate <- aov((prop_obligate)~host_type,
                     data=decapods)
summary(prop_obligate)
TukeyHSD(prop_obligate)

#direct
prop_direct <- aov((prop_direct)~host_type,
                   data=decapods)
summary(prop_direct)
TukeyHSD(prop_direct)
#direct and sociality
direct_soc <- aov((num_direct/path_search_results)~fam_soc_score,
                  data=decapods_wild)
summary(direct_soc)
complex_soc <- aov((num_complex/path_search_results)~fam_soc_score,
                  data=decapods_wild)
summary(complex_soc)

prop_macro <- aov((prop_macro)~host_type,
                  data=decapods)
summary(prop_macro)
TukeyHSD(prop_macro)

longev <- aov(longev_max~host_type,data=decapods)
summary(longev)
TukeyHSD(longev)

longev_wild<- aov(longev_max~host_type,data=decapods_wild)
summary(longev_wild)
TukeyHSD(longev_wild)

longev2 <- lm((num_pathogens)~(longev_max),data=decapods)
summary(longev2)

longev_wild2 <- lm((num_pathogens)~(longev_max),data=decapods_wild)
summary(longev_wild2)

longev3 <- lm(virus_index~longev_max,data=decapods_wild)
summary(longev3)

longev4 <- lm(path_index~longev_max,data=decapods_wild)
summary(longev4)

#lm for each taxon
decapods_anomuran <- decapods_wild %>% 
  filter(host_type=="Anomuran crab")
longev_ano <- lm(path_index~longev_max,
                 data=decapods_anomuran)
summary(longev_ano)

decapods_brachyuran <- decapods_wild %>% 
  filter(host_type=="Brachyuran crab")
longev_brach <- lm(path_index~longev_max,
                 data=decapods_brachyuran)
summary(longev_brach)

decapods_lobster <- decapods_wild %>% 
  filter(host_type=="Lobster/Crayfish")
longev_lob <- lm(path_index~longev_max,
                 data=decapods_lobster)
summary(longev_lob)

decapods_shrimp <- decapods_wild %>% 
  filter(host_type=="Shrimp")
longev_shrimp <- lm(path_index~longev_max,
                 data=decapods_shrimp)
summary(longev_shrimp)

longev5 <- lm(virus_index~longev_max,data=decapods_wild)
summary(longev5)

social <- aov(num_pathogens~fam_soc_score,data=decapods_wild)
summary(social)
TukeyHSD(social)

social2 <- aov(path_index~fam_soc_score,data=decapods_wild)
summary(social2)

social3 <- aov(virus_index~fam_soc_score,data=decapods_wild)
summary(social3)

social4 <- aov(num_viruses~fam_soc_score,data=decapods_wild)
summary(social4)

habitat <- aov(num_pathogens~aq_hab,data=decapods_wild)
summary(habitat)
TukeyHSD(habitat)

habitat2 <- aov(num_viruses~aq_hab,data=decapods)
summary(habitat2)
TukeyHSD(habitat2)

habitat2_wild <- aov(num_viruses~aq_hab,data=decapods_wild)
summary(habitat2_wild)
TukeyHSD(habitat2_wild)

habitat3 <- aov(path_index~aq_hab,data=decapods)
summary(habitat3)

habitat4 <- aov(virus_index~aq_hab,data=decapods)
summary(habitat4)

habitat4_wild <- aov(virus_index~aq_hab,data=decapods_wild)
summary(habitat4_wild)

