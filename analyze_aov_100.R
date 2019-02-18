library(dplyr)

decapods <- read.csv("analyze_data/hosts_clean_pathcounts100.csv")
path_hosts <- read.csv("clean_data/path_hosts_clean.csv")
pathogens <- read.csv("clean_data/pathogen_clean.csv")

path1 <- aov(num_pathogens~host_type,data=decapods)
summary(path1)

path2 <- aov((path_index)~host_type,
             data=decapods)
summary(path2)
#doesn't work bcuz of NaN NA and Inf...

vir1 <- aov((num_viruses)~host_type,
            data=decapods)
summary(vir1)
TukeyHSD(vir1)

vir2 <- aov((virus_index)~host_type,
            data=decapods)
summary(vir2)
TukeyHSD(vir2)

iso1 <- aov((num_isopods)~host_type,
            data=decapods)
summary(iso1)
TukeyHSD(iso1)

barnacles <- aov((num_rhiz)~host_type,
            data=decapods)
summary(barnacles)
TukeyHSD(barnacles)

bacteria <- aov((num_bacteria)~host_type,
                 data=decapods)
summary(bacteria)

microsp <- aov((num_microsp)~host_type,
                 data=decapods)
summary(microsp)

api <- aov((num_api)~host_type,
                 data=decapods)
summary(api)
TukeyHSD(api)

trematode <- aov((num_trematode)~host_type,
           data=decapods)
summary(trematode)

fungi <- aov((num_fungi)~host_type,
           data=decapods)
summary(fungi)

prop_obligate <- aov((prop_obligate)~host_type,
             data=decapods)
summary(prop_obligate)
TukeyHSD(prop_obligate)

prop_direct <- aov((prop_direct)~host_type,
                   data=decapods)
summary(prop_direct)

prop_macro <- aov((prop_macro)~host_type,
                   data=decapods)
summary(prop_macro)
TukeyHSD(prop_macro)

longev <- aov(longev_max~host_type,data=decapods)
summary(longev)
TukeyHSD(longev)

social <- aov(num_pathogens~fam_soc_score,data=decapods)
summary(social)

social2 <- aov(path_index~fam_soc_score,data=decapods)
summary(social2)

social3 <- aov(virus_index~fam_soc_score,data=decapods)
summary(social3)

social4 <- aov(num_viruses~fam_soc_score,data=decapods)
summary(social4)

habitat <- aov(num_pathogens~aq_hab,data=decapods)
summary(habitat)

habitat2 <- aov(num_viruses~aq_hab,data=decapods)
summary(habitat2)
TukeyHSD(habitat2)

habitat3 <- aov(path_index~aq_hab,data=decapods)
summary(habitat3)

habitat4 <- aov(virus_index~aq_hab,data=decapods)
summary(habitat4)
