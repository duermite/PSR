#Preliminary Plotting
######################
#162 SPECIES
######################

#####################
#SWEEP ENVIRONMENT
#####################

library(ggplot2)
library(dplyr)
library(cowplot)


decapods <- read.csv("analyze_data/hosts_clean_pathcounts162.csv")
path_hosts <- read.csv("clean_data/path_hosts_clean.csv")
pathogens <- read.csv("clean_data/pathogen_clean.csv")

#Tasks
#1-Graph total number of pathogens per taxa
#2-Graph # viruses per host type
#3-Graph #isopods per taxa
#4-Graph rhizocephalan barnacles per taxa
#5-Graph # bacteria per taxa
#6-Graph # microsporidians per taxa
#7-Scatterplot number pathogens vs longevity
#8-scatterplot of num_viruses vs longev
#9-#boxplot of aq.hab vs num_pathogens
##*need to make simplified habitat column and continue
#10-boxplot of aq.hab vs num_viruses
#11-graph longevity by taxa
#12-Graph number pathogens by sociality
##*need to add what the scores mean
#13-Graph number of viruses by sociality
#14-Graph tranmission by taxa
#15-Graph requirements by taxa
#16-Graph size by taxa

levels(pathogens$pathogen_type)
#number of different types of pathogens
path_type_count <- pathogens %>% 
  group_by(pathogen_type) %>% 
  count(pathogen_type) %>% 
  rename(n_paths=n)

####################################
#PLOTS


#1-
#Graph total number of pathogens per taxa
#create vector with host_type levels and number of species in each
xlab_path <- paste(levels(decapods$host_type),"\n(n=",
                   table(decapods$host_type),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_pathogens))+
  geom_boxplot()+
  labs(x="Taxon",y="Number of Pathogens",title="Decapod Crustacean Pathogens")+
  scale_x_discrete(labels=xlab_path)
#these numbers aren't right

#1.5 Pathogens per taxon per citation (pathogen index)
ggplot(data=decapods, aes(x=host_type,y=num_pathogens/num_results))+
  geom_boxplot()+
  labs(x="Taxon",y="Pathogen Index",title="Decapod Crustacean Pathogens")+
  scale_x_discrete(labels=xlab_path)

#1.5.5 Try different index, divide by # pathogen citations instead
ggplot(data=decapods2, aes(x=host_type,y=num_pathogens/path_search_results))+
  geom_boxplot()+
  labs(x="Taxon",y="Pathogen Index 2.0",title="Decapod Crustacean Pathogens")+
  scale_x_discrete(labels=xlab_path)

############
#Path groups by taxon
############

#2-
#Graph # viruses per host type
#create vector with host_type levels and number of each host_type that has viruses
#however, it's plotting all, including those spp with ZERO viruses
xlab_vir <- paste(levels(decapods$host_type),"\n(n=",
                  table(decapods$host_type[decapods$num_viruses>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_viruses))+
  geom_boxplot()+
  labs(x="Taxon",y="Number of Viruses",title="Decapod Crustacean Viruses")+
  scale_x_discrete(labels=xlab_vir)

#2.5 virus index per host type
ggplot(data=decapods, aes(x=host_type,y=(num_viruses/path_search_results)))+
  geom_boxplot()+
  labs(x="Taxon",y="Virus Index",title="Decapod Crustacean Viruses")+
  scale_x_discrete(labels=xlab_vir)

#3-
#graph isopods per taxa
#however plotting those with 0
xlab_isopod <- paste(levels(decapods$host_type),"\n(n=",
                     table(decapods$host_type[decapods$num_isopods>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_isopods))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Isopods",title="Decapod Crustacean Bopyrid Isopods")+
  scale_x_discrete(labels=xlab_isopod)
#isopod index
ggplot(data=decapods, aes(x=host_type,y=(num_isopods/path_search_results)))+
  geom_boxplot()+
  labs(x="Host Type",y="Isopod Index",title="Decapod Crustacean Bopyrid Isopods")+
  scale_x_discrete(labels=xlab_isopod)

#4-
#rhizocephalan barnacles per taxa
#however plotting those with 0
xlab_rhiz <- paste(levels(decapods$host_type),"\n(N=",
                   table(decapods$host_type[decapods$num_rhiz>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_rhiz))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Barnacles",title="Decapod Crustacean Rhizocephalan Barnacles")+
  scale_x_discrete(labels=xlab_rhiz)
#barnacle index
ggplot(data=decapods, aes(x=host_type,y=(num_rhiz/path_search_results)))+
  geom_boxplot()+
  labs(x="Host Type",y="Barnacle Index",title="Decapod Crustacean Rhizocephalan Barnacles")+
  scale_x_discrete(labels=xlab_rhiz)

#5-
#Graph # bacteria per taxa
xlab_bacteria <- paste(levels(decapods$host_type),"\n(N=",
                   table(decapods$host_type[decapods$num_bacteria>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_bacteria))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Bacteria Species",title="Decapod Crustacean Bacterial Parasites")+
  scale_x_discrete(labels=xlab_bacteria)
#bacteria index
ggplot(data=decapods, aes(x=host_type,y=(num_bacteria/path_search_results)))+
  geom_boxplot()+
  labs(x="Host Type",y="Bacteria Index",title="Decapod Crustacean Bacterial Parasites")+
  scale_x_discrete(labels=xlab_bacteria)
#5.1
#Graph # intracellular bacteria
xlab_bact_intra <- paste(levels(decapods$host_type),"\n(N=",
                         table(decapods$host_type[decapods$num_bacteria_intra>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_bacteria_intra))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Intracellular Bacteria Species",
       title="Decapod Crustacean Intracellular Bacterial Parasites")+
  scale_x_discrete(labels=xlab_bact_intra)
#intracellular bacteria index
ggplot(data=decapods, aes(x=host_type,y=(num_bacteria_intra/path_search_results)))+
  geom_boxplot()+
  labs(x="Host Type",y="Intracellular Bacteria Index",
       title="Decapod Crustacean Intracellular Bacterial Parasites")+
  scale_x_discrete(labels=xlab_bact_intra)
#5.2
#Graph # extracellular bacteria
xlab_bact_extra <- paste(levels(decapods$host_type),"\n(N=",
                         table(decapods$host_type[decapods$num_bacteria_extra>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_bacteria_extra))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Extracellular Bacteria Species",
       title="Decapod Crustacean Extracellular Bacterial Parasites")+
  scale_x_discrete(labels=xlab_bact_extra)
#intracellular  index
ggplot(data=decapods, aes(x=host_type,y=(num_bacteria_extra/path_search_results)))+
  geom_boxplot()+
  labs(x="Host Type",y="Intracellular Bacteria Index",
       title="Decapod Crustacean Extracellular Bacterial Parasites")+
  scale_x_discrete(labels=xlab_bact_extra)
#6-
#Graph # microsporidians per taxa
xlab_microsp <- paste(levels(decapods$host_type),"\n(N=",
                       table(decapods$host_type[decapods$num_microsp>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_microsp))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Microsporidians",title="Decapod Crustacean Microsporidian Parasites")+
  scale_x_discrete(labels=xlab_microsp)
#microsporidian index
ggplot(data=decapods, aes(x=host_type,y=(num_microsp/path_search_results)))+
  geom_boxplot()+
  labs(x="Host Type",y="Microsporidian Index",title="Decapod Crustacean Microsporidian Parasites")+
  scale_x_discrete(labels=xlab_microsp)

#Graph # apicomplexans
xlab_api <- paste(levels(decapods$host_type),"\n(N=",
                      table(decapods$host_type[decapods$num_api>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_api))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Apicomplexans",title="Decapod Crustacean Apicomplexan Parasites")+
  scale_x_discrete(labels=xlab_api)
#apicomplexan index
ggplot(data=decapods, aes(x=host_type,y=num_api/path_search_results))+
  geom_boxplot()+
  labs(x="Host Type",y="Apicomplexan Index",title="Decapod Crustacean Apicomplexan Parasites")+
  scale_x_discrete(labels=xlab_api)

#graph # cestodes
xlab_cestode <- paste(levels(decapods$host_type),"\n(N=",
                  table(decapods$host_type[decapods$num_cestode>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_cestode))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Cestodes",title="Decapod Crustacean Cestode Parasites")+
  scale_x_discrete(labels=xlab_cestode)
#apicomplexan index
ggplot(data=decapods, aes(x=host_type,y=num_cestode/path_search_results))+
  geom_boxplot()+
  labs(x="Host Type",y="Cestode Index",title="Decapod Crustacean Cestode Parasites")+
  scale_x_discrete(labels=xlab_cestode)

#graph # fungi
xlab_fungi <- paste(levels(decapods$host_type),"\n(N=",
                      table(decapods$host_type[decapods$num_fungi>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_fungi))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Fungi",title="Decapod Crustacean Fungi Parasites")+
  scale_x_discrete(labels=xlab_fungi)
#apicomplexan index
ggplot(data=decapods, aes(x=host_type,y=num_fungi/path_search_results))+
  geom_boxplot()+
  labs(x="Host Type",y="Fungi Index",title="Decapod Crustacean Fungi Parasites")+
  scale_x_discrete(labels=xlab_fungi)

#graph # nematodes
xlab_nematode <- paste(levels(decapods$host_type),"\n(N=",
                    table(decapods$host_type[decapods$num_nematode>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_nematode))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Nematode",title="Decapod Crustacean Nematode Parasites")+
  scale_x_discrete(labels=xlab_nematode)
#apicomplexan index
ggplot(data=decapods, aes(x=host_type,y=num_nematode/path_search_results))+
  geom_boxplot()+
  labs(x="Host Type",y="Nematode Index",title="Decapod Crustacean Nematode Parasites")+
  scale_x_discrete(labels=xlab_nematode)

#graph # trematodes
xlab_trematode <- paste(levels(decapods$host_type),"\n(N=",
                       table(decapods$host_type[decapods$num_trematode>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_trematode))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of trematode",title="Decapod Crustacean trematode Parasites")+
  scale_x_discrete(labels=xlab_trematode)
#apicomplexan index
ggplot(data=decapods, aes(x=host_type,y=num_trematode/path_search_results))+
  geom_boxplot()+
  labs(x="Host Type",y="trematode Index",title="Decapod Crustacean trematode Parasites")+
  scale_x_discrete(labels=xlab_trematode)


#7-
#scatterplot of num_pathogens vs longev
ggplot(data=decapods, aes(x=longev_max,y=num_pathogens,fill=host_type))+
  geom_point(pch=21,size=2,col="black",alpha=.85)+
  labs(x="Longevity (years)", y="Number of Pathogens")+
  scale_fill_manual(values=bem_colors,name="Host Type")



ggplot(data=temp, aes(x=longev_max,y=num_pathogens,fill=host_type))+
  geom_point(pch=21,size=2,col="black",alpha=.85)+
  labs(x="Longevity (years)", y="Number of Pathogens")+
  scale_fill_manual(values=bem_colors,name="Host Type")

summary(lm(longev_max~num_pathogens,data=decapods_na.rm)) #not sure where these datasets are created

#7.5 Longev and pathogen index
ggplot(data=decapods, aes(x=longev_max,y=path_index,fill=host_type))+
  geom_point(pch=21,size=1.5,col="black",alpha=.85)+
  labs(x="Longevity (years)", y="Pathogen Index")+
  scale_fill_manual(values=bem_colors,name="Host Type")

#8-
#scatterplot of num_viruses vs longev
bem_colors <- c("#D1800B","#694008","#1D6295","#84ACB6")
long_vir_plot <- ggplot(data=decapods, aes(x=longev_max,y=num_viruses,fill=host_type))+
  geom_point(pch=21,size=2,col="black",alpha=.85)+
  labs(x="Longevity (years)", y="Number of Viruses")+
  scale_fill_manual(values=bem_colors,name="Host Type")+
  scale_x_continuous(limit=c(0,70))+
  theme(legend.position=c(.55,.75),legend.title.align=0.5)
long_vir_plot
#Save a plot to main folder
ggsave("outputs/bem2018_virus_longev_plot.png",plot=long_vir_plot,
       width=6,
       height=6.7,
       unit=c("in"),
       scale=1)
#8.5 Longevity and virus index
ggplot(data=decapods, aes(x=longev_max,y=virus_index,fill=host_type))+
  geom_point(pch=21,size=1.5,col="black",alpha=.85)+
  labs(x="Longevity (years)", y="Virus Index")+
  scale_fill_manual(values=bem_colors,name="Host Type")


#9-
#boxplot of aq.hab vs num_pathogens
levels(decapods$aq_hab)
lab_hab <- c("brackish",
             "fresh",
             "brackish &\nfresh",
             "marine & \nbrackish",
             "marine &\nbracksih \n& fresh",
             "terrestrial \n& marine", 
             "marine")
ggplot(data=decapods, aes(x=aq_hab,y=num_pathogens))+
  geom_boxplot()+
  labs(x="Habitat",y="Number of Pathogens",title="Decapod Crustacean Pathogens")+
  scale_x_discrete(labels=lab_hab)
#create simplified habitat

#graph simplified habitat
ggplot(data=decapods, aes(x=hab_simple,y=num_pathogens))+
  geom_boxplot()+
  labs(x="Habitat",y="Number of Pathogens",title="Decapod Crustacean Pathogens")

#10-
#boxplot of aq.hab vs num_viruses
ggplot(data=decapods, aes(x=aq_hab,y=num_viruses))+
  geom_boxplot()+
  labs(x="Habitat",y="Number of Viruses",title="Decapod Crustacean Pathogens")+
  scale_x_discrete(labels=lab_hab)

#11-
#Graph longevity by taxa
xlab_long <- paste(levels(decapods$host_type),"\n(n=",
                   table(decapods$host_type[decapods$longev_max!="NA"]),")",sep="")
ggplot(data=decapods,aes(x=host_type,y=longev_max))+
  geom_boxplot()+
  scale_x_discrete(labels=xlab_long)+
  labs(x="Taxon",y="Longevity")
  
#12-
#Graph number pathogens by sociality
as.factor(decapods$social_score)
xlab_soc_path <- paste(levels(decapods$social_score),
                       "\n(n=",table(decapods$social_score[decapods$num_pathogens>0]),")",sep="")
ggplot(data=decapods, aes(x=as.factor(social_score),y=num_pathogens))+
  geom_boxplot()+
  labs(x="Social Score",y="Number of Pathogens",title="Decapod Crustacean Pathogens and Sociality")+
  scale_x_discrete(labels=xlab_soc_path)
#extrapolated by family
xlab_soc_fam_path <- paste(levels(decapods$social_score),
                           "\n(n=",table(decapods$fam_soc_score[decapods$num_pathogens>0]),")",sep="")
ggplot(data=decapods, aes(x=as.factor(fam_soc_score),y=num_pathogens))+
  geom_boxplot()+
  labs(x="Sociality",y="Number of Pathogens",title="Decapod Crustacean Pathogens and Sociality")+
  scale_x_discrete(labels=xlab_soc_path)
#with path index instead of count
ggplot(data=decapods, aes(x=as.factor(fam_soc_score),y=path_index))+
  geom_boxplot()+
  labs(x="Sociality",y="Number of Pathogens",title="Decapod Crustacean Pathogens and Sociality")+
  scale_x_discrete(labels=xlab_soc_path)

#13-
#Graph number of viruses by sociality
#extrapolated by family
xlab_soc_vir <- paste(levels(decapods$social_score),"\n(n=",
                  table(decapods$fam_soc_score[decapods$num_viruses>0]),")",sep="")
ggplot(data=decapods, aes(x=fam_soc_score,y=num_viruses))+
  geom_boxplot()+
  labs(x="Sociality",y="Number of Viruses",title="Decapod Crustacean Viruses and Sociality")+
  scale_x_discrete(labels=xlab_soc_vir)

#14-Graph tranmission by taxa
xlab_trans <- paste(levels(decapods$host_type),
                    "\n(n=",table(decapods$host_type[decapods$num_pathogens>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=prop_direct))+
  geom_boxplot()+
  labs(x="Taxon",y="Proportion with Direct Transmission",title="Decapod Crustacean Parasite Tranmission Type")+
  scale_x_discrete(labels=xlab_trans)

#15-Graph requirements by taxa
xlab_req <- paste(levels(decapods$host_type),
                  "\n(n=",table(decapods$host_type[decapods$num_pathogens>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=prop_obligate))+
  geom_boxplot()+
  labs(x="Taxon",y="Proportion Obligate Parasites",title="Decapod Crustacean Parasite Tranmission Type")+
  scale_x_discrete(labels=xlab_req)

#16-Graph size by taxa
xlab_pathsize <- paste(levels(decapods$host_type),"\n(n=",
                       table(decapods$host_type[decapods$num_pathogens>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=prop_macro))+
  geom_boxplot()+
  labs(x="Taxon",y="Proportion Macro Parasites",title="Decapod Crustacean Parasite Tranmission Type")+
  scale_x_discrete(labels=xlab_pathsize)


