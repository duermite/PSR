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
library(reshape)


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
ggplot(data=decapods, aes(x=host_type,y=num_pathogens/path_search_results))+
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
#trematode index
ggplot(data=decapods, aes(x=host_type,y=num_trematode/path_search_results))+
  geom_boxplot()+
  labs(x="Host Type",y="trematode Index",title="Decapod Crustacean trematode Parasites")+
  scale_x_discrete(labels=xlab_trematode)

############
#Host Life History
############
#Longevity
############

#symbol codes:
bem_colors <- c("#D1800B","#694008","#1D6295","#84ACB6")
shapes <- c(0,1,17,3)
#emoji crab: 1f980
#emoji shrimp: 1f990
#don't think there's lobster or hermit crab =(
#emoji snail: 1f40c (for hermit crab??)

#7-
#scatterplot of num_pathogens vs longev
ggplot(data=decapods, aes(x=longev_max,y=num_pathogens,shape=host_type))+
  geom_point()+
  labs(x="Longevity (years)", y="Number of Pathogens")+
  scale_shape_manual(values=shapes,name="Taxon")+
  theme(legend.position=c(.55,.75),legend.title.align=0.5)+
  scale_x_continuous(limit=c(0,110))

#Longev and path number individual taxon graphs
ggplot(data=decapods, aes(x=longev_max,y=num_pathogens))+
  geom_point(shape=1)+
  labs(x="Longevity (years)", y="Number of Pathogens")+
  facet_wrap(~host_type)

#7.5 Longev and pathogen index
ggplot(data=decapods, aes(x=longev_max,y=num_pathogens/path_search_results,
                          shape=host_type))+
  geom_point()+
  labs(x="Longevity (years)", y="Pathogen Index")+
  scale_shape_manual(values=shapes,name="Host Type")+
  theme(legend.position=c(.55,.75),legend.title.align=0.5)

#Longev and path index individual taxon graphs
ggplot(data=decapods, aes(x=longev_max,y=num_pathogens/path_search_results))+
  geom_point(shape=1)+
  labs(x="Longevity (years)", y="Pathogen Index")+
  facet_wrap(~host_type)
#currently the index is adding a point way up high that is "Inf" because dividing by 0
##this should change with new search terms when I do that

#8-
#scatterplot of num_viruses vs longev
ggplot(data=decapods, aes(x=longev_max,y=num_viruses,shape=host_type))+
  geom_point()+
  labs(x="Longevity (years)", y="Number of Viruses")+
  scale_shape_manual(values=shapes,name="Host Type")+
  scale_x_continuous(limit=c(0,70))+
  theme(legend.position=c(.55,.75),legend.title.align=0.5)
#by taxon
ggplot(data=decapods, aes(x=longev_max,y=num_viruses))+
  geom_point(shape=1)+
  labs(x="Longevity (years)", y="Number of Viruses")+
  scale_x_continuous(limit=c(0,70))+
  facet_wrap(~host_type)
#How to Save a plot to main folder if I had a plot called long_vir_plot
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
#longevity and virus index by taxon
ggplot(data=decapods, aes(x=longev_max,y=num_viruses/path_search_results))+
  geom_point(shape=1)+
  labs(x="Longevity (years)", y="Viruses Index")+
  scale_x_continuous(limit=c(0,70))+
  facet_wrap(~host_type)

#11-
#Graph longevity by taxa (no pathogen info)
xlab_long <- paste(levels(decapods$host_type),"\n(n=",
                   table(decapods$host_type[decapods$longev_max!="NA"]),")",sep="")
ggplot(data=decapods,aes(x=host_type,y=longev_max))+
  geom_boxplot()+
  scale_x_discrete(labels=xlab_long)+
  labs(x="Taxon",y="Longevity")

################
#Habitat
################
#9-
#boxplot of aq.hab vs num_pathogens
levels(decapods$aq_hab)
lab_hab <- paste(c("Freshwater","Euryhaline","Marine"),"\n(n=",
                   table(decapods$aq_hab),")",sep="")
ggplot(data=decapods, aes(x=aq_hab,y=num_pathogens))+
  geom_boxplot()+
  labs(x="Habitat",y="Number of Pathogens",title="Decapod Crustacean Pathogens")+
  scale_x_discrete(labels=lab_hab,limits=c("fw","euryhaline","marine"))
#pathogen index
ggplot(data=decapods, aes(x=aq_hab,y=num_pathogens/path_search_results))+
  geom_boxplot()+
  labs(x="Habitat",y="Pathogen Index",title="Decapod Crustacean Pathogens")+
  scale_x_discrete(labels=lab_hab,limits=c("fw","euryhaline","marine"))

#10-
#boxplot of aq.hab vs num_viruses
ggplot(data=decapods, aes(x=aq_hab,y=num_viruses))+
  geom_boxplot()+
  labs(x="Habitat",y="Number of Viruses",title="Decapod Crustacean Pathogens")+
  scale_x_discrete(labels=lab_hab,limits=c("fw","euryhaline","marine"))
#virus index
ggplot(data=decapods, aes(x=aq_hab,y=num_viruses/path_search_results))+
  geom_boxplot()+
  labs(x="Habitat",y="Virus Index",title="Decapod Crustacean Pathogens")+
  scale_x_discrete(labels=lab_hab,limits=c("fw","euryhaline","marine"))

##################
#Sociality
##################
  
#12-
#Graph number pathogens by sociality
as.factor(decapods$social_score)
xlab_soc_path <- paste(levels(decapods$social_score),
                       "\n(n=",table(decapods$social_score[decapods$num_pathogens>0]),")",sep="")
ggplot(data=decapods, aes(x=as.factor(social_score),y=num_pathogens))+
  geom_boxplot()+
  labs(x="Social Score",y="Number of Pathogens",title="Decapod Crustacean Pathogens and Sociality")+
  scale_x_discrete(labels=xlab_soc_path)
#extrapolated by family (extrapolates for 32 unknowns)
xlab_soc_fam_path <- paste(levels(decapods$social_score),
                           "\n(n=",table(decapods$fam_soc_score[decapods$num_pathogens>0]),")",sep="")
ggplot(data=decapods, aes(x=as.factor(fam_soc_score),y=num_pathogens))+
  geom_boxplot()+
  labs(x="Sociality",y="Number of Pathogens",title="Decapod Crustacean Pathogens and Sociality")+
  scale_x_discrete(labels=xlab_soc_path)
#with path index instead of count
ggplot(data=decapods, aes(x=as.factor(fam_soc_score),y=num_pathogens/path_search_results))+
  geom_boxplot()+
  labs(x="Sociality",y="Pathogen Index",title="Decapod Crustacean Pathogens and Sociality")+
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
#virus index
ggplot(data=decapods, aes(x=fam_soc_score,y=num_viruses/path_search_results))+
  geom_boxplot()+
  labs(x="Sociality",y="Virus Index",title="Decapod Crustacean Viruses and Sociality")+
  scale_x_discrete(labels=xlab_soc_vir)

#####################
#Pathogen Characteristics
#####################

#14-Graph tranmission by taxa
xlab_trans <- paste(levels(decapods$host_type),
                    "\n(n=",table(decapods$host_type[decapods$num_pathogens>0]),")",sep="")
dec_trans <- decapods %>% 
  select(host_type, num_complex, num_direct) %>% 
  melt(id="host_type")

ggplot(data=dec_trans,aes(x=host_type,y=value,fill=variable))+
  geom_boxplot()+
  labs(x="Taxon",y="Number of Pathogens")
#index
dec_trans_index <- decapods %>% 
  select(host_type, num_complex, num_direct,path_search_results) %>% 
  mutate(complex=num_complex/path_search_results) %>% 
  mutate(direct=num_direct/path_search_results) %>% 
  select(host_type,complex,direct) %>% 
  melt(id="host_type")
ggplot(data=dec_trans_index,aes(x=host_type,y=value,fill=variable))+
  geom_boxplot()+
  labs(x="Taxon",y="Pathogen Index")
#proportion direct
ggplot(data=decapods, aes(x=host_type,y=prop_direct))+
  geom_boxplot()+
  labs(x="Taxon",y="Proportion with Direct Transmission",title="Decapod Crustacean Parasite Tranmission Type")+
  scale_x_discrete(labels=xlab_trans)


#15-Graph requirements by taxa
xlab_req <- paste(levels(decapods$host_type),
                  "\n(n=",table(decapods$host_type[decapods$num_pathogens>0]),")",sep="")
dec_req <- decapods %>% 
  select(host_type, num_obligate, num_opportunist) %>% 
  melt(id="host_type")
ggplot(data=dec_req,aes(x=host_type,y=value,fill=variable))+
  geom_boxplot()+
  labs(x="Taxon",y="Number of Pathogens")
#index
dec_req_index <- decapods %>% 
  select(host_type, num_obligate, num_opportunist, path_search_results) %>% 
  mutate(obligate=num_obligate/path_search_results) %>% 
  mutate(opportunist=num_opportunist/path_search_results) %>% 
  select(host_type,obligate,opportunist) %>% 
  melt(id="host_type")
ggplot(data=dec_req_index,aes(x=host_type,y=value,fill=variable))+
  geom_boxplot()+
  labs(x="Taxon",y="Pathogen Index")
#proportion obligate
ggplot(data=decapods, aes(x=host_type,y=prop_obligate))+
  geom_boxplot()+
  labs(x="Taxon",y="Proportion Obligate Parasites",title="Decapod Crustacean Parasite Tranmission Type")+
  scale_x_discrete(labels=xlab_req)

#16-Graph size by taxa
xlab_pathsize <- paste(levels(decapods$host_type),"\n(n=",
                       table(decapods$host_type[decapods$num_pathogens>0]),")",sep="")
dec_size <- decapods %>% 
  select(host_type, num_macro, num_micro) %>% 
  melt(id="host_type")
ggplot(data=dec_size,aes(x=host_type,y=value,fill=variable))+
  geom_boxplot()+
  labs(x="Taxon",y="Number of Pathogens")
#index
dec_size_index <- decapods %>% 
  select(host_type, num_macro, num_micro, path_search_results) %>% 
  mutate(macro=num_macro/path_search_results) %>% 
  mutate(micro=num_micro/path_search_results) %>% 
  select(host_type,macro,micro) %>% 
  melt(id="host_type")
ggplot(data=dec_size_index,aes(x=host_type,y=value,fill=variable))+
  geom_boxplot()+
  labs(x="Taxon",y="Pathogen Index")
#proportion macro
ggplot(data=decapods, aes(x=host_type,y=prop_macro))+
  geom_boxplot()+
  labs(x="Taxon",y="Proportion Macro Parasites",title="Decapod Crustacean Parasite Tranmission Type")+
  scale_x_discrete(labels=xlab_pathsize)

######################
#Host Summary Graphs
######################
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

#Host Type
ggplot(decapods,aes(x=factor(1),fill=host_type))+
  geom_bar(width=1)+
  coord_polar("y")+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

#geom_text(aes(y=host_type + c(0,cumsum(host_type)[length(host_type)]), label=length(host_type)))
#needs labels!!
#habitat
ggplot(decapods,aes(x=factor(1),fill=aq_hab))+
  geom_bar(width=1)+
  coord_polar("y")+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

#sociality
ggplot(decapods,aes(x=factor(1),fill=fam_soc_score))+
  geom_bar(width=1)+
  coord_polar("y")+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

#production type
ggplot(decapods,aes(x=factor(1),fill=production_type))+
  geom_bar(width=1)+
  coord_polar("y")+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

########################
#Parasite summary graphs
#######################
#pathogen type
ggplot(pathogens,aes(x=factor(1),fill=pathogen_type))+
  geom_bar(width=1)+
  coord_polar("y")+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

#virus type
virus <- pathogens %>% 
  filter(pathogen_type=="virus")
#54 viruses
ggplot(virus,aes(x=factor(1),fill=virus_type))+
  geom_bar(width=1)+
  coord_polar("y")+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())
#transmission
ggplot(pathogens,aes(x=factor(1),fill=transmission))+
  geom_bar(width=1)+
  coord_polar("y")+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())
#requirements
ggplot(pathogens,aes(x=factor(1),fill=requirement))+
  geom_bar(width=1)+
  coord_polar("y")+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())
#size
ggplot(pathogens,aes(x=factor(1),fill=size))+
  geom_bar(width=1)+
  coord_polar("y")+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())
