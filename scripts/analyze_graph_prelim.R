#Preliminary Plotting

library(ggplot2)
library(dplyr)
library(cowplot)


decapods <- read.csv("analyze_data/hosts_clean_pathcounts.csv")
pathogens <- read.csv("clean_data/path_hosts_clean.csv")

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

levels(pathogen$pathogen_type)

####################################
#PLOTS


#1-
#Graph total number of pathogens per taxa
#create vector with host_type levels and number of species in each
xlab_path <- paste(levels(decapods$host_type),"\n(n=",table(decapods$host_type),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_pathogens))+
  geom_boxplot()+
  labs(x="Taxa",y="Number of Pathogens",title="Decapod Crustacean Pathogens")+
  scale_x_discrete(labels=xlab_path)

#2-
#Graph # viruses per host type
#create vector with host_type levels and number of each host_type that has viruses
#however, it's plotting all, including those spp with ZERO viruses
xlab_vir <- paste(levels(decapods$host_type),"\n(n=",
                  table(decapods$host_type[decapods$num_viruses>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_viruses))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Viruses",title="Decapod Crustacean Viruses")+
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

#4-
#rhizocephalan barnacles per taxa
#however plotting those with 0
xlab_rhiz <- paste(levels(decapods$host_type),"\n(N=",
                   table(decapods$host_type[decapods$num_rhiz>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_rhiz))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Rhizocephalan Barnacles",title="Decapod Crustacean Rhizocephalan Barnacles")+
  scale_x_discrete(labels=xlab_rhiz)

#5-
#Graph # bacteria per taxa
xlab_bacteria <- paste(levels(decapods$host_type),"\n(N=",
                   table(decapods$host_type[decapods$num_bacteria>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_bacteria))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Bacteria Species",title="Decapod Crustacean Bacterial Parasites")+
  scale_x_discrete(labels=xlab_bacteria)

#6-
#Graph # microsporidians per taxa
xlab_microsp <- paste(levels(decapods$host_type),"\n(N=",
                       table(decapods$host_type[decapods$num_microsp>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_microsp))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Microsporidians",title="Decapod Crustacean Microsporidian Parasites")+
  scale_x_discrete(labels=xlab_microsp)

#7-
#scatterplot of num_pathogens vs longev
ggplot(data=decapods, aes(x=longev_max,y=num_pathogens))+
  geom_point()

summary(lm(longev_max~num_pathogens,data=decapods_na.rm)) #not sure where these datasets are created

#8-
#scatterplot of num_viruses vs longev
bem_colors <- c("#D1800B","#694008","#1D6295","#84ACB6")
long_vir_plot <- ggplot(data=decapods, aes(x=longev_max,y=num_viruses,fill=host_type))+
  geom_point(pch=21,size=4,col="black",alpha=.85)+
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
  labs(x="Taxa",y="Longevity")
  
#12-
#Graph number pathogens by sociality
as.factor(decapods$social_score)
#need to add what the scores mean
xlab_soc_path <- paste(levels(decapods$social_score),
                       "\n(n=",table(decapods$social_score),")",sep="")
ggplot(data=decapods, aes(x=as.factor(social_score),y=num_pathogens))+
  geom_boxplot()+
  labs(x="Social Score",y="Number of Pathogens",title="Decapod Crustacean Pathogens and Sociality")+
  scale_x_discrete(labels=xlab_soc_path)
#extrapolated by family
xlab_soc_fam_path <- paste(levels(decapods$social_score),
                           "\n(n=",table(decapods$fam_soc_score),")",sep="")
ggplot(data=decapods, aes(x=as.factor(fam_soc_score),y=num_pathogens))+
  geom_boxplot()+
  labs(x="Social Score",y="Number of Pathogens",title="Decapod Crustacean Pathogens and Sociality")+
  scale_x_discrete(labels=xlab_soc_path)

#13-
#Graph number of viruses by sociality
xlab_soc_vir <- paste(levels(decapods$social_score),"\n(n=",
                  table(decapods$social_score[decapods$num_viruses>0]),")",sep="")
ggplot(data=decapods, aes(x=as.factor(social_score),y=num_viruses))+
  geom_boxplot()+
  labs(x="Social Score",y="Number of Viruses",title="Decapod Crustacean Viruses and Sociality")+
  scale_x_discrete(labels=xlab_soc_vir)


