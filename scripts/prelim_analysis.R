#Preliminary Analyses and Plotting
#I have not looked at this recently, when I get back here, need to check everything

library(ggplot2)
library(dplyr)
library(cowplot)


decapods <- read.csv("clean_data/decapod_hosts_clean.csv")
pathogens <- read.csv("clean_data/pathogen_hosts_clean.csv")



levels(pathogen$pathogen_type)

####################################
#plots
#create vector with host_type levels and number of species in each
xlab_path <- paste(levels(decapods$host_type),"\n(n=",table(decapods$host_type),")",sep="")
#Total number of pathogens per taxa
ggplot(data=decapods, aes(x=host_type,y=num_pathogens))+
  geom_boxplot()+
  labs(x="Taxa",y="Number of Pathogens",title="Decapod Crustacean Pathogens")+
  scale_x_discrete(labels=xlab_path)

#create vector with host_type levels and number of each host_type that has viruses
#however, it's plotting all, including those spp with ZERO viruses
xlab_vir <- paste(levels(decapods$host_type),"\n(n=",
                  table(decapods$host_type[decapods$num_viruses>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_viruses))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Viruses",title="Decapod Crustacean Viruses")+
  scale_x_discrete(labels=xlab_vir)

#isopods per taxa
#however plotting those with 0
xlab_isopod <- paste(levels(decapods$host_type),"\n(n=",
                     table(decapods$host_type[decapods$num_isopods>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_isopods))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Isopods",title="Decapod Crustacean Bopyrid Isopods")+
  scale_x_discrete(labels=xlab_isopod)

#rhizocephalan barnacles per taxa
#however plotting those with 0
xlab_rhiz <- paste(levels(decapods$host_type),"\n(N=",
                   table(decapods$host_type[decapods$num_rhiz>0]),")",sep="")
ggplot(data=decapods, aes(x=host_type,y=num_rhiz))+
  geom_boxplot()+
  labs(x="Host Type",y="Number of Rhizocephalan Barnacles",title="Decapod Crustacean Rhizocephalan Barnacles")+
  scale_x_discrete(labels=xlab_rhiz)

#scatterplot of num_pathogens vs longev
ggplot(data=decapods, aes(x=longev_max,y=num_pathogens))+
  geom_point()

summary(lm(longev_max~num_pathogens,data=decapods_na.rm))

#scatterplot of num_viruses vs longev
bem_colors <- c("#D1800B","#694008","#1D6295","#84ACB6")
long_vir_plot <- ggplot(data=decapods, aes(x=longev_max,y=num_viruses,fill=host_type))+
  geom_point(pch=21,size=4,col="black",alpha=.85)+
  labs(x="Longevity (years)", y="Number of Viruses")+
  scale_fill_manual(values=bem_colors,name="Host Type")+
  scale_x_continuous(limit=c(0,70))+
  theme(legend.position=c(.55,.75),legend.title.align=0.5)
long_vir_plot
  
ggsave("bem_virus_longev_plot.png",plot=long_vir_plot,
       width=6,
       height=6.7,
       unit=c("in"),
       scale=1)


#boxplot of aq.hab vs num_pathogens
#need to clean up this data and the way it's entered
##########something messy here right now
levels(decapods$aq_hab)
lab_hab <- c("brackish","fresh","brackish &\nfresh","fresh &\nmarine","marine & \nbrackish", "marine")
ggplot(data=decapods, aes(x=aq_hab,y=num_pathogens))+
  geom_boxplot()+
  labs(x="Habitat",y="Number of Pathogens",title="Decapod Crustacean Pathogens")+
  scale_x_discrete(labels=lab_hab)

#graph simplified habitat
ggplot(data=decapods, aes(x=hab_simple,y=num_pathogens))+
  geom_boxplot()+
  labs(x="Habitat",y="Number of Pathogens",title="Decapod Crustacean Pathogens")


#boxplot of aq.hab vs num_viruses
ggplot(data=decapods, aes(x=aq_hab,y=num_viruses))+
  geom_boxplot()+
  labs(x="Habitat",y="Number of Viruses",title="Decapod Crustacean Pathogens")+
  scale_x_discrete(labels=lab_hab)

#longevity by taxa
xlab_long <- paste(levels(decapods$host_type),"\n(n=",
                   table(decapods$host_type[decapods$longev_max!="NA"]),")",sep="")
ggplot(data=decapods,aes(x=host_type,y=longev_max))+
  geom_boxplot()+
  scale_x_discrete(labels=xlab_long)+
  labs(x="Taxa",y="Longevity")
  

###################################
#anovas
htype_numpaths <- aov(num_pathogens~host_type,data=decapods_na.rm)
summary(htype_numpaths)
plot(htype_numpaths) #doesn't look normal
#need to examine residuals
TukeyHSD(htype_numpaths)
#transform data
htype_numpaths_trans <- aov(log(num_pathogens)~host_type,data=decapods_na.rm)
summary(htype_numpaths_trans) #barely significant
plot(htype_numpaths_trans) #I don't know if this is normal!
TukeyHSD(htype_numpaths_trans) #but no pairwise difference...huh?

htype_numvirus <- aov(num_viruses~host_type,data=decapods_na.rm)
summary(htype_numvirus)
TukeyHSD(htype_numvirus)
plot(htype_numvirus)
#need to examine residuals

longev_vir <- glm(num_viruses~longev_max,data=decapods)
summary(longev_vir)
