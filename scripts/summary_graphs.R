library(ggplot2)
library(dplyr)
library(cowplot)
library(reshape)
library(scales)
library(wesanderson)

################
#162
################
decapods <- read.csv("analyze_data/hosts_clean_pathcounts162.csv")
path_hosts <- read.csv("clean_data/path_hosts_clean.csv")
pathogens <- read.csv("clean_data/pathogen_clean.csv")

######################
#Host Summary Graphs
######################

###########
#Stacked bar charts
###########
ggplot(decapods,aes(x=host_type,fill=aq_hab))+
  geom_histogram(stat="count")+
  scale_fill_manual(values=wes_palette("Darjeeling2")[c(1,2,4)],
                    name="Salinity",
                    breaks=c("fw","euryhaline","marine"),
                    labels=c("Freshwater","Euryhaline","Marine"))+
  labs(x="Taxon",y="Number of Species")

ggplot(decapods,aes(x=host_type,fill=fam_soc_score))+
  geom_histogram(stat="count")+
  scale_fill_manual(values=wes_palette("Darjeeling2")[c(2,3,1,4)],
                    name="Sociality",
                    labels=c("Gregarious",
                             "Ontogenetic aggregations",
                             "Reproductive aggregations",
                             "Solitary"))+
  labs(x="Taxon",y="Number of Species")

ggplot(decapods,aes(x=host_type,fill=production_type))+
  geom_histogram(stat="count")+
  scale_fill_manual(values=wes_palette("Darjeeling2")[c(3,2,4,1)],
                    name="Fishery Production",
                    breaks=c("capture","aquaculture","both","neither"),
                    labels=c("Commercial capture","Aquaculture","Capture & Aquaculture","Neither"))+
  labs(x="Taxon",y="Number of Species")

ggplot(decapods,aes(x=host_type,fill=invasive))+
  geom_histogram(stat="count")+
  scale_fill_manual(values=wes_palette("Darjeeling2"),
                    name="Invasion History",
                    labels=c("Invasive","Not Invasive"))+
  labs(x="Taxon",y="Number of Species")
##########
#Pie charts
##########
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
host_value <- decapods %>% 
  group_by(host_type) %>% 
  count(host_type)

ggplot(host_value,aes(x="",y=n,fill=host_type))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())+
  geom_text(aes(label=paste0(n)),
            position=position_stack(vjust=.5))+
  scale_fill_manual(name="Taxon",
                    values=wes_palette("Darjeeling2"))

#to get percent:
#label=paste0(n," (",scales::percent(n/sum(n)),")"),

#habitat
habitat_value <- decapods %>% 
  group_by(aq_hab) %>% 
  count(aq_hab)
ggplot(habitat_value,aes(x="",y=n,fill=aq_hab))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())+
  geom_text(aes(label=paste0(n)),
            position=position_stack(vjust=.5))+
  scale_fill_manual(name="Salinity",
                    values=wes_palette("Darjeeling2")[c(1,2,4)],
                    breaks=c("fw","euryhaline","marine"),
                    labels=c("Freshwater","Euryhaline","Marine"))

#sociality
social_values <- decapods %>% 
  group_by(fam_soc_score) %>% 
  count(fam_soc_score)
ggplot(social_values,aes(x="",y=n,fill=fam_soc_score))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())+
  geom_text(aes(label=paste0(n)),
            position=position_stack(vjust=.5))+
  scale_fill_manual(name="Sociality",
                    values=wes_palette("Darjeeling2"),
                    labels=c("Gregarious",
                             "Ontogenetic aggregations",
                             "Reproductive aggregations",
                             "Solitary"))

#production type
production_values <- decapods %>% 
  group_by(production_type) %>% 
  count(production_type)
ggplot(production_values,aes(x="",y=n,fill=production_type))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())+
  geom_text(aes(label=paste0(n)),
            position=position_stack(vjust=.5))+
  scale_fill_manual(name="Fishery Production",
                    values=wes_palette("Darjeeling2"),
                    breaks=c("capture","aquaculture","both","neither"),
                    labels=c("Commercial capture","Aquaculture","Capture & Aquaculture","Neither"))
########################
#Parasite summary graphs
#######################
#pathogen type

pathogen_value <- pathogens %>% 
  group_by(pathogen_type) %>% 
  count(pathogen_type) 
write.csv(pathogen_value,"clean_data/pathogen_value.csv",
          row.names=FALSE)
#edit this in excel for now
pathogen_value <- read.csv("clean_data/pathogen_value.csv")

ggplot(pathogens,aes(x=pathogen_type))+
  geom_bar()

ggplot(pathogen_value,aes(x="",y=n,fill=pathogen_type))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())
#alternative pie chart
library(RColorBrewer)
coul=brewer.pal(4,"BrBG")
coul=colorRampPalette(coul)(25)
pie(x=pathogen_value$n,labels=pathogen_value$pathogen_type,col=coul,
    radius=1,cex=1,init.angle=145)
    
pie(rep(1,length(coul)),col=coul),main="",radius=1,cex=1)
   
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
trans_value <- pathogens %>% 
  group_by(transmission) %>% 
  count(transmission)
ggplot(trans_value,aes(x="",y=n,fill=transmission))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())+
  geom_text(aes(label=paste0(n)),
            position=position_stack(vjust=.5))+
  scale_fill_manual(name="Transmission",
                    values=wes_palette("Darjeeling2")[c(1,4,2)],
                    breaks=c("complex","direct","both"),
                    labels=c("Complex","Direct","Both"))
#requirements
req_values <- pathogens %>% 
  group_by(requirement) %>% 
  count(requirement)
ggplot(req_values,aes(x="",y=n,fill=requirement))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())+
  geom_text(aes(label=paste0(n)),
            position=position_stack(vjust=.5))+
  scale_fill_manual(name="Requirement",
                    values=wes_palette("Darjeeling2"),
                    breaks=c("opportunist","obligate"),
                    labels=c("Opportunist","Obligate"))
#size
size_values <- pathogens %>% 
  group_by(size) %>% 
  count(size)
  
ggplot(size_values,aes(x="",y=n,fill=size))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  blank_theme+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())+
  geom_text(aes(label=paste0(n)),
            position=position_stack(vjust=.5))+
  scale_fill_manual(name="Requirement",
                    values=wes_palette("Darjeeling2"),
                    breaks=c("macro","micro"),
                    labels=c("Macro","Micro"))

################
#Sunburst
###############
#not currently working
library(rPython)
library(ggsunburst)

decapods_soc <- decapods %>% 
  select(fam_soc_score,host_type) %>% 
  mutate(level=2)%>% 
  dplyr::rename(node=fam_soc_score,parent=host_type)
decapods_soc2 <- decapods_soc %>% 
  mutate(fam_soc_score="NA") %>% 
  mutate(level="root") 
decapods_soc3 <- decapods_soc %>% 
  rbind(decapods_soc2) %>% 
  dplyr::rename(node=fam_soc_score,parent=host_type)


write.csv(decapods_soc2,"analyze_data/sb_soc.csv")
sb_soc <- sunburst_data("analyze_data/sb_soc.csv",type="node_parent",sep=",")
sunburst(sb_soc)


