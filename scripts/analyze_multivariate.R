#Multivariate Analysis

library(vegan)
library(dplyr)

host_char <- read.csv("analyze_data/host_char_mult.csv")
host_path <- read.csv("analyze_data/host_path_mult.csv")
host_char_cat <- read.csv("analyze_data/host_char_mult_cat.csv")

#temporarily remove max_size_mm
host_char <- host_char %>% 
  select(-max_size_mm)
host_char_cat <- host_char_cat %>% 
  select(-max_size_mm)

#add num_other to host_path
num_all <- rowSums(host_path[,c(2:13)]) - rowSums(host_path[,c(6,7)])
host_path <- cbind(host_path,num_all) %>% 
  rowwise() %>% 
  mutate(other=pathogens-num_all) %>% 
  select(-num_all)

#separate datasets into useful components
#Host characteristics with all dummy vars
colnames(host_char)
host_type_char <- host_char %>% 
  select(1:8,31:33)

host_fam_char <- host_char %>% 
  select(1,3:4,9:33)

#Host char with host type and family as categorical
colnames(host_char_cat)
host_type_char2 <- host_char_cat %>% 
  select(2:8)

host_fam_char2 <- host_char_cat %>% 
  select(1,3:8)

#Host pathogen matrix
colnames(host_path)
host_path_biggrp <- host_path %>% 
  select(1,14:19)

host_path_smgrp <- host_path %>% 
  select(2:4,6:13,20)

##################
#look at depencies among variables
plot(host_path,gap=0,panel=panel.smooth)
plot(host_char,gap=0,panel=panel.smooth)
plot(host_char[1:9],gap=0,panel=panel.smooth)

#playing
a <- cca(host_path)
a
aa <- cca(host_path,host_char)
aa
plot(a)
ordispider(aa)
  
#######################
#Host char vars as dummy
#######################
########
#Host Type and Path Taxon
#Unconstrained
b <- cca(host_path_smgrp)
b
plot(b)
text(b,display="species",col="red")
#Constrained
bb <- cca(host_path_smgrp,host_type_char)
bb
plot(bb)
host_path_cols <- colnames(host_path_smgrp)
text(bb,display="species",col="red")

#########
#Host Fam and Path Taxon
#Constrained
cc <- cca(host_path_smgrp,host_fam_char)
cc
plot(cc)
text(cc,display="species",col="red")

###########
#Host type and path group
#Unconstrained
d <- cca(host_path_biggrp)
d
plot(d)
text(d,display="species",col="red")
#Constrained
dd <- cca(host_path_biggrp,host_type_char)
dd
plot(dd)
text(dd,display="species",col="red")

#######################
#Host char vars some categorical
#######################
#Host Type and Path Taxon
#Constrained
ee <- cca(host_path_smgrp,host_type_char2[2:7])
ee
plot(ee)
host_path_cols <- colnames(host_path_smgrp)
text(ee,display="species",col="black")
colvec <- c("red","green","purple","orange")
with(host_type_char2,points(ee,display="sites",col=colvec[host_type]),pch=21,
     bg=colvec[host_type])
with(host_type_char2,legend("topright",legend=levels(host_type),bty="n",
                            col=colvec,pch=21,pt.bg=colvec))
spenvcor(ee) #species-environment correlation, which is the corr btw LC (constraints) and WA (sp scores) for each axis
plot(procrustes(b,ee)) #inspect to see if constraint help? not sure
plot(b)
plot(envfit(b,host_type_char2[2:7]))
#Model building and choosing best model
ee1 <- cca(host_path_smgrp~.,host_type_char2[2:7]) #all env variables
ee0 <- cca(host_path_smgrp~1,host_type_char2[2:7]) #no env variables
ee1
ee0
ee_step <- step(ee0,scope=formula(ee1),test="perm")
ee_back <- step(ee1,test="perm") #simplifies the maximum model?
ee_ordi <- ordistep(ee0,scope=formula(ee1))
ee_ordi$anova #giving same variables each time, but not sure what I'm doing differently...
#variance inflation factor
##1 for completely indep variables and >10-20 for highly multicollinear
vif.cca(ee1)
vif.cca(ee_step)
vif.cca(ee_back)
#all are close to 1 and <2 so pretty independent
#significance testing
anova(ee_step) #whole model
anova(ee_step,by="term",permu=200) #by term
anova(ee_step,by="axis",perm=500) #test by axis, first 3 are significant


#Host Type and Path group
#Constrained
ff <- cca(host_path_biggrp,host_type_char2[2:7])
ff
plot(ff)
text(ff,display="species",col="black")
with(host_type_char2,points(ff,display="sites",col=colvec[host_type]),pch=21,
     bg=colvec[host_type])
with(host_type_char2,legend("topright",legend=levels(host_type),bty="n",
                            col=colvec,pch=21,pt.bg=colvec))
