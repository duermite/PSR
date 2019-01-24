#Preliminary Analyses

decapods <- read.csv("analyze_data/hosts_clean_pathcounts.csv")
pathogens <- read.csv("clean_data/path_hosts_clean.csv")

#Tasks
#1-Anova Host type and # pathogens
#2-Anova host type and # viruses
#3-linear model of longevity and number pathogens
#4-linear model of longevity and number viruses

###################################
#Anovas
#1-
#Anova Host type and # pathogens
htype_numpaths <- aov(num_pathogens~host_type,data=decapods)
summary(htype_numpaths) #not significant
plot(htype_numpaths) #doesn't look normal
#need to examine residuals
#transform data
htype_numpaths_trans <- aov(log(num_pathogens)~host_type,data=decapods)
summary(htype_numpaths_trans) #not significant
plot(htype_numpaths_trans) #I don't know if this is normal!

#2-
#Anova host type and # viruses
htype_numvirus <- aov(num_viruses~host_type,data=decapods)
summary(htype_numvirus) #barely significant!
TukeyHSD(htype_numvirus) #but no pairwise differences
plot(htype_numvirus) #maybe not normal?
#need to examine residuals

#####################
#Linear Models
#current linear models do NOT have significant slope
#this is ok because the relationship isn't linear, need another model type
#3-
#linear model of longevity and number pathogens
longev_path <- glm(num_pathogens~longev_max,data=decapods)
summary(longev_path)

#4-
#linear model of longevity and number viruses
longev_vir <- glm(num_viruses~longev_max,data=decapods)
summary(longev_vir)
