#ZERO INFLATED NEGATIVE BINOMIAL MODEL

library(ggplot2)
library(dplyr)
library(cowplot)
library(pscl)
library(boot)
library(lme4)
library(MASS)
library(lmerTest)

hosts <- read.csv("analyze_data/hosts_clean_pathcounts100_wild.csv")

#add (longev)^2 column
hosts <- hosts %>% 
  mutate(longev2=longev_max^2)

#examine the spread of the data
ggplot(data=hosts,aes(x=num_pathogens))+
  geom_histogram(binwidth=5)

ggplot(data=hosts,aes(num_pathogens,fill=host_type))+
  geom_histogram(binwidth=5)+
  facet_grid(~host_type)

#avg num pathogens by host_type
with(hosts, tapply(num_pathogens, host_type, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
#variance is higher than mean, suggesting overdispersion, suggestion negative binomial

#################
#Examine residuals on pathogen search results
#Attempt to normalize data with transformations
#################
#residual psr
lm_psr <- lm(num_pathogens~path_search_results,data=hosts)
summary(lm_psr)
plot(num_pathogens~path_search_results,data=hosts)
plot(resid(lm_psr))
eff_resid <- as.data.frame(resid(lm_psr))
hosts <- cbind(hosts,eff_resid)
histogram(resid(lm_psr))
shapiro.test(resid(lm_psr)) #not normal

#attempt using residuals
summary(lm(resid(lm_psr)~longev_max,data=hosts))

#with log transformation
lm_psr_log <- lm(log(num_pathogens)~path_search_results,data=hosts)
summary(lm_psr_log)
histogram(resid(lm_psr_log)) #more normal, but not fully there
shapiro.test(resid(lm_psr_log)) #not normal

#with sqrt transformation
lm_psr_sqrt <- lm(sqrt(num_pathogens)~path_search_results,data=hosts)
summary(lm_psr_sqrt)
histogram(resid(lm_psr_sqrt))
shapiro.test(resid(lm_psr_sqrt)) #not normal

#add polynomial to predictor
lm_psr_poly <- lm(num_pathogens~path_search_results+
                    (path_search_results)^2,data=hosts)
summary(lm_psr_poly)
histogram(resid(lm_psr_poly))
shapiro.test(resid(lm_psr_poly)) #not normal

#transform predictor
##log
lm_psr_pr_log <- lm(num_pathogens~log(path_search_results),data=hosts)
summary(lm_psr_pr_log)
histogram(resid(lm_psr_pr_log))
shapiro.test(resid(lm_psr_pr_log)) #not normal
##sqrt
lm_psr_pr_sqrt <- lm(num_pathogens~sqrt(path_search_results),data=hosts)
summary(lm_psr_pr_sqrt)
histogram(resid(lm_psr_pr_sqrt))
shapiro.test(resid(lm_psr_pr_sqrt)) #not normal, but best looking

#COULD NOT NORMALIZE

########################
#Attempt specialized glm
########################
#all pathogens
#can't do zero inflated for all pathogens because no zeros....
psr <- lm(log(num_pathogens)~host_type+longev_max+aq_hab+
                    production_type+fam_soc_score+invasive+path_search_results,
                  data=hosts)
summary(psr)
histogram(resid(psr))
shapiro.test(resid(psr)) #basically normal
stepAIC(psr)

#mixed effects model with citations as random variable
psr2 <- lmer(sqrt(num_pathogens)~longev_max+longev2+aq_hab+production_type+fam_soc_score+invasive+path_search_results+
              (0+path_search_results|host_type), 
          data=hosts)
summary(psr2)
histogram(resid(psr2))
shapiro.test(resid(psr2)) #nonnormal
step(psr2)
psr_final <- lmer(sqrt(num_pathogens)~longev_max+production_type+
                    fam_soc_score+invasive+(1|path_search_results),
                  data=hosts)
summary(psr_final)

psr2 <- lm(sqrt(num_pathogens)~longev_max+longev2+aq_hab+production_type+fam_soc_score+invasive+
               +path_search_results+host_type, 
             data=hosts)

#but what kind of lm to use? what family of glm? or is this ok?
#same but with negative binomial distribution
psr3 <- glmer.nb(num_pathogens~host_type+longev_max+aq_hab+
                   production_type+fam_soc_score+invasive+
                   (1|path_search_results), data=hosts)
#doesn't work, model failed to converge

#virus
virus <- lmer(sqrt(num_viruses)~host_type+longev_max+aq_hab+production_type+fam_soc_score+invasive+
               (1|path_search_results),
             data=hosts)
summary(virus)
histogram(resid(virus))
shapiro.test(resid(virus)) #normal!
step(virus)
virus_final <- lmer((num_viruses)~host_type+fam_soc_score+(1|path_search_results),
                  data=hosts)
summary(virus_final)
shapiro.test(resid(virus_final))#normal
#will need to rearrange to get all differences bcuz categorical
#No longev but...
plot(num_viruses~longev_max,data=hosts)
plot(num_pathogens~longev_max,data=hosts)

#bacteria
bact <- lmer((num_bacteria)~host_type+longev_max+aq_hab+production_type+fam_soc_score+invasive+
                (1|path_search_results),
              data=hosts)
summary(bact)
histogram(resid(bact))
shapiro.test(resid(bact)) #not normal
step(bact)
bact_final <- lmer((num_bacteria)~(longev_max)+invasive+(1|path_search_results),
                    data=hosts)
summary(bact_final)
shapiro.test(resid(bact_final)) #nonnormal, can't transform

#num_isopods
iso <- lmer((num_isopods)~host_type+longev_max+aq_hab+production_type+fam_soc_score+invasive+
               (1|path_search_results),
             data=hosts)
summary(iso)
histogram(resid(iso))
shapiro.test(resid(iso)) #nearly normal
step(iso) #doesn't work, categorical vars only ones sig?
iso_final <- lmer((num_pathogens)~host_type+aq_hab+
                    fam_soc_score+(1|path_search_results),
                  data=hosts)
#summary(iso_final)
#num_rhiz
rhiz <- lmer((num_rhiz)~host_type+longev_max+aq_hab+production_type+fam_soc_score+invasive+
               (1|path_search_results),
             data=hosts)
summary(rhiz)
histogram(resid(rhiz))
shapiro.test(resid(rhiz)) #nonnormal
step(rhiz)#doesn't work, categorical vars only ones sig?
#rhiz_final <- lmer((num_pathogens)~longev_max+production_type+
                    fam_soc_score+invasive+(1|path_search_results),
                  data=hosts)
#summary(rhiz_final)

#num_microsp
microsp <- lmer((num_microsp)~host_type+longev_max+aq_hab+production_type+fam_soc_score+invasive+
               (1|path_search_results),
             data=hosts)
summary(microsp)
histogram(resid(microsp))
shapiro.test(resid(microsp)) #nearly normal
step(microsp)
microsp_final <- lmer((num_microsp)~longev_max+production_type+
                        (1|path_search_results),
                  data=hosts)
summary(microsp_final)
shapiro.test(resid(microsp_final)) #almost sorta normal
histogram(resid(microsp_final)) #doesn't look terrible
#num_api
api <- lmer((num_api)~host_type+longev_max+aq_hab+production_type+fam_soc_score+invasive+
               (1|path_search_results),
             data=hosts)
summary(api)
histogram(resid(api))
shapiro.test(resid(api)) #nonnormal
step(api)
api_final <- lmer((num_api)~longev_max+production_type+
                    fam_soc_score+invasive+(1|path_search_results),
                  data=hosts)
summary(psr_final)
#num_cestode
cestode <- lmer((num_cestode)~host_type+longev_max+aq_hab+production_type+fam_soc_score+invasive+
               (1|path_search_results),
             data=hosts)
summary(cestode)
histogram(resid(cestode))
shapiro.test(resid(cestode)) #nonnormal
step(cestode)
cestode_final <- lmer((num_cestode)~(1|path_search_results),
                  data=hosts)
summary(cestode_final)
shapiro.test(resid(cestode_final)) #not normal
#num_fungi
fungi <- lmer((num_fungi)~host_type+longev_max+aq_hab+production_type+fam_soc_score+invasive+
               (1|path_search_results),
             data=hosts)
summary(fungi)
histogram(resid(fungi))
shapiro.test(resid(fungi)) #nonnormal
step(fungi)
fungi_final <- lmer((num_fungi)~fam_soc_score+
                      (1|path_search_results),
                  data=hosts)
summary(fungi_final)
shapiro.test(resid(fungi_final))
#num_nematode
nematode <- lmer((num_nematode)~host_type+longev_max+aq_hab+production_type+fam_soc_score+invasive+
               (1|path_search_results),
             data=hosts)
summary(nematode)
histogram(resid(nematode))
shapiro.test(resid(nematode)) #nonnormal
step(nematode)
nematode_final <- lmer((num_nematode)~(longev_max)+(1|path_search_results),
                  data=hosts)
summary(nematode_final)
shapiro.test(resid(nematode_final))
#num_trematode
trematode <- lmer((num_trematode)~host_type+longev_max+aq_hab+production_type+fam_soc_score+invasive+
               (1|path_search_results),
             data=hosts)
summary(trematode)
histogram(resid(trematode))
shapiro.test(resid(trematode)) #nonnormal
step(trematode)
trematode_final <- lmer((num_trematode)~(1|path_search_results),
                  data=hosts)
summary(trematode_final)
shapiro.test(resid(trematode_final))
