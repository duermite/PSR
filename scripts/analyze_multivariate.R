#Multivariate Analysis

library(vegan)
library(dplyr)

host_char <- read.csv("analyze_data/host_char_mult.csv")
host_path <- read.csv("analyze_data/host_path_mult.csv")
host_char_cat <- read.csv("analyze_data/host_char_mult_cat.csv")
host_cat <- read.csv("analyze_data/host_char_all_cat.csv")

#temporarily remove max_size_mm
host_char <- host_char %>% 
  dplyr::select(-max_size_mm)
host_char_cat <- host_char_cat %>% 
  dplyr::select(-max_size_mm)
host_cat <- host_cat %>% 
  dplyr::select(-max_size_mm) %>% 
  dplyr::rename(inv=invasive) %>% 
  dplyr::rename(sociality=fam_soc_score) %>% 
  dplyr::rename(capture=production_type) %>% 
  dplyr::rename(habitat=aq_hab)

###########
#Not using this section currently
#add num_other to host_path
#num_all <- rowSums(host_path[,c(2:13)]) - rowSums(host_path[,c(6,7)])
#host_path <- cbind(host_path,num_all) %>% 
  #rowwise() %>% 
  #mutate(other=pathogens-num_all) %>% 
  #select(-num_all)
###########

#separate datasets into useful components
#Host characteristics with all dummy vars
colnames(host_char)
host_type_char <- host_char %>% 
  dplyr::select(1:8,31:33)

host_fam_char <- host_char %>% 
  dplyr::select(1:4,9:33)

#Host char with host type and family as categorical
colnames(host_char_cat)
host_type_char2 <- host_char_cat %>% 
  dplyr::select(2:9)

host_fam_char2 <- host_char_cat %>% 
  dplyr::select(1,3:9)

#Host pathogen matrix
colnames(host_path)
host_path_biggrp <- host_path %>% 
  dplyr::select(1,25:30)

host_path_smgrp <- host_path %>% 
  dplyr::select(2:24)

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
ee <- cca(host_path_smgrp,host_type_char2[2:8])
ee
plot(ee,scaling=-1) #scaling means species scores divided by sd so abundant and scarce species will be approximately as far away from origin
host_path_cols <- colnames(host_path_smgrp)
text(ee,display="species",col="black",scaling=-1)
colvec <- c("red","green","purple","orange")
spenvcor(ee) #species-environment correlation, which is the corr btw LC (constraints) and WA (sp scores) for each axis
plot(procrustes(b,ee)) #inspect to see if constraint help? not sure
plot(b)
plot(envfit(b,host_type_char2[2:8]))
#Constrained and conditioned
ee2 <- cca(host_path_smgrp~longev_max+aquaculture+capture+inv+habitat+sociality+
             Condition(path_search_results),host_type_char2)
plot(ee2)
ee2
with(host_type_char2,points(ee2,display="sites",col=colvec[host_type]),pch=21,
     bg=colvec[host_type])
with(host_type_char2,legend("topright",legend=levels(host_type),bty="n",
                            col=colvec,pch=21,pt.bg=colvec))

anova(ee2,perm.max=2000)
#Model building and choosing best model
ee1 <- cca(host_path_smgrp~.,host_type_char2[2:8]) #all env variables
ee0 <- cca(host_path_smgrp~1,host_type_char2[2:8]) #no env variables
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

###################
#Cluster Analysis with small group
#http://cc.oulu.fi/~jarioksa/opetus/metodi/sessio3.pdf
###################
dist <- vegdist(host_path_smgrp)
head(dist)
par(mfrow=c(1,3))
#single linkage cluster
csin <- hclust(dist, method="single")
csin
plot(csin,hang=-1)
rect.hclust(csin, 4)
#complete linkage
ccom <- hclust(dist, method="complete")
plot(ccom, hang=-1)
rect.hclust(ccom, 4)
#average linkage
caver <- hclust(dist, method="aver")
plot(caver, hang=-1)
rect.hclust(caver, 4)
par(mfrow=c(1,1))
plot(caver, hang=-1)
rect.hclust(caver, 4)
#moving forward with average linkage
cl <- cutree(caver, 4)
cl_orig <- cutree(caver, 4)
cl
table(cl)
#confusion matrix, lots of mismatches between methods
table(cl, cutree(ccom, 3))
table(cl, cutree(csin, 3))

####################
#Ordination
###################
ord <- cmdscale(dist)
head(ord)
ordiplot(ord)
#different ways of plotting the groups (do ordiplot first)
ordihull(ord, cl, lty=3,col="red") #for distinct, non-overlapping groups
ordiplot(ord)
ordispider(ord, cl, col="blue", label=TRUE) #for overlapping groups
ordiplot(ord)
ordiellipse(ord, cl, col="red") #if we're not interesting in indiv. point but averages and centroids
ordiplot(ord, dis="si")
ordihull(ord, cutree(caver, 4))
ordiplot(ord, dis="si")
ordicluster(ord, caver,prune=2) #i don't know what this is doing
#dendrogram
den <- as.dendrogram(caver)
plot(den)
rect.hclust(caver, 4)
#rearrange so that ordering corresponds with first ordination axis
x <- scores(ord, display = "sites", choices = 1)
oden <- reorder(den, x)
par(mfrow=c(2,1))
plot(den)
plot(oden)
par(mfrow=c(1,1))
vegemite(host_path_smgrp, oden) #not working
tabasco(host_path_smgrp, caver) #not working
tabasco(host_path_smgrp, caver, Rowv = FALSE) #not working
tabasco(host_path_smgrp, oden, Rowv=FALSE)
#minimum spanning tree
mst <- spantree(dist)
ordiplot(ord, dis="si")
lines(mst, ord) #??
#cophenetic distance = dist. est. from dendrogram
plot(dist, cophenetic(csin), asp=1)
abline(0, 1) #overestimates
plot(dist, cophenetic(ccom), asp=1)
abline(0, 1) #underestimates
plot(dist, cophenetic(caver), asp=1)
abline(0, 1) #line in middle
#cophenetic correlation
cor(dist, cophenetic(csin))
cor(dist, cophenetic(ccom))
cor(dist, cophenetic(caver)) #highest corr = good
#average will always maximize the cophenetic correlations

###################
#Interpretation of Classes
###################
cl <- factor(cl)
#compare longev among classes
boxplot(as.numeric(host_char$longev_max)~cl,notch=FALSE)
anova(lm(as.numeric(host_char$longev_max) ~ cl)) #sig. diff.
#nonparametric:
clrda <- anova(rda(as.numeric(host_char$longev_max) ~ cl)) #sig. diff
clrda
TukeyHSD(clrda) #doesn't work
#Might need to add something to chisq tests because of 0s
#compare habitat
hab_tbl <- with(host_cat, table(cl, habitat))
hab_tbl
chisq.test((hab_tbl)) #p<<0.001
prop.table(hab_tbl,1)
prop.table(hab_tbl,2)
#compare invasion
inv_tbl <- with(host_cat, table(cl, inv))
inv_tbl
chisq.test(inv_tbl) #p=0.015
prop.table(inv_tbl,1)
prop.table(inv_tbl,2)
#compare sociality
soc_tbl <- with(host_cat, table(cl, sociality))
soc_tbl
chisq.test(soc_tbl) #p=0.0002
prop.table(soc_tbl,1)
prop.table(soc_tbl,2)
#compare host_type
host_tbl <- with(host_cat,table(cl,host_type))
host_tbl
chisq.test(host_tbl) #p<<0.001
prop.table(host_tbl,1)
prop.table(host_tbl,2)

#community summaries
library(labdsv)
#species occurrence frequencies:
const(host_path_smgrp, cl)
#mean abundance
importance(host_path_smgrp, cl)
#indicator species analysis:
mod <- indval(host_path_smgrp, as.numeric(cl))
names(mod)
#class to which species has the highest indicator value
mod$maxcls
#probability of finding higher indicator value in random permutations
#if this probability is low, the species is a significant indicator
mod$pval
summary(mod)
summary(mod, type="long")

#####################
#Optimizing Clustering at a Given Level
#####################
#Hellinger transformation
ckm <- kmeans(decostand(host_path_smgrp, "hell"), 4)
cl <- ckm$cluster
cl_opt <- ckm$cluster
ordiplot(ord, dis="si")
ordihull(ord, ckm$cluster, col="red")
#Optimum number of classes
ccas <- cascadeKM(decostand(host_path_smgrp, "hell"), 2, 15)
plot(ccas, sortq=TRUE)
#in above plot, the highest value of the criterion shows the optimal # of classes
#2 classes is optimal
#but compare to dune data:
data(dune)
ccas <- cascadeKM(decostand(dune, "hell"), 2, 15)
plot(ccas, sortq=TRUE)

#####################
#Add these clusters to the dataset
####################
host_char_cat <- host_char_cat %>% 
  cbind(cl_opt,cl_orig)
host_cat <- host_cat %>% 
  cbind(cl_opt,cl_orig)

#####################
#Fuzzy Clustering
####################
#each obs. has certain probability of belonging to a class
#in best case scenario is has prob of 
library(cluster)
cfuz <- fanny(dist,3,memb.exp=1.5)
names(cfuz)
summary(cfuz)
#plotting fuzzy clusters
ordiplot(ord, dis="si")
ordiplot(ord, dis="si", type="n")
stars(cfuz$membership, locatio=ord, draw.segm=TRUE, 
      add=TRUE, scale=FALSE, len=0.1)
ordihull(ord, cfuz$clustering, col="blue")
#the size of the sector shows the prob of the class membership
#in clear cases, one of the segments is dominant
#most appear equal membership btw two groups except one of the clusters

##################
#Discriminant Analysis
##################
#want to know the degree to which cluster membership is predictable from host variables
library(tree)
tree1 <- tree(factor(cl_orig)~longev_max+path_search_results+capture+inv+
                habitat+sociality+host_type,
              data=host_cat)
plot(tree1)
text(tree1) #values that are true go L, false go R
tree1
summary(tree1) #misclassification rate is high (0.333)
#confusion matrix
tree1.pred <- predict(tree1,newdata=host_char_cat,type="class")
tree1.pred
table(tree1.pred,cl_orig)

#now do the optimum clusters
tree2 <- tree(factor(cl_opt)~longev_max+path_search_results+capture+inv+
                habitat+sociality+host_type,
              data=host_char_cat)
plot(tree2)
text(tree2)
tree2
summary(tree2) #same amount of misclassification
tree2.pred <- predict(tree2,newdata=host_char_cat,type="class")
tree2.pred
table(tree2.pred,cl_opt)

library(MASS)
library(tidyverse)
library(klaR)

training_sample <- sample(c(TRUE,FALSE),nrow(host_cat),
                          replace=T, prob=c(0.6,0.4))
train <- host_cat[training_sample,]
test <- host_cat[!training_sample,]
########
#linear discriminant function
########
#Original Clusters
lda.orig <- lda(cl_orig~longev_max+path_search_results+capture+inv+habitat+sociality+
             host_type, data=train)
lda.orig
#Group means shows mean value for each of indep vars for each class
#Coeff of linear discriminants are coeff for each disc (indep var)
#proportions of trace = how much of variance each LD explains
#LD1 explains 59%
plot(lda.orig, col=as.integer(train$cl_orig))
#these do not clearly group together
plot(lda.orig,dimen=1,type="b") #doesn't work
#partition plot
partimat(cl_orig~longev_max+path_search_results+capture+inv+habitat+sociality+
           host_type, data=train, method="lda") #doesn't work

#LDA predictions
#verify model fits data
lda.train <- predict(lda.orig)
train$lda <- lda.train$class
lda_orig_tb <- table(train$lda,train$cl_orig)
lda_orig_tb
#total percent correct
sum(diag(prop.table(lda_orig_tb)))
#total # of correctly predicted obsertations is the sum of the diagonal
#45/61 = 73.7% correctly predicted 
lda.test <- predict(lda.orig,test)
test$lda <- lda.test$class
lda_orig_tb2 <- table(test$lda,test$cl_orig)
lda_orig_tb2
sum(diag(prop.table(lda_orig_tb2)))
#correct classifications of testing dataset = 15/35 = 42.9%
#not great

###
#Optimized Clusters
lda.opt <- lda(cl_opt~longev_max+path_search_results+capture+inv+habitat+sociality+
                  host_type, data=train)
lda.opt
#Group means shows mean value for each of indep vars for each class
#Coeff of linear discriminants are coeff for each disc (indep var)
#proportions of trace = how much of variance each LD explains
#LD1 explains 66.6%
plot(lda.opt, col=as.integer(train$cl_orig))
#these do not clearly group together
plot(lda.opt,dimen=1,type="b") #doesn't work
#partition plot
partimat(cl_opt~longev_max+path_search_results+capture+inv+habitat+sociality+
           host_type, data=train, method="lda") #doesn't work

#LDA predictions
#verify model fits data
lda.train <- predict(lda.opt)
train$lda <- lda.train$class
lda_opt_tb <- table(train$lda,train$cl_opt)
lda_opt_tb
#total percent correct
sum(diag(prop.table(lda_opt_tb)))
#total # of correctly predicted obsertations is the sum of the diagonal
#45/61 = 73.7% correctly predicted 
#exactly the same percentage as orig clusters
lda.test <- predict(lda.opt,test)
test$lda <- lda.test$class
lda_opt_tb2 <- table(test$lda,test$cl_opt)
lda_opt_tb2
sum(diag(prop.table(lda_opt_tb2)))
#correct classifications of testing dataset = 15/35 = 42.9%
#not great

#################
#ANOSIM & SIMPER
################
#host type
path_dist <- vegdist(host_path_smgrp)
path_ano <- anosim(path_dist,host_cat$host_type)
summary(path_ano)
plot(path_ano)

path_sim <- simper(host_path_smgrp,host_cat$host_type)
summary(path_sim)
#I still don't know which groups are significantly different

#habitat
hab_ano <- anosim(path_dist,host_cat$habitat)
summary(hab_ano)
#groups are similar R=0.01


#########################
#PERMANOVA
#########################
#Manova warmup
path_matrix <- as.matrix(host_path_smgrp)
host_man <- manova(path_matrix~host_cat$host_type)
summary(host_man)
summary.aov(host_man)
#how to plot manova results?

longev_man <- manova(path_matrix~host_cat$longev_max)
summary(longev_man)
summary.aov(longev_man)
#permanova
path_mat <- sqrt(path_matrix)
path_dist <- vegdist(path_mat,method="bray")
set.seed(36)
host_per <- adonis2(path_dist~host_cat$host_type,permutations=999,method="bray")
host_per
#this is significant, what next?
dispersion <- betadisper(path_dist,group=host_cat$host_type)
permutest(dispersion)
plot(dispersion,hull=FALSE,ellipse=TRUE)


#############
#Quadratic Discriminant Analysis
#############
#assumes indep vars normally distributed
#does not assume equal covariance bte classes
qda.orig <- qda(cl_orig~longev_max+path_search_results+capture+inv+habitat+sociality+
                  host_type, data=train)
#can't do it
#some group is too small for qda




ct <- table(cl,fit$class)
#proportion correctly assigned to each group
diag(prop.table(ct,1))
#total percent correct
sum(diag(prop.table(ct)))


