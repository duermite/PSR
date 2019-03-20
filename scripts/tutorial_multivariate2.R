library(vegan)

library(vegan)
library(MASS)
data(varespec)



#################
#NMDS
#################
vare.dis <- vegdist(varespec)
vare.mds0 <- isoMDS(vare.dis)

######my data
path.dis <- vegdist(host_path_smgrp)
path.mds0 <- isoMDS(path.dis) #doesn't work, zero or neg distances
path.mds0 <- metaMDS(path.dis) #doesn't converge
stressplot(path.mds0,path.dis) #ugh
#######

#Shepard plot
##ordination distances are plotted against community dissimilarities
##Fit as monotone step line
##R2=1-stress
##Null model: all ordination dist. are = and fit is flat horizontal line
stressplot(vare.mds0,vare.dis)


ordiplot(vare.mds0,type="t")

vare.mds <- metaMDS(varespec,trace=FALSE)
vare.mds
plot(vare.mds,type="t")

#discussion of different indices
data(varechem)
rankindex(scale(varechem), varespec, c("euc","man","bray","jac","kul"))
#standardize sites to equal sum of squares
dis <- vegdist(decostand(varespec, "norm"), "euclid")

#procrustes rotation
##uses uniform scaling and rotation to minimize the squared differences between two ordinations
tmp <- wisconsin(sqrt(varespec))
dis <- vegdist(tmp)
vare.mds0 <- isoMDS(dis,trace=0)
pro <- procrustes(vare.mds,vare.mds0)
pro
plot(pro)

##############
#Eigenvector Methods
##############

#Principle Components
vare.pca <- rda(varespec)
vare.pca
#inertia is variance
#"the solution decomposes the total variance into linear components"??
sum(apply(varespec,2,var)) #sum of linear components
#each eigenvalue "explains" a certain proportion of total variance
##divide PC by intertia
pc1 <- 983.0/1826
biplot(vare.pca,scaling=-1)
#standardizing all species to unit variance or using correlation coefficients instead of covariances will give a more balanced orgination
vare.pca <- rda(varespec,scale=TRUE)
vare.pca
biplot(vare.pca,scaling=3)

#################
#Conditioned or partial models
################
dune.cca <- cca(dune~Management+Condition(Moisture),dune.env)
plot(dune.cca)
dune.cca

################
#ANOSIM
##################

data(dune)
data(dune.env)
dune.dist <- vegdist(dune) #default is bray
attach(dune.env)
dune.ano <- anosim(dune.dist, Management)
summary(dune.ano)
plot(dune.ano)



