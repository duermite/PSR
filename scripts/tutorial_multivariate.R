#Multivariate stats

library(vegan)

data(varespec)
data(varechem)
str(varechem)
summary(varechem)
plot(varechem,gap=0,panel=panel.smooth)

m <- cca(varespec)
mm <- cca(varespec,varechem)
m
mm

cca(varespec, varechem[, c("Al", "P", "K")])
summary(m)

spenvcor(mm)
plot(mm)
plot(mm, display = c("lc","bp"))

plot(mm, dis=c("wa","lc"))
ordispider(mm)
plot(procrustes(m, mm))

#plot env variables to unconstrained ordination
plot(m)
plot(envfit(m, varechem))

#select specific variables as constraints
cca(varespec ~ Al + P + K, data=varechem)

