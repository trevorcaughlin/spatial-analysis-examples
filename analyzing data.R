
library("ape")
library("raster")

#read in data
read.csv(file="biomass_dat.csv")

#biomass of trees in a tropical landscape. check: do data make sense?
head(biomass_dat)


#explore the data
hist(biomass_dat$biomass,breaks=20,col="green")


plot(biomass_dat$biomass~biomass_dat$logging_rate,pch=19,col="red")


plot(biomass_dat$biomass~biomass_dat$Annualrainfall,pch=19,col="blue")

#make a plot indexed by color

bio_perc=biomass_dat$biomass/max(biomass_dat$biomass)

rain_perc=biomass_dat$Annualrainfall/max(biomass_dat$Annualrainfall)

plot(biomass_dat$x,biomass_dat$y,col=rgb(0,bio_perc,0),pch=19,cex=4)

plot(biomass_dat$x,biomass_dat$y,col=rgb(0,0,rain_perc),pch=19,cex=4)

#QUESTION: ARE THESE DATA SPATIALLY AUTOCORRELATED?
#QUESTION: ARE THESE DATA SPATIALLY AUTOCORRELATED?

#calculate distance matrix
bio.dists <- as.matrix(dist(cbind(biomass_dat$x, biomass_dat$y)))

#set inverse: we want neighbors that are closer together to be more similar
bio.dists.inv <- 1/bio.dists
diag(bio.dists.inv) <- 0

#examine first rows of this
bio.dists.inv[1:5, 1:5]

#do Moran's I test
Moran.I(biomass_dat$biomass, bio.dists.inv)


#How do we solve this?
#linear regression
biomodel=lm(biomass~Annualrainfall,data=biomass_dat)
summary(biomodel)

bio_resids=residuals(biomodel)

Moran.I(bio_resids, bio.dists.inv)

#is there evidence for spatial autocorrelation after accounting for rainfall?

