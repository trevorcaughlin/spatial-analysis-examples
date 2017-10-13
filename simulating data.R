
directory<-strsplit(getwd(),"/")[[1]][3]
setwd(paste("C:/Users/",directory,"/Dropbox/R files",sep=""))
source("generalBAYESIANcode.r")


setwd(paste("C:/Users/",directory,"/Dropbox/R files/reforestation R code/land use cover change",sep=""))
#setwd(paste("C:/Users/",directory,"/Dropbox/R files/reforestation R code/land use cover change",sep=""))

library("maps")
library("raster")
library("rgeos")
library("rgdal")
library("sp")
library("ape")

rain=raster("RAINlandsatscale.tif")

rpts<-rasterToPoints(rain)

setwd(paste("C:/Users/",directory,"/Dropbox/R files/reforestation R code/land use cover change/landsat/landsat for 2012",sep=""))
LM<-raster("L_M3.tif")

rp=projectRaster(rain,crs="+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

rpts<-rasterToPoints(rp)


logging_rate<-runif(nrow(rpts))

biomass<-rnorm(n=nrow(rpts),mean=100+rpts[,3]*2.5+logging_rate*-1000,sd=5)

biomass_dat=data.frame(biomass,rpts,logging_rate)
colnames(biomass_dat)[4]="Annualrainfall"


plot(biomass~Annualrainfall,data=biomass_dat)

write.csv(biomass_dat,file="biomass_dat.csv",row.names=F)

#calculate distance matrix
bio.dists <- as.matrix(dist(cbind(biomass_dat$x, biomass_dat$y)))

#set inverse
bio.dists.inv <- 1/bio.dists
diag(bio.dists.inv) <- 0

#examine first rows of this
bio.dists.inv[1:5, 1:5]

Moran.I(biomass_dat$biomass, bio.dists.inv)


biomodel=lm(biomass~Annualrainfall,data=biomass_dat)
summary(biomodel)

bio_resids=residuals(biomodel)

Moran.I(bio_resids, bio.dists.inv)
