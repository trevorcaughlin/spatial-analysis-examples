#CAR example with spautolm

library(spdep)
library("maptools")
library("rgdal")

#make polygon

#Sudden Infant Death Syndrome in North Carolina, aggregated from 1979 to 1983

nc.sid <- readShapePoly(
  system.file("etc/shapes/sids.shp",
              package="spdep")[1],
  ID="FIPSNO", 
  proj4string=CRS("+proj=longlat +ellps=clrk66"))

plot(nc.sid)


nc.sids=nc.sid[-which(nc.sid$NAME %in% c("Dare","Hyde")==T),]

#regional ids and stored
neighborhoods<-poly2nb(nc.sids)

#plot the state
plot(nc.sids,col="gray")

#get x and y coordinates
pcoords=getSpPPolygonsLabptSlots(nc.sids)

#add neighbor matrix here
plot(neighborhoods,coords=pcoords,add=T,col="yellow",lwd=0.5,cex=0.8)

#make list of neighborhoods
pnb=nb2listw(neighborhoods, style="B", zero.policy=TRUE)

#fitting the model
CARmodel<-spautolm(SID79~BIR79,data=nc.sids,family="CAR",listw=pnb)

#summary of the model. lambda is spatial impact.
summary(CARmodel,Nagelkerke=T)
