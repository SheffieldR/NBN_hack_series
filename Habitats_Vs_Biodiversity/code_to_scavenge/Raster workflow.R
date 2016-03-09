#Set up
rm(list = ls())

require(raster)
require(ncdf4)

#...Load and visualise the 2 maps reuired for area bathymetry
bath.ns<-raster(x="~/Documents/SATELLITE/Associated data/Bathymetry Greater North Sea mean.nc")
bath.cs<-raster(x="~/Documents/SATELLITE/Associated data/Bathymetry Celtic Seas mean.nc")


plot(bath.ns)
plot(bath.cs)

#...Define areal extent of study
bath.ext<-extent(c(-4,11,51,61))

#...Merge the 2 tiles & check
bath<-merge(bath.ns, bath.cs)

plot(bath)

#...Crop to size
bath<-crop(bath, y=extent(c(-4,11,51,61)))
#...Aggregate
bath<-aggregate(bath, 2)
#...Resample
bath.res <- raster(nrow=1112, ncol=926, ext=bath.ext)
bath <- resample(bath, bath.res, method='bilinear')

save(bath, file="~/Documents/SATELLITE/Associated data/r files/Bathymetry.rast.RData")
load(file="~/Documents/SATELLITE/Associated data/r files/Bathymetry.rast.RData")

#...Convert raster layer to matrix
bath.m<-as.matrix(bath)

#...Clean matrix
bath.m[which(bath.m<=0, arr.ind=TRUE)]<-0 #Clean negative depths
bath.m[which(is.na(bath.m), arr.ind=TRUE)]<-0 #Clean land
#...SAVE
save(bath.m, file="~/Documents/SATELLITE/Associated data/r files/Bathymetry.RData")

#...MATCH...........................
#...LOAD
load(file="~/Documents/SATELLITE/Associated data/r files/Bathymetry.RData")


#Load latest trainind dataset
load(file="/Users/annakrystalli/Documents/TRAINING DATA/Latest training save.RData")

X$bath<-NA
X$bath<-bath.m[cbind(X$r.ind, X$c.ind)]


#...NAO__________________________________________________________________________________________________________________

NAO<-read.csv("~/Documents/SATELLITE/Associated data/NAO.csv", header=TRUE)
NAO$m.index<-paste(NAO$year, "-", NAO$month, "-", sep="")

save(NAO, file="~/Documents/SATELLITE/Associated data/r files/NAO.RData")
load(file="~/Documents/SATELLITE/Associated data/r files/NAO.RData")