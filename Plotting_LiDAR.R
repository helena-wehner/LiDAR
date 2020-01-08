#################################################
###visulization
### Author: Steven Hill and Hooman Latifi
########################################
library(RStoolbox)
library(raster)
indir <- "C:\\Users/Lenovo/Desktop/Master/Latifi_Laser_UAV/LiDAR_Course2020/Excercises/R/CHM_Vis_R/"
setwd(indir)

###visulization DTM CHM

cir<-brick("CIR.tif")
dtm<-raster("DTM.tif")
chm<-raster("CHM.tif")
dtm_hillshade<-raster("DTM_hillshade.tif")
chm_hillshade<-raster("CHM_hillshade.tif")

###visulization DTM CHM
#colors
nclr <- 10# number of bins
max <- cellStats(chm,stat='max') 
min <- cellStats(chm,stat='min') #
breaks <- (max - min) / nclr
max.c<-max+breaks
min.c<-min
colfunc <- colorRampPalette(c("beige","chartreuse3","yellow","orange","red"))

#plotting
ggRGB(cir,r = 1, g = 2, b = 3)


x11(); 
par (mfrow=c(2,2))
# DTM
plot(dtm,col=colfunc(100))
plot(dtm_hillshade,col=grey(0:100/100),legend=FALSE)
plot(dtm,col=colfunc(100),alpha=0.55)

# now DTM overlaid by Hillshade  (to see the texture better)
plot(dtm_hillshade,col=grey(0:100/100),legend=FALSE)
plot(dtm,col=colfunc(100),alpha=0.55, add=T)

# CHM
x11(); 
par (mfrow=c(2,2))

plot(chm,col=colfunc(20))
plot(chm_hillshade,col=grey(0:100/100),legend=FALSE)
plot(chm,col=colfunc(20),alpha=0.55)

# now CHMS overlaid by hillshade (to see the texture better)
plot(chm_hillshade,col=grey(0:100/100),legend=FALSE)
plot(chm,col=colfunc(20),alpha=0.55,add=T)





