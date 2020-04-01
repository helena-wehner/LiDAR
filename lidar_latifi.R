### Lidar Course Latifi

dir <- "C:/Users/Lenovo/Desktop/Master/Latifi_Laser_UAV/"
setwd(dir)
getwd()

library(raster) #
library(sp) #
library(lidR)
library(rlas)
library(rLiDAR) #
library(alphashape3d)
library(geometry)
library(rgl) #

data <- readLAS("NEON_discrete-return-lidar-point-cloud/test_NO.las", short=T)
head(data)
plot(data)

df <- data.frame(x=data[,1], y=data[,2], z=data[,3])

# plotting in 2d and 3d
# Set a color by intensity
# color ramp
# Color by intensity
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

col <- myColorRamp(c("blue","green","yellow","red"),data[,3])

# plot 2D
plot(data[,1], data[,2],col=col, xlab="UTM.Easting", ylab="UTM.Northing", main="Color by Height")

# plot 3D (package rgdal)
points3d(data[,1:3], col=col, axes=FALSE, xlab="", ylab="", zlab="")
axes3d(c("x+", "y-", "z-")) # axes
grid3d(side=c('x+', 'y-', 'z'), col="gray") # grid
title3d(xlab = "UTM.Easting", ylab = "UTM.Northing",zlab = "Height(m)", col="red", main="Color by Height") # title
planes3d(0, 0, -1, 0.001, col="gray",alpha=0.7) # terrain


### single tree detection
# need to derive a canopy height model with FUSION

chm <- raster("NEON_discrete-return-lidar-point-cloud/results/ndsm.asc")
plot(chm, main="LiDAR derived Canopy Height Model")

# smooth the data
smooth <- CHMsmoothing(chm)
plot(smooth, main="LiDAR derived Height Model (smoothed)")

# Setting the fws:
fws<-5 # dimension 5x5

# Setting the specified height above ground for detectionbreak
minht<-8.0

# getting the individual tree detection list
tree <- FindTreesCHM(chm, fws, minht)
head(tree)
summary(tree)
length(tree[,1]) # less trees detected than in smoothed data

plot(chm, main="LiDAR-derived Canopy Height Model")

xy <- SpatialPoints(tree)
plot(xy, add=T, col="red")

# Getting the individual tree detection list (smoothed data)
treeList<-FindTreesCHM(smooth, fws, minht)
summary(treeList)
length(treeList[,1]) # more trees detected than in non-smoothed data

plot(smooth, main="LiDAR-derived Canopy Height Model")

xysmooth <- SpatialPoints(treeList)
plot(xysmooth, add=T, col="red")

#######################################################################################################
#function for Individual trees crown deliniation from LiDAR-derived CHM

# Import the LiDAR-derived CHM file

# Set the loc parameter
sCHM<-CHMsmoothing(chm, filter="mean", ws=5) # smoothing CHM
loc<-FindTreesCHM(sCHM) # or import a tree list

# Set the maxcrown parameter
maxcrown=10.0

# Set the exclusion parameter
exclusion=0.3 # 30

# Compute individual tree detection canopy area
canopy<-ForestCAS(chm, loc, maxcrown, exclusion)
summary(canopy)

#==================================================================================#
# Retrieving the boundary for individual tree detection and canopy area calculation
#==================================================================================#
boundaryTrees<-canopy[[1]]

# Plotting the individual tree canopy boundary over the CHM
plot(chm, main="LiDAR-derived Canopy Height Model")
plot(boundaryTrees, add=T, border='red', bg='transparent') # adding tree canopy boundary
#============================================================================#
# Retrieving the list of individual trees detected for canopy area calculation
#============================================================================#
canopyList<-canopy[[2]] # list of ground-projected areas of individual tree canopies
head(canopyList)
summary(canopyList) # summary

# Spatial location of the trees
XY<-SpatialPoints(canopyList[,1:2]) # Spatial points
plot(XY, col="black", add=T, pch="*") # adding tree location to the plot

####################################################################################################

