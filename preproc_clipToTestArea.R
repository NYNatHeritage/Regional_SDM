# File: preproc_clipToTestArea.r
# Purpose: This script will clip rasters and point shapefiles to a test area. 
#  If we find ourselves creating models within subsets of the range, this 
#  script should be able to create our subsets.

library(raster)
library(rgdal)

# set this path to the folder where the environmental rasters reside
pathToTifs <- "X:/RegionalSDM/ScirAnci/env_vars/geotiffs"

# the path to write out the brick to
pathToClipped <- "X:/RegionalSDM/ScirAnci/zz_testArea/env_vars/geotiffs"

# path to the shape to use for clipping
pathToClipShape <- "X:/RegionalSDM/ScirAnci/zz_testArea"
clipShapeName <- "testAreaGlypMuhl_AlbersUSGS"

clpShp <- readOGR(pathToClipShape,clipShapeName)

# get a list of the grids
tiflist <- list.files(path = pathToTifs, pattern = ".tif$")

## already got some clipped? use the next few lines to check and 
## remove the ones already done
donetiflist <- list.files(path = pathToClipped, pattern = ".tif$")
finalTifList <- tiflist[!tiflist %in% donetiflist]
tiflist <- finalTifList


# tack on the full paths and name them
gridlist<-as.list(paste(pathToTifs,tiflist,sep = "/"))
nm <- substr(tiflist,1,nchar(tiflist) - 4)
names(gridlist)<-nm

for (i in 1:length(gridlist)){
  ras <- raster(gridlist[[i]])
  fn <- paste(pathToClipped, "/", names(gridlist[i]), ".tif", sep="")
  a <- crop(ras,clpShp, filename = fn, format = "GTiff", overwrite = TRUE)
}


### now clip the points to same rectangle

pathToBackgPts <- "X:/RegionalSDM/ScirAnci/inputs/background"
backgPts <- "clpBnd_SDM_RanPts"
outPathBkg <- "X:/RegionalSDM/ScirAnci/zz_testArea/inputs/background"

bigArea <- readOGR(pathToBackgPts, backgPts)
smallArea <- bigArea[clpShp,]
writeOGR(smallArea, outPathBkg, backgPts, driver="ESRI Shapefile")

pathToPresPts <- "X:/RegionalSDM/ScirAnci/inputs/species/glypmuhl/point_data"
presPts <- "glypmuhl_att"
outPathPres <- "X:/RegionalSDM/ScirAnci/zz_testArea/inputs/species/glypmuhl/point_data"

bigArea <- readOGR(pathToPresPts, presPts)
smallArea <- bigArea[clpShp,]
writeOGR(smallArea, outPathPres, presPts, driver="ESRI Shapefile")

pathToPresPolys <- "X:/RegionalSDM/ScirAnci/inputs/species/glypmuhl/polygon_data"
presPolys <- "glypmuhl"
outPathPres <- "X:/RegionalSDM/ScirAnci/zz_testArea/inputs/species/glypmuhl/polygon_data"

bigArea <- readOGR(pathToPresPolys, presPolys)
smallArea <- bigArea[clpShp,]
writeOGR(smallArea, outPathPres, presPolys, driver="ESRI Shapefile")





