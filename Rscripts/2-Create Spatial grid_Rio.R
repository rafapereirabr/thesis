

### Refs ---------
# Spatial Grid - Rio
# http://gis.stackexchange.com/questions/88830/overlay-a-spatial-polygon-with-a-grid-and-check-in-which-grid-element-specific-c
# http://rfunctions.blogspot.co.uk/2014/12/how-to-create-grid-and-intersect-it.html
# https://gis.stackexchange.com/questions/82362/what-are-the-benefits-of-hexagonal-sampling-polygons
# https://www.r-bloggers.com/session-1-gridding-data-for-multi-scale-macroecological-analyses/
#> http://strimas.com/spatial/hexagonal-grids/
# http://www-cs-students.stanford.edu/~amitp/game-programming/polygon-map-generation/
# http://rstudio-pubs-static.s3.amazonaws.com/7993_6b081819ba184047802a508a7f3187cb.html






# Set Working Directory ------------



# Lap top
setwd("R:/Dropbox/Dout/Data Dout")




##################### Load packages -------------------------------------------------

source("./R scripts/00_LoadPackages.R")




# 1 Load shape file of Rio de Janeiro -------------------------------------------------
  muni <- readOGR(dsn="./Shapes_IBGE", layer="muni")      #read shape file
  plot(muni)

# Change projection to UTM WGS84
  myCRSutm <- "+proj=utm +zone=23 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0" # +units=km m cm
  muni <- spTransform(muni, CRS( myCRSutm ))
  gc(reset = T)
  
# # Change projection to Lat Long WGS84
#   muni <- spTransform(muni, CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#   proj4string(muni)



# # 2 Create Hexagonal grid various sizes -------------------


for (i in c(500, 1000, 2000, 4000)){
  
  cat("working on grid", i, "\n")

  # get bounding box
  boundingbox <- as(raster::extent( bbox(muni) ), "SpatialPolygons")
  
  # sample points
  HexPts <- spsample(boundingbox, type="hexagonal", offset=c(0.5, 0.5), cellsize= i )  # 1km
  plot(muni)
  plot(HexPts, pch = ".", add=T) #20 "."
  
  HexPols <- HexPoints2SpatialPolygons(HexPts)
  plot(HexPols, border="red", add= T)
  
  # projection in UTM
  crs(HexPols) <- myCRSutm
  HexPols <- spTransform(HexPols, CRS( myCRSutm ))
  
  
  # Intersect our grid with Muni shapefile. R might need a considerable time to do that
  hexriopoly <- raster::intersect( muni , HexPols) #intersect
  plot(hexriopoly)
  
  
  # Alternative crop keeping over border
  # hexriopoly2 <- HexPols[muni, ] #intersect
  
  # plot centroids
    trueCentroids = gCentroid(hexriopoly, byid=TRUE)
    points(trueCentroids, col="red", pch=1)
    
    # get area and remove polygons that are too small     
    hexriopoly@data$area <- area(hexriopoly) /1000000
    hexriopoly <- hexriopoly[hexriopoly$area > 0.05,]

        
  # Rename row names and ID of Grid Cells 
  hexriopoly@data$ID <- 1:nrow(hexriopoly@data) %>% as.numeric()
  row.names(hexriopoly@data) <- 1:nrow(hexriopoly@data) %>% as.character()
  

  # only keep id column
  hexriopoly@data <- hexriopoly@data[1]
  head(hexriopoly@data) # Check Grid Variables

# convert back to lat long
  hexriopoly <- spTransform(hexriopoly, CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
# save
  writeOGR(hexriopoly, dsn = './Spatial Grid', layer =paste0('hex_grid_',i) , driver = 'ESRI Shapefile')
}




