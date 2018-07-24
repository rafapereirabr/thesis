
# This script builds a Data Frame with all the
# spatial keys between the spatial units of the thesis


# 1 Read Shape files IBGE Grid200 and Grid 500m Regular + Hex
# 2 Intersect with 500m Grid (cells)regular 
# 3 Distribution Hospitals
# 4 Distribution of Schools
# 5 Distribution of JOBS



# Lap top
setwd("R:/Dropbox/Dout/Data Dout")



##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")



##### 1 Read Shape files IBGE Grid200 and Grid 500m Regular + Hex    -----------------------





# Read shapes
  hex_0500 <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_0500')
  hex_1000 <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_1000')
  hex_2000 <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_2000')
  hex_4000 <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_4000')
  map_trzn <- readOGR(dsn="./shapefiles_OD_rio_2012", layer="map_grid_trzn", stringsAsFactors = FALSE)

  gridIBGE200 <- readOGR(dsn="./Shapes_IBGE", layer="gridIBGE200")      #read shape file
  udhs_shp <- readOGR(dsn="./UDHS censo2010", layer="udhs")
  muni <- readOGR(dsn="./Shapes_IBGE", layer="muni")

  
    
# transform projections

  # lat long and utm projections
  myprojection_latlong <-  CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  myprojection_utm <-      CRS("+proj=utm +zone=23 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0")
    
  # 1st UTM to get area of each polygon
    hex_0500 <- spTransform(hex_0500, myprojection_utm)
    hex_1000 <- spTransform(hex_1000, myprojection_utm)
    hex_2000 <- spTransform(hex_2000, myprojection_utm)
    hex_4000 <- spTransform(hex_4000, myprojection_utm)
    map_trzn <- spTransform(map_trzn, myprojection_utm)
    
    gridIBGE200 <- spTransform(gridIBGE200, myprojection_utm)
    udhs_shp <- spTransform(udhs_shp, myprojection_utm)
    muni <- spTransform(muni, myprojection_utm)
    beep()
  
    
# Clip map_trzn  2003
    # map_trzn  2003 -- map_trzn <- subset(map_trzn, MUNICIPIO=="Rio de Janeiro")
    # map_trzn  2003 -- map_trzn <- raster::crop(map_trzn, muni)
    trzn <- raster::crop(map_trzn, muni)
    trzn@data <- trzn@data[1]

        
    # interactive viz of map
    plot(trzn)
    mapview::mapview(trzn)
    
    # Subset / trim map
    trzn <- subset(trzn,  !(ID %in% c(884, 1008, 998, 1003, 984, 985, 961, 974)))
    
    # plot map and centroits
    trueCentroids = gCentroid(trzn, byid=TRUE)
    plot(trzn)
    points(coordinates(trzn), col="red", pch=1)
    

    # get ID to numeric
    trzn@data$ID <- trzn@data$ID %>% as.numeric()


    
    
    
    
# get area of each polygon in Km2
  hex_0500@data$area <- area(hex_0500) /1000000
  hex_1000@data$area <- area(hex_1000) /1000000
  hex_2000@data$area <- area(hex_2000) /1000000
  hex_4000@data$area <- area(hex_4000) /1000000
  trzn@data$area <- area(trzn) /1000000
  gridIBGE200@data$area <- area(gridIBGE200) /1000000
  
  
# Assign grid identity
  hex_0500@data$grid <- 'grid_0500'
  hex_1000@data$grid <- 'grid_1000'
  hex_2000@data$grid <- 'grid_2000'
  hex_4000@data$grid <- 'grid_4000'
  trzn@data$grid <-        'grid_trzn' 
  gridIBGE200@data$grid <- 'grid_200'
  
  
    
  # Now back to lat long
  hex_0500 <- spTransform(hex_0500, myprojection_latlong)
  hex_1000 <- spTransform(hex_1000, myprojection_latlong)
  hex_2000 <- spTransform(hex_2000, myprojection_latlong)
  hex_4000 <- spTransform(hex_4000, myprojection_latlong)
  trzn <- spTransform(trzn, myprojection_latlong)
  
  gridIBGE200 <- spTransform(gridIBGE200, myprojection_latlong)
  udhs_shp <- spTransform(udhs_shp, myprojection_latlong)
  muni <- spTransform(muni, myprojection_latlong)
  beep()
  
    
# Save Shape File of Rio traffic zones
  writeOGR( gridIBGE200, dsn = './Shapes_IBGE', layer ='gridIBGE200', driver = 'ESRI Shapefile', overwrite_layer = T)
  writeOGR( hex_0500, dsn = './Spatial Grid', layer ='hex_grid_0500', driver = 'ESRI Shapefile', overwrite_layer = T)
  writeOGR( hex_1000, dsn = './Spatial Grid', layer ='hex_grid_1000', driver = 'ESRI Shapefile', overwrite_layer = T)
  writeOGR( hex_2000, dsn = './Spatial Grid', layer ='hex_grid_2000', driver = 'ESRI Shapefile', overwrite_layer = T)
  writeOGR( hex_4000, dsn = './Spatial Grid', layer ='hex_grid_4000', driver = 'ESRI Shapefile', overwrite_layer = T)
  writeOGR( trzn, dsn = './shapefiles_OD_rio_2012', layer ='map_grid_trzn', driver = 'ESRI Shapefile', overwrite_layer = T)
  gc(reset = T)

  
  
  
# # # read saved shapes
#   gridIBGE200   <- readOGR(dsn = './Shapes_IBGE', layer ='gridIBGE200' )
#   hex_0500      <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_0500' )
#   hex_1000      <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_1000' )
#   hex_2000      <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_2000' )
#   hex_4000      <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_4000' )
#   trzn      <- readOGR(dsn = './shapefiles_OD_rio_2012', layer ='map_grid_trzn')
#   udhs_shp <- readOGR(dsn="./UDHS censo2010", layer="udhs")
# # projection
#   myprojection_latlong <-  CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# 
#   hex_0500 <- spTransform(hex_0500, myprojection_latlong)
#   hex_1000 <- spTransform(hex_1000, myprojection_latlong)
#   hex_2000 <- spTransform(hex_2000, myprojection_latlong)
#   hex_4000 <- spTransform(hex_4000, myprojection_latlong)
#   trzn <- spTransform(trzn, myprojection_latlong)
#   gridIBGE200 <- spTransform(gridIBGE200, myprojection_latlong)
#   udhs_shp <- spTransform(udhs_shp, myprojection_latlong)

  
  
# 2 Intersect CT centroids with Grid cells     -----------------------
      
  # fst sf approach
    # aaa <- st_centroid(gridIBGE200)
    # out <- st_intersection(aaa, hex_4000)
  
  
      # get centroids of gridIBGE200
      grid_centroids <- SpatialPointsDataFrame(gCentroid(gridIBGE200, byid=TRUE),
                                               gridIBGE200@data, match.ID=FALSE,
                                               proj4string = myprojection_latlong)
  
        # intersect with other grids
        inter_0500 <- point.in.poly(grid_centroids, hex_0500) %>% as.data.table() %>% .[, .(id200, ID)]
        inter_1000 <- point.in.poly(grid_centroids, hex_1000) %>% as.data.table() %>% .[, .(id200, ID)]
        inter_2000 <- point.in.poly(grid_centroids, hex_2000) %>% as.data.table() %>% .[, .(id200, ID)]
        inter_4000 <- point.in.poly(grid_centroids, hex_4000) %>% as.data.table() %>% .[, .(id200, ID)]
        inter_udhs <- point.in.poly(grid_centroids, udhs_shp) %>% as.data.table() %>% .[, .(id200, UDH_ATLAS)]
        inter_trzn <- point.in.poly(grid_centroids, trzn) %>% as.data.table() %>% .[, .(id200, ID )]
        
    # change name to facilitate merge
        names(inter_0500)[2] <- 'id_0500'
        names(inter_1000)[2] <- 'id_1000'
        names(inter_2000)[2] <- 'id_2000'
        names(inter_4000)[2] <- 'id_4000'
        names(inter_trzn)[2] <- 'id_trzn'
        names(inter_udhs)[2] <- 'UDH_ATLAS'
        
        
  # convert to Data Table
  grid_centroids_df <- as.data.frame(grid_centroids) %>% setDT()
  
  
  # Merge info of intersection
    grid_centroids_df <- left_join(grid_centroids_df, inter_0500, by='id200') %>% 
                                         left_join(., inter_1000, by='id200') %>% 
                                         left_join(., inter_2000, by='id200') %>% 
                                         left_join(., inter_4000, by='id200') %>% 
                                         left_join(., inter_udhs, by='id200') %>% 
                                         left_join(., inter_trzn, by='id200') %>%  setDT()
                                    
  

    
    
    
  # rename columns with lat long
  setnames(grid_centroids_df, 'x', 'X200')
  setnames(grid_centroids_df, 'y', 'Y200')
  
  # # rename traffic zone      
  # setnames(grid_centroids_df, "ID", "id_trzn")
  
  
  head(grid_centroids_df)
  nrow(grid_centroids_df)    
  
  # population loss
  sum(gridIBGE200@data$POP) - sum(grid_centroids_df$POP) # 0
  
  
  
  
  
      
      # keep only important varibles
      spatialkey <- grid_centroids_df[, .(id200, id_0500, id_1000, id_2000, id_4000, UDH_ATLAS,  id_trzn,  X200, Y200, POP, MASC, FEM  )]
      head(spatialkey)
      
      # clean memory
      gc(reset = T)
      

      
      
#### #### #### #### #### #### #### #### #### 
#### 2.1 Add lat long of id500  and idhex500
      
      # get centroids of grid500
      centroids0500hex <- SpatialPointsDataFrame(gCentroid(hex_0500, byid=TRUE),
                                                hex_0500@data, match.ID=FALSE)
      
      centroids1000hex <- SpatialPointsDataFrame(gCentroid(hex_1000, byid=TRUE),
                                                hex_1000@data, match.ID=FALSE)
      
      centroids2000hex <- SpatialPointsDataFrame(gCentroid(hex_2000, byid=TRUE),
                                                hex_2000@data, match.ID=FALSE)
      
      centroids4000hex <- SpatialPointsDataFrame(gCentroid(hex_4000, byid=TRUE),
                                                hex_4000@data, match.ID=FALSE)
      
      centroidstrazone <- SpatialPointsDataFrame(gCentroid(trzn, byid=TRUE),
                                                 trzn@data, match.ID=FALSE)

      # filter columns and merge
        centroids0500hex <- setDT(as.data.frame(centroids0500hex))
        centroids1000hex <- setDT(as.data.frame(centroids1000hex))
        centroids2000hex <- setDT(as.data.frame(centroids2000hex))
        centroids4000hex <- setDT(as.data.frame(centroids4000hex))
        centroidstrazone <- setDT(as.data.frame(centroidstrazone))
        # keep cols id, lat long
        centroids0500hex <- centroids0500hex[, c('ID', 'x', 'y')]
        centroids1000hex <- centroids1000hex[, c('ID', 'x', 'y')]
        centroids2000hex <- centroids2000hex[, c('ID', 'x', 'y')]
        centroids4000hex <- centroids4000hex[, c('ID', 'x', 'y')]
        centroidstrazone <- centroidstrazone[, c('ID', 'x', 'y')]
        
      # change names of coordinates
        names(centroids0500hex)[1:3] <- c( 'id_0500', 'X0500' , 'Y0500' )
        names(centroids1000hex)[1:3] <- c( 'id_1000', 'X1000' , 'Y1000' )
        names(centroids2000hex)[1:3] <- c( 'id_2000', 'X2000' , 'Y2000' )
        names(centroids4000hex)[1:3] <- c( 'id_4000', 'X4000' , 'Y4000' )
        names(centroidstrazone)[1:3] <- c( 'id_trzn', 'Xtrzn' , 'Ytrzn' )
        
        
        spatialkey <- left_join(spatialkey, centroids0500hex, by="id_0500") 
        spatialkey <- left_join(spatialkey, centroids1000hex, by="id_1000")
        spatialkey <- left_join(spatialkey, centroids2000hex, by="id_2000")
        spatialkey <- left_join(spatialkey, centroids4000hex, by="id_4000")  %>% setDT()
        spatialkey <- left_join(spatialkey, centroidstrazone, by="id_trzn") %>% setDT()
        head(spatialkey)
      
# Total popultion of Rio municipality via Grid Source - 6,149,183 \ 14.689.543
  setDT(spatialkey)[, POP := as.numeric(POP) ]
  sum(spatialkey$POP, na.rm=T)  # 6.148.379

  # convert all to numeric
  # spatialkey[, names(spatialkey) := lapply(.SD, as.numeric)]
  
# save spatialkey as a.csv file
  fwrite(spatialkey, file = "./Spatial KEY/spatialkey_phd.csv")
  
  
# Clean Env. and memory
  rm(list=setdiff(ls(), c("spatialkey","gridIBGE200", "myprojection_latlong")))
  gc(reset = T)
  
  

  
  
  
  
  
  
  
##########################################################
# 3 Hospitals     -----------------------
    #  spatialkey <-  fread("./Spatial KEY/spatialkey_phd.csv")
    # spatialkey[, c("hospitals", "hosp_low", "hosp_med" , "hosp_high") := NULL]
  
  
  # read hospitals data (304 hospitals)
  hospitals_filtered <- fread("./Rio places/hospitals_filtered_correct.csv")
  nrow(hospitals_filtered)
  

  # convert into spatial point data frame
  coordinates(hospitals_filtered) = ~Longitude + Latitude
  plot(hospitals_filtered)
  
  # Apply same projection
  proj4string(hospitals_filtered) = myprojection_latlong
  proj4string(gridIBGE200) == proj4string(hospitals_filtered) # must be TRUE
  
  
  # intersec com grid 200
    hospitals_in_grid200 <- point.in.poly(hospitals_filtered, gridIBGE200) 
    plot(hospitals_in_grid200)
  
  # convert into data table
  hospitals_in_grid200 <- setDT(as.data.frame(hospitals_in_grid200))
  head(hospitals_in_grid200)
  
  # Summarize number of hospitals per grid cell
  hospitals_in_grid200_count <- hospitals_in_grid200[, .(hospitals = .N,
                                                         hosp_low = sum(hosp_low, na.rm=T),
                                                         hosp_med = sum(hosp_med, na.rm=T),
                                                         hosp_high = sum(hosp_high, na.rm=T)), , by=.(id200)]

  # check total hospitals ==304
    sum(hospitals_in_grid200_count$hospitals) == 304
  
  # merge spatial key
  spatialkey <- left_join(spatialkey, hospitals_in_grid200_count, by="id200")
  head(spatialkey)
  summary(spatialkey$hospitals)
  
  # check total hospitals ==304
  sum(spatialkey$hospitals, na.rm=T) == 304
  
  
  # save spatial key
  fwrite(spatialkey, "./Spatial KEY/spatialkey_phd.csv")
  
  
  sum(spatialkey$hosp_high, na.rm=T)
  sum(spatialkey$hosp_med, na.rm=T)
  sum(spatialkey$hosp_low, na.rm=T)
  
  # Clean Env. and memory
  rm(list=setdiff(ls(), c("spatialkey","gridIBGE200", "myprojection_latlong")))
  gc(reset = T)
  

  
  
  
  
  
  
  
  
  
   
    
    
########## 4 JOBS     (total and by edu level) to grid cells  -----------------------

# 4.0 read spatial Key    
  # spatialkey <- fread("./Spatial KEY/spatialkey_phd.csv")
    
# 4.1 read data with geolocated workers records

    grid200_edu_levels_2015 <- fread("./rais/grid200_edu_levels_2015.csv")

      
    # total jobs in the end
    sum(grid200_edu_levels_2015$totaljobs) == 2691206
    
    
  
  # merge total jobs to gridDATA
    spatialkey <- left_join(spatialkey, grid200_edu_levels_2015, by="id200")
    head(spatialkey)
    
    


    
    
    summary(spatialkey$totaljobs)
    sum(spatialkey$totaljobs, na.rm=T) == 2691206
    
# save spatial key
  fwrite(spatialkey, "./Spatial KEY/spatialkey_phd.csv")


# quick map
  ggplot(data=subset(spatialkey, POP>0)) + geom_point(aes(x=X200, y=Y200, color=POP, size=POP)) + coord_equal()
  ggplot(data=spatialkey) + geom_point(aes(x=X200, y=Y200, color=totaljobs, size=totaljobs)) + coord_equal()
  
################################################################
# 5 Schools     -----------------------
  
  # read schools data
  schools_filtered <- fread("./Rio places/schools_filtered.csv") # 279 schools
  
  nrow(schools_filtered) == 279

    
  
  # convert into spatial point data frame
  coordinates(schools_filtered) = ~lon + lat
  
  # Apply same projection
  proj4string(schools_filtered) = myprojection_latlong
  proj4string(gridIBGE200) == proj4string(schools_filtered) # must be TRUE
  plot(schools_filtered)
  
  # intersec com grid 200
  schools_in_grid200 <- point.in.poly(schools_filtered, gridIBGE200) 
  
  # convert into data table
  schools_in_grid200 <- as.data.frame(schools_in_grid200) %>% setDT()
  
  
  setdiff(schools_filtered$CO_ENTIDADE, schools_in_grid200$CO_ENTIDADE) # escola na ilha de paqueta
  

  
  # Summarize number of schools per grid cell
  schools_in_grid200.count <- schools_in_grid200[, .(schools = .N), by=.(id200)]
  plot(schools_in_grid200.count)
  
  sum(schools_in_grid200.count$schools) # 278
  
  # remove cells with too high number of schoos
  schools_in_grid200.count <- schools_in_grid200.count[schools < 40, ]
  plot(schools_in_grid200.count)
  
  
  # merge spatial key
  spatialkey <- left_join(spatialkey, schools_in_grid200.count, by="id200")
  head(spatialkey)
  
  summary(spatialkey$schools)
  sum(spatialkey$schools, na.rm=T) == 278
  
  # save spatial key
  fwrite(spatialkey, "./Spatial KEY/spatialkey_phd.csv")
  
  
  
  # Clean Env. and memory
  rm(list=setdiff(ls(), c("spatialkey","gridIBGE200", "myprojection_latlong")))
  gc(reset = T)
  
  
  
  
  
  
  
###### get elevation data ----------------
  
  
  
  # CRS projection
  myprojection_utm <- CRS("+proj=utm +units=m +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  # read grid IBGE
  grid <- readOGR(dsn="./Shapes_IBGE", layer="gridIBGE200")      #read shape file
  grid <- spTransform(grid, myprojection_utm )
  
  # read elevation raster
          # raster IPP, melhor resolucao, mas problema na projecao
          # test <- raster("./relevo/relevo2.tif.0.tif")
          # crs(test) <- myprojection_utm
          # proj4string(test) <- myprojection_utm
  
    test1 <- raster("./Topography/S23W044.hgt")
    test2 <- raster("./Topography/S24W044.hgt")
  # merger raster files
    test3 <- raster::merge(test1, test2)
    test3 <- projectRaster(test3,  crs= myprojection_utm) # lat long projection
  
  
  
  proj4string(test3) == proj4string(grid)
  
# crop
  test3 <- crop(test3, extent(grid))
  # arqui IPP  >>>> extent(test) <- extent(grid)
  
  plot(test3)
  plot(grid, add=T)
  beep()
  
  gc(reset = T)
  
  
# get elevation in each gid id  
  elevation <- raster::extract(test3, grid,  fun=mean, na.rm=T )
  beep()

# add elevation to spatial key
  spatialkey <- fread("./Spatial KEY/spatialkey_phd.csv")
  
  spatialkey$elevation <- elevation
  head(spatialkey)

# save spatial key
  fwrite(spatialkey, "./Spatial KEY/spatialkey_phd.csv")


  ggplot() + 
    geom_point(data=spatialkey, aes(x=X200, y=Y200, color=elevation)) + 
    coord_equal() + 
    scale_colour_gradientn(colours = terrain.colors(10))

  

  
  
  

###### 6. BUFFER pop around transp stops -----------------------------
  
# read shape
  #  gridIBGE200 <- readOGR(dsn="./Shapes_IBGE", layer="gridIBGE200")      #read shape file
  muni <- readOGR(dsn="./Shapes_IBGE", layer="muni")      #read shape file
  gridIBGE200 <- readOGR(dsn="./Shapes_IBGE", layer="gridIBGE200")      #read shape file
  
  
  
  # muni <- st_read(dsn = './Shapes_IBGE', layer ='muni')
  # muni <- st_transform(muni, "+proj=utm +units=m +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  
    # UTM Meter projection
    myprojection_latlong <-  CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    myprojection_utm <- CRS("+proj=utm +units=m +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    
    gridIBGE200 <- spTransform(gridIBGE200, myprojection_utm)
    muni <- spTransform(muni, myprojection_utm)
    
    # convert to SpatialPointsDataFrame
    pointsIBGE200 <- SpatialPointsDataFrame(gridIBGE200, data=gridIBGE200@data, proj4string = myprojection_utm)
    
  # BRTs do ITDP
    brt_stops <-   readOGR(dsn="./Shapes_BRT", layer="Stations_BRT_revised")      #read shape file
    brt_stops <- spTransform(brt_stops, myprojection_utm)
    brt_stops$Name <- as.character(brt_stops$Name)
    brt_stops$Corredor <- as.character(brt_stops$Corredor)
    Encoding(brt_stops$Name)  <- "UTF-8" # encode using UTF-8, para lidar com acentos no portugues
    Encoding(brt_stops$Corredor)  <- "UTF-8" # encode using UTF-8, para lidar com acentos no portugues
    plot(brt_stops)
    

    
    
# read GTFS stops
    
# read GTFS data
  gtfs_file <- "R:/Dropbox/bases_de_dados/GTFS/Rio GTFS/GTFS Rio feed_20170728_sem trens.zip"
  gtfs_file <- "./GTFS data/GTFS Rio feed_2017-05-5_sem_trens.zip" # GTFS Rio feed_2017-05-05.zip
  
  #gtfs_file <- "R:/Dropbox/OpenTripPlanner/jython_rio_2017mix/busfeed_2017-05-5.zip"
  gtfs_file <- "R:/Dropbox/OpenTripPlanner/jython_rio_2017-05-05/GTFS Rio feed_2017-05-5_sem_trens.zip"
  
  
  
  routes <- fread(unzip (gtfs_file, "routes.txt")) # get route_id
  trips <- fread(unzip (gtfs_file, "trips.txt")) # get trip_id
  stop_times <- fread(unzip (gtfs_file, "stop_times.txt")) # get stop_id
  stops <- fread(unzip (gtfs_file, "stops.txt")) # get stop_id + lat long
  shapes <- fread(unzip (gtfs_file, "shapes.txt")) # get stop_id + lat long
  
  Encoding(routes$route_long_name)  <- "UTF-8" # encode using UTF-8, para lidar com acentos no portugues
  Encoding(stops$stop_name)  <- "UTF-8" # encode using UTF-8, para lidar com acentos no portugues
  gc(reset=T)


get_pop <- function(var, route, radius) {
  
# #   #test
# #   my_route <- routes[ route_long_name %like% "Transcarioca" , .(route_id)] # Transcarioca Transoeste
#   #   my_route <- routes[ route_long_name %like% "Linha 4" , .(route_id)] # Transcarioca Transoeste
#   
# #   my_route <- routes[ get(var) %like% route , .(route_id)]
# 
#     # my_route <- routes[ route_type==1 , .(route_id)] #only subwat lines
#   
#   # var= "route_long_name" # "route_short_name" #
#   # route="Linha 4"

    
# all new investments
  #  my_route <- routes[ route_long_name %like% "Transcarioca|Transoeste|Transolímpica|Linha 4" | route_short_name %like% "VLT", .(route_id)]
  # radius=1000

  
  # identify route
  my_route <- routes[ get(var) %like% route , .(route_id)]
  
# identify trips
  my_trips <- trips[ route_id %in% my_route$route_id, .(trip_id)]
  
# identify stops
  my_stops <- stop_times[ trip_id %in% my_trips$trip_id, .(stop_id)]
  
# get correspondent stops with geolocation
  my_stops <- stops[ stop_id %in% my_stops$stop_id, .(stop_id, stop_lat, stop_lon, stop_name)]
  my_stops[, stop_lat := as.numeric(stop_lat)][, stop_lon := as.numeric(stop_lon)]
  
  
  if ( route %like% "Linha 4") {
                                my_stops <- stops[ stop_name %like% c("Jardim de Alah|São Conrado|Nossa Senhora da Paz|Jardim Oceânico|Ipanema|General Osório|Antero de Quental"), .(stop_id, stop_lat, stop_lon, stop_name)] 
                              }
  
  # # ALL new investments
  #  my_stops <- rbind(my_stops[ !(stop_name %like% c("Cantagalo|Siqueira Campos|Cardeal Arcoverde|Botafogo|Flamengo|Catete|Glória|Cinelândia|Carioca|Uruguaiana|Avenida Presidente Vargas|Rua Regente|Rua da Alfândega|Chile|Avenida Rio Branco|Central|Onze|Estácio de|Rua Carmo Neto|Afonso Pena|São Francisco Xavier|Saens Peña|Uruguai|Conde|Heitor|Satamini|Rua Carmo|Largo do Machado|de Abrantes|Nelson Mandela|Tonelero|Avenida Henrique|Ruas Viconde|Praça Grécia|Avenida Ataulfo|Rua Visconde de Pi")), .(stop_id, stop_lat, stop_lon, stop_name)] ,
  #                    stops[ stop_name %like% c("Jardim de Alah|São Conrado|Nossa Senhora da Paz|Jardim Oceânico|Ipanema|General Osório|Antero de Quental"), .(stop_id, stop_lat, stop_lon, stop_name)] )
  
  
# identify shape (EXTRA)
  my_shapes <- trips[ trip_id %in% my_trips$trip_id, .(shape_id)]
  my_shapes <- shapes[ shape_id %in% my_shapes$shape_id, .(shape_id, shape_pt_lat, shape_pt_lon)]
  my_shapes <- unique( my_shapes )
  
  my_stops$stop_lat <- as.numeric(my_stops$stop_lat)
  my_stops$stop_lon <- as.numeric(my_stops$stop_lon)
  
  my_shapes$shape_pt_lat <- as.numeric(my_shapes$shape_pt_lat)
  my_shapes$shape_pt_lon <- as.numeric(my_shapes$shape_pt_lon)
  
  ggplot() +
    geom_spatial(muni, col="gray", fill=NA) +
    geom_point(data=my_stops, aes(x=stop_lon, y=stop_lat, group=stop_id)) +
    geom_path(data=my_shapes, aes(x=shape_pt_lon, y=shape_pt_lat, group=shape_id, color=shape_id)) +
    coord_equal()  
  

# Convert stops into spatial points
  if(route=="Transoeste"){my_stops <-  subset(brt_stops, Corredor == "TransOeste" & Status == "Operational")}
  if(route=="TransBrasil"){my_stops <-  subset(brt_stops, Corredor == "TransBrasil")} else{
  
  my_stops <- SpatialPointsDataFrame(coords =  my_stops[,.(stop_lon,stop_lat)], data = my_stops, proj4string = myprojection_latlong)
  my_stops <- spTransform(my_stops, myprojection_utm)
  }
  plot(muni)
  plot(my_stops, add=T)
  

  

  
  # # interactive viz of map
  # mapview::mapview(my_stops)
  
  
# create buffer for all stops
  stop_buffer <- gBuffer( my_stops, width= radius, byid=TRUE)
  #stop_buffer@data <- na.omit(stop_buffer@data)
  stop_buffer@data$Type <- NULL
  plot(stop_buffer, add=T)

  
# overlay buffer over pop grid centroids
  gridcells <- point.in.poly( pointsIBGE200, stop_buffer)
  plot(gridcells, col = gridcells@data$POP, pch=1, add=T)
  head(gridcells)

  # convert POP to numeric and sum
  gridcells@data$POP <- as.numeric(gridcells@data$POP)
  
  # ggplot() + 
  #   geom_spatial(muni, col="gray", fill=NA) +
  #   geom_spatial(stop_buffer, col="red") +
  #   geom_spatial(gridcells, col=gridcells@data$POP) + coord_equal()
  
  return( sum(gridcells@data$POP) )
  }
    

# BRTS
  get_pop(var="route_long_name", route="Transcarioca", radius=1000)  # 667,070    ||| (ITDP shape) 566,428
  get_pop("route_long_name", "Transoeste", radius=1000)    # 489,786 ||| (ITDP shape)
  get_pop("route_long_name", "Transolímpica", radius=1000) # 238,736 ||| (ITDP shape)
  get_pop(var="route_long_name", route="TransBrasil", radius=1000) # 238,736 ||| (ITDP shape)
  
  
# VLT
  get_pop("route_short_name", "VLT", radius=1000)        # 87,872    ||| (ITDP shape)
# Metro
  get_pop(var="route_long_name", route="Linha 4", radius=1000)         # 88,710    ||| (ITDP shape)


  
# POP ao redor de todos novos investimentos
  get_pop_allinvest <- function(radius) {
    
    
    # get stops linha 4
    my_stops_linha4 <- stops[ stop_name %like% c("Jardim de Alah|São Conrado|Nossa Senhora da Paz|Jardim Oceânico|Ipanema|General Osório|Antero de Quental"), .(stop_id, stop_lat, stop_lon, stop_name)] 
    
    # convert to spatial object
    my_stops_linha4$stop_lat <- as.numeric(my_stops_linha4$stop_lat)
    my_stops_linha4$stop_lon <- as.numeric(my_stops_linha4$stop_lon)
    my_stops_linha4 <- SpatialPointsDataFrame(coords =  my_stops_linha4[,.(stop_lon,stop_lat)], data = my_stops_linha4, proj4string = myprojection_latlong)
    my_stops_linha4 <- spTransform(my_stops_linha4, myprojection_utm)
    
    
    # get stops VLT
    my_route <- routes[ route_short_name %like% "VLT" , .(route_id)] # identify route
    my_trips <- trips[ route_id %in% my_route$route_id, .(trip_id)] # identify trips
    my_stops_VLT <- stop_times[ trip_id %in% my_trips$trip_id, .(stop_id)] # identify stops
    my_stops_VLT <- stops[ stop_id %in% my_stops_VLT$stop_id, .(stop_id, stop_lat, stop_lon, stop_name)] # get correspondent stops with geolocation
    my_stops_VLT[, stop_lat := as.numeric(stop_lat)][, stop_lon := as.numeric(stop_lon)]
    
    # convert to spatial object
    my_stops_VLT <- SpatialPointsDataFrame(coords =  my_stops_VLT[,.(stop_lon,stop_lat)], data = my_stops_VLT, proj4string = myprojection_latlong)
    my_stops_VLT <- spTransform(my_stops_VLT, myprojection_utm)
    
    
    # get stops BRTs   
    # my_stops_BRT <-  subset(brt_stops, (Corredor == "TransOeste" & Status == "Operational") | (Corredor == "TransCarioca" & Status == "Operational") | (Corredor == "TransBrasil") )
    my_stops_BRT <-  copy(brt_stops)
    
    
    plot(muni)
    plot(my_stops_BRT, add=T, col="red")
    plot(my_stops_VLT, add=T, col="green")
    plot(my_stops_linha4, add=T, col="blue")
    
    # create buffer for all stops
    buffer1 <- gBuffer( my_stops_BRT, width= radius, byid=TRUE)
    buffer2 <- gBuffer( my_stops_VLT, width= radius, byid=TRUE)
    buffer3 <- gBuffer( my_stops_linha4, width= radius, byid=TRUE)
    
    # merge buffers
    stop_buffer <- raster::union(buffer1, buffer2)
    stop_buffer <- raster::union(stop_buffer, buffer3)
    
    stop_buffer <- stop_buffer[,-(3:12)] # remove columns with NA
    stop_buffer@data[is.na(stop_buffer@data)] <- "0"
    plot(stop_buffer, add=T)
    
    
    
    # overlay buffer over pop grid centroids
    gridcells <- point.in.poly( pointsIBGE200, stop_buffer)
    plot(gridcells, col = gridcells@data$POP, pch=1, add=T)
    head(gridcells)
    
    # convert POP to numeric and sum
    gridcells@data$POP <- as.numeric(gridcells@data$POP)
    
    
    return( sum(gridcells@data$POP) )
  }
  
  get_pop_allinvest(radius=1000) # 1803960
  
  
  
  
  
  
  

  
  


