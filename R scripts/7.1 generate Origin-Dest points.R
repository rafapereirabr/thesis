# This script:
# 1 Prepare Origin-Destination points
# 2 compute OD Matricies in Jython ( !!! outside R !!!)
    # (to be done yet) 3 Compute Population and Jobs within 1km radius before and after investments
# 4 Accessibility, compute
# Analysis


  

# set working Directory
setwd("R:/Dropbox/Dout/Data Dout")


##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")



      
########## 1 Prepare Origin-Destination points -------------------------------------------------------

# read grid data
  gridDaTA_0500 <- fread("./Spatial Grid/gridDaTA_0500.csv")
  gridDaTA_1000 <- fread("./Spatial Grid/gridDaTA_1000.csv")
  gridDaTA_2000 <- fread("./Spatial Grid/gridDaTA_2000.csv")
  gridDaTA_4000 <- fread("./Spatial Grid/gridDaTA_4000.csv")
  gridDaTA_trzn <- fread("./Spatial Grid/gridDaTA_trzn.csv")
  
  
# function to save points (population-weighted centroids)
get_points <- function(DATA, i) {
    
                      # collapse data to idhex500 level
                      temp <- DATA[, .( X = X[1L],
                                        Y = Y[1L],
                                        #area=area[1L],
                                        POP = sum(pop, na.rm=T),
                                        totaljobs = sum(totaljobs, na.rm=T),
                                        hospitals = sum(hospitals, na.rm=T),
                                        schools = sum(schools, na.rm=T)), by = ID ]
                      
                      # Keep only those cells that have some activity (pop, job, school or hospital)
                      temp <- temp[POP > 0 | totaljobs > 0 | hospitals > 0 | schools > 0, .(ID, X, Y)]
                      
                      # quick map
                      plot(temp$X, temp$Y)
                      
                      # Sort data by and change name of ID column
                      temp <- temp[order(ID)]

                      xx <- i
                                            
                      # save points
                        fwrite(temp, file= paste0("./Spatial Grid/points_id_",xx,".csv") )
                      return(temp)
                    }

# Save points (population-weighted centroids)
  points_0500 <- get_points( DATA = gridDaTA_0500 , i = '0500')
  points_1000 <- get_points( DATA = gridDaTA_1000 , i = '1000')
  points_2000 <- get_points( DATA = gridDaTA_2000 , i = '2000')
  points_4000 <- get_points( DATA = gridDaTA_4000 , i = '4000')
  points_trzn <- get_points( DATA = gridDaTA_trzn , i = 'trzn')


# lat long projection
  myprojection_latlong <- "+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
# UTM projection
  myCRSutm <- "+proj=utm +zone=23 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0" # +units=km m cm
  
# read street network
  streets <- readOGR(dsn = './OSM_rio-de-janeiro', layer ='osm_roads')
  streets <- spTransform(streets, myprojection_latlong)
  streets <- spTransform(streets, myCRSutm)
  
  
  
  
# function to Correct points (snap points to closes road segment)
correct_points <- function(DATA, i) {
    # DATA <- copy(points_4000)
    # i=4000
    
    if (i=="trzn"){  cut_dist = 1000  } else { cut_dist = i * 2/3 }
    
# convert points to SpatialPointsDataFrame
  pts <- SpatialPointsDataFrame(coords = DATA[, .(X, Y)], proj4string = CRS(myprojection_latlong), data = DATA) # create points and transform
  pts <- spTransform(pts, myprojection_latlong)
  pts <- spTransform(pts, myCRSutm)
  
  # plot
 # plot(streets, col="gray")
#  plot(pts, add=T, col="blue")
  
  # snap points to closest road
    new_points <- snapPointsToLines(pts, streets, maxDist = cut_dist) 
  #  plot(new_points, add=T, col="red")
    new_points <- spTransform(new_points, myprojection_latlong)
 
  # get coorect coordinates
    new_coords <- as.data.frame(new_points) %>% setDT()
    new_coords <- new_coords[, .(ID, X.1, Y.1)] 
    names(new_coords) <- c('ID','X','Y')
    
    return(new_coords)
    beep()
    }

  
  
  points_0500 <- correct_points( DATA = points_0500 , i = 0500)
  points_1000 <- correct_points( DATA = points_1000 , i = 1000)
  points_2000 <- correct_points( DATA = points_2000 , i = 2000)
  points_4000 <- correct_points( DATA = points_4000 , i = 4000)
  points_trzn <- correct_points( DATA = points_trzn , i = 'trzn')
  
  beep()
  
  
  # Remove 0 num ano e infinito na diferenca
  points_1000 <- subset(points_1000, ID != 1436 ) # industrial areas -  fronteira nordeste
  points_1000 <- subset(points_1000, ID != 1240 ) # rural area - north
  
  # Remove 0 num ano e infinito na diferenca
  points_0500 <- subset(points_0500, ID != 5445 ) # industrial areas -  fronteira nordeste
  points_0500 <- subset(points_0500, ID != 5446 ) # industrial areas -  fronteira nordeste
  points_0500 <- subset(points_0500, ID != 5463 ) # industrial areas -  fronteira nordeste
  points_0500 <- subset(points_0500, ID != 4689 ) # rural area - north
  

# salva pontos corrigidos
  fwrite(points_0500, "R:/Dropbox/Dout/Data Dout/Spatial Grid/points_id_0500_corrected.csv")
  fwrite(points_1000, "R:/Dropbox/Dout/Data Dout/Spatial Grid/points_id_1000_corrected.csv")
  fwrite(points_2000, "R:/Dropbox/Dout/Data Dout/Spatial Grid/points_id_2000_corrected.csv")
  fwrite(points_4000, "R:/Dropbox/Dout/Data Dout/Spatial Grid/points_id_4000_corrected.csv")
  fwrite(points_trzn, "R:/Dropbox/Dout/Data Dout/Spatial Grid/points_id_trzn_corrected.csv")
  
  
  
  
  
 ######## roda test de travel time ######## trips de 1h entre 8 e 9am -------------------------
######## roda test de travel time ######## trips de 1h entre 8 e 9am -------------------------
######## roda test de travel time ######## trips de 1h entre 8 e 9am -------------------------
######## roda test de travel time ######## trips de 1h entre 8 e 9am -------------------------
######## roda test de travel time ######## trips de 1h entre 8 e 9am -------------------------
######## roda test de travel time ######## trips de 1h entre 8 e 9am -------------------------
######## roda test de travel time ######## trips de 1h entre 8 e 9am -------------------------
######## roda test de travel time ######## trips de 1h entre 8 e 9am -------------------------
# agora checa os poligons que ficaram missing e corrige o centroide deles manualmente
  
  
  
  
  
  
  
  
  

############# read Maps  ----------------------

# read maps
muni <- st_read(dsn = './Shapes_IBGE', layer ='muni')
#gridIBGE200 <- st_read(dsn = './Shapes_IBGE', layer ='gridIBGE200')
hex_grid_0500 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_0500')
hex_grid_1000 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_1000')
hex_grid_2000 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_2000')
hex_grid_4000 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_4000')
map_grid_trzn <- st_read(dsn = './shapefiles_OD_rio_2012', layer ='map_grid_trzn')


# read points
points_0500 <- fread("./Spatial Grid/points_id_0500.csv")
points_1000 <- fread("./Spatial Grid/points_id_1000.csv")
points_2000 <- fread("./Spatial Grid/points_id_2000.csv")
points_4000 <- fread("./Spatial Grid/points_id_4000.csv")
points_trzn <- fread("./Spatial Grid/points_id_trzn.csv")


# subset maps, only keep origin points
hex_grid_0500 <-  subset(hex_grid_0500, ID %in% points_0500$ID )
hex_grid_1000 <-  subset(hex_grid_1000, ID %in% points_1000$ID )
hex_grid_2000 <-  subset(hex_grid_2000, ID %in% points_2000$ID )
hex_grid_4000 <-  subset(hex_grid_4000, ID %in% points_4000$ID )
map_grid_trzn <-  subset(map_grid_trzn, ID %in% points_trzn$ID )



plot(hex_grid_0500 )
plot(hex_grid_1000 )
plot(hex_grid_2000 )
plot(hex_grid_4000 )
plot(map_grid_trzn )


############# read travel matrix ----------------------

# get files
tt_0500 <- list.files("R:/Dropbox/OpenTripPlanner/jython_rio_2014-08", pattern="matrix_500", full.names=TRUE)  %>% .[c(1,2)]
tt_1000 <- list.files("R:/Dropbox/OpenTripPlanner/jython_rio_2014-08", pattern="matrix_1000", full.names=TRUE) %>% .[c(1,2)]
tt_2000 <- list.files("R:/Dropbox/OpenTripPlanner/jython_rio_2014-08", pattern="matrix_2000", full.names=TRUE) %>% .[c(1,2)]
tt_4000 <- list.files("R:/Dropbox/OpenTripPlanner/jython_rio_2014-08", pattern="matrix_4000", full.names=TRUE) %>% .[c(1,2)]
tt_trzn <- list.files("R:/Dropbox/OpenTripPlanner/jython_rio_2014-08", pattern="matrix_trzn", full.names=TRUE) %>% .[c(1,2)]





# read travel time
tt_0500 <- pblapply( tt_0500, fread) %>% rbindlist()
tt_1000 <- pblapply( tt_1000, fread) %>% rbindlist()
tt_2000 <- pblapply( tt_2000, fread) %>% rbindlist()
tt_4000 <- pblapply( tt_4000, fread) %>% rbindlist()
tt_trzn <- pblapply( tt_trzn, fread) %>% rbindlist()




############# ACCESSIBILITY ----------------------

# function to calculate accessibility 
get_oaccess <- function(data) {
  
  # convert time to minutes
  data[ , travel_time := travel_time/60]
  # create homogeneous distribution of jobs in the territory
  data[ , jobs := 1 ]
  
  # access for each dept time
  access_moment <- data[ travel_time <= 60, .(accessjobs = sum(jobs, na.rm=T)),
                         by=.(origin, year, depart_time)]
  
  # average access over the day
  access_day <- access_moment[ , .(avg_accessjobs = mean(accessjobs, na.rm=T)),
                               by=.(origin, year)]
  
  access_day[, origin := as.numeric(origin)]
  return(access_day)
}

# get accessibility for each grid
oaccess_0500 <- get_oaccess(tt_0500)
oaccess_1000 <- get_oaccess(tt_1000)
oaccess_2000 <- get_oaccess(tt_2000)
oaccess_4000 <- get_oaccess(tt_4000)
oaccess_trzn <- get_oaccess(tt_trzn)


# Merge access data with sf files  
hex_grid_0500 <- left_join( hex_grid_0500 ,  oaccess_0500, by=c('ID'='origin') )   
hex_grid_1000 <- left_join( hex_grid_1000 ,  oaccess_1000, by=c('ID'='origin') )   
hex_grid_2000 <- left_join( hex_grid_2000 ,  oaccess_2000, by=c('ID'='origin') )   
hex_grid_4000 <- left_join( hex_grid_4000 ,  oaccess_4000, by=c('ID'='origin') )   
map_grid_trzn <- left_join( map_grid_trzn ,  oaccess_trzn, by=c('ID'='origin') )

gc(reset = T)
gc(reset = T)

##### Maps --------------------

access_map <- function(xxx){ ggplot(data= xxx ) +
    geom_sf(data= muni,  fill=NA, color="gray70") +
    geom_sf( aes(fill=avg_accessjobs, label=get(names(xxx)[1]) ), color="gray70") +
    scale_fill_distiller( palette="Oranges", guide = "colorbar", name="Jobs\nDensity", direction = 1) +
    theme_map() } 



access_map( hex_grid_0500 )
access_map( hex_grid_1000 )
access_map( hex_grid_2000 )
access_map( hex_grid_4000 )
access_map( map_grid_trzn )


ggplotly()






######## Manual correction of coodinates ########  -------------------------
######## Manual correction of coodinates ########  -------------------------
######## Manual correction of coodinates ########  -------------------------


### 4000 -------------------------------------------------------------------------------------------

access_map( hex_grid_4000 )

# areas with missing info (4) (1)
hex_grid_4000$ID[which(is.na(hex_grid_4000$avg_accessjobs))]

# manually correct centroids to closest road segment
  points_4000[ ID== 115, c('X', 'Y') := .( -43.24655,-22.81456 ) ]
  points_4000[ ID==  60, c('X', 'Y') := .( -43.44129,-22.93219 ) ]
  points_4000[ ID==  44, c('X', 'Y') := .( -43.42409,-22.96960 ) ]
  points_4000[ ID==  42, c('X', 'Y') := .( -43.50658,-22.97026 ) ]



#gridDaTA_4000[ ID ==42]
#spatialkey <- fread("./Spatial KEY/spatialkey_phd.csv")
# 
# 
# id53 <- spatialkey[ id_4000 ==44]
# 
# id53[ edusup >0 ]
# sum(id53$edusup, na.rm=T)




### 2000 -------------------------------------------------------------------------------------------
  access_map( hex_grid_2000 )
  
# areas with missing info (?) (4)(4)
hex_grid_2000$ID[which(is.na(hex_grid_2000$avg_accessjobs))] %>% length()

  
# manually correct centroids to closest road segment
  points_2000[ ID ==  58, c('X', 'Y')   := .( -43.54288,-22.99699 ) ]
  points_2000[ ID ==  59, c('X', 'Y')   := .( -43.53493,-23.00097 ) ]
  points_2000[ ID ==  70, c('X', 'Y')   := .( -43.31143,-22.99152 ) ]
  points_2000[ ID == 100, c('X', 'Y')   := .( -43.27291,-22.98300 ) ]
  points_2000[ ID == 126, c('X', 'Y')   := .( -43.31924,-22.96634 ) ]
  points_2000[ ID == 162, c('X', 'Y')   := .( -43.21773,-22.94691 ) ]
  points_2000[ ID == 191, c('X', 'Y')   := .( -43.29044,-22.92470 ) ]
  points_2000[ ID == 222, c('X', 'Y')   := .( -43.31781,-22.91033 ) ]
  points_2000[ ID == 216, c('X', 'Y')   := .( -43.43300,-22.91684 ) ]
  points_2000[ ID == 300, c('X', 'Y')   := .( -43.64784,-22.87834 ) ]
  
  points_2000[ ID ==  82, c('X', 'Y')   := .( -43.62182,-22.98256 ) ]
  points_2000[ ID == 296, c('X', 'Y')   := .( -43.72532,-22.88120 ) ]
  points_2000[ ID == 371, c('X', 'Y')   := .( -43.49942,-22.83889 ) ]
  points_2000[ ID == 369, c('X', 'Y')   := .( -43.54602,-22.82979 ) ] # lost
  
  #gridDaTA_2000[ ID == 369]






### 1000 -------------------------------------------------------------------------------------------
  access_map( hex_grid_1000 )
  
# areas with missing info (17)( 11)
hex_grid_1000$ID[which(is.na(hex_grid_1000$avg_accessjobs))] %>% length()

# manually correct centroids to closest road segment
  gridDaTA_1000[ ID== 1385]


# Remove 0 num ano e infinito na diferenca
  points_1000 <- subset(points_1000, ID != 1436 ) # industrial areas -  fronteira nordeste
  points_1000 <- subset(points_1000, ID != 1240 ) # rural area - north
  
  
points_1000[ ID==   98, c('X', 'Y')   := .( -43.54432,-23.01891 ) ]
points_1000[ ID==  126, c('X', 'Y')   := .( -43.49946,-23.01692 ) ]
points_1000[ ID==  137, c('X', 'Y')   := .( -43.39132,-23.01283 ) ]
points_1000[ ID==  295, c('X', 'Y')   := .( -43.62081,-22.98179 ) ] 
points_1000[ ID==  503, c('X', 'Y')   := .( -43.24127,-22.95624 ) ] # lost !!!!!!!!! Mudei empresas de lugar
points_1000[ ID==  745, c('X', 'Y')   := .( -43.30033,-22.92242 ) ]
points_1000[ ID==  935, c('X', 'Y')   := .( -43.34176,-22.89986 ) ] # lost
points_1000[ ID==  966, c('X', 'Y')   := .( -43.65453,-22.89855 ) ]
points_1000[ ID== 1024, c('X', 'Y')   := .( -43.68738,-22.89438 ) ]
points_1000[ ID== 1078, c('X', 'Y')   := .( -43.70068,-22.88232 ) ] # lost fazenda
points_1000[ ID== 1079, c('X', 'Y')   := .( -43.68892,-22.88342 ) ] # lost fazenda
points_1000[ ID== 1132, c('X', 'Y')   := .( -43.72982,-22.87599 ) ]
points_1000[ ID== 1133, c('X', 'Y')   := .( -43.71141,-22.87661 ) ]
points_1000[ ID== 1186, c('X', 'Y')   := .( -43.70976,-22.86862 ) ] # lost fazenda
points_1000[ ID== 1283, c('X', 'Y')   := .( -43.56787,-22.84979 ) ]
points_1000[ ID== 1385, c('X', 'Y')   := .( -43.53060,-22.82787 ) ] # lost
points_1000[ ID== 313, c('X', 'Y')   := .( -43.44395,-22.98024 ) ] # lost

# extra - Cemiterio Israelita de Inhauma
points_1000[ ID ==  1121, c('X', 'Y')   := .( -43.28450,-22.87939 ) ]


#points_1000[ ID== 964, c('X', 'Y')   := .( ) ]


### 500 -------------------------------------------------------------------------------------------
access_map( hex_grid_0500 )

gridDaTA_0500[ ID== 2770]

# areas with missing info (43)( 38)(21)
hex_grid_0500$ID[which(is.na(hex_grid_0500$avg_accessjobs))] %>% length()




# Remove 0 num ano e infinito na diferenca
  points_0500 <- subset(points_0500, ID != 5445 ) # industrial areas -  fronteira nordeste
  points_0500 <- subset(points_0500, ID != 5446 ) # industrial areas -  fronteira nordeste
  points_0500 <- subset(points_0500, ID != 5463 ) # industrial areas -  fronteira nordeste
  points_0500 <- subset(points_0500, ID != 4689 ) # rural area - north
  


# manually correct centroids to closest road segment 
points_0500[ ID ==  310, c('X', 'Y')   := .( -43.54595,-23.02059 ) ]
points_0500[ ID ==  347, c('X', 'Y')   := .( -43.54458,-23.01940 ) ]
points_0500[ ID ==  348, c('X', 'Y')   := .( -43.54399,-23.01767 ) ]
points_0500[ ID ==  356, c('X', 'Y')   := .( -43.50351,-23.01785 ) ]
points_0500[ ID ==  429, c('X', 'Y')   := .( -43.38846,-23.01235 ) ]
points_0500[ ID == 1067, c('X', 'Y')   := .( -43.61918,-22.98153 ) ]
points_0500[ ID == 1170, c('X', 'Y')   := .( -43.63483,-22.97652 ) ]
points_0500[ ID == 1173, c('X', 'Y')   := .( -43.62080,-22.98178 ) ]
points_0500[ ID == 1174, c('X', 'Y')   := .( -43.61456,-22.97996 ) ]
points_0500[ ID == 1453, c('X', 'Y')   := .( -43.31915,-22.96633 ) ]
points_0500[ ID ==  1494, c('X', 'Y')   := .( -43.66087,-22.96700 ) ]
points_0500[ ID ==  1812, c('X', 'Y')   := .( -43.24253,-22.95628 ) ] # lost
points_0500[ ID ==  1843, c('X', 'Y')   := .( -43.66143,-22.95710 ) ]
points_0500[ ID ==  1955, c('X', 'Y')   := .( -43.70071,-22.95285 ) ]
points_0500[ ID ==  1956, c('X', 'Y')   := .( -43.69883,-22.95085 ) ]
points_0500[ ID ==  1963, c('X', 'Y')   := .( -43.66203,-22.95412 ) ]
points_0500[ ID ==  2083, c('X', 'Y')   := .( -43.65974,-22.94634 ) ]
points_0500[ ID ==  2770, c('X', 'Y')   := .( -43.30650,-22.92319 ) ]
points_0500[ ID ==  2896, c('X', 'Y')   := .( -43.30916,-22.92312 ) ]
points_0500[ ID ==  2897, c('X', 'Y')   := .( -43.30355,-22.92302 ) ]
points_0500[ ID ==  3149, c('X', 'Y')   := .( -43.33586,-22.91509 ) ]
points_0500[ ID ==  3598, c('X', 'Y')   := .( -43.64348,-22.90249 ) ] # lost
points_0500[ ID ==  3599, c('X', 'Y')   := .( -43.64285,-22.90070 ) ] # lost
points_0500[ ID ==  3723, c('X', 'Y')   := .( -43.65404,-22.89534 ) ]
points_0500[ ID ==  3725, c('X', 'Y')   := .( -43.64092,-22.89899 ) ] # lost
points_0500[ ID ==  3946, c('X', 'Y')   := .( -43.68772,-22.89077 ) ] #????????? que empregos aqui?
points_0500[ ID ==  4056, c('X', 'Y')   := .( -43.68961,-22.88619 ) ]
points_0500[ ID ==  4165, c('X', 'Y')   := .( -43.70133,-22.88300 ) ]  # lost fazenda
points_0500[ ID ==  4228, c('X', 'Y')   := .( -43.39610,-22.88152 ) ]
points_0500[ ID ==  4272, c('X', 'Y')   := .( -43.72473,-22.87806 ) ]
points_0500[ ID ==  4286, c('X', 'Y')   := .( -43.65434,-22.87881 ) ]
points_0500[ ID ==  4379, c('X', 'Y')   := .( -43.72807,-22.87433 ) ] # lost 
points_0500[ ID ==  4381, c('X', 'Y')   := .( -43.71786,-22.87307 ) ]
points_0500[ ID ==  4485, c('X', 'Y')   := .( -43.71496,-22.87119 ) ]
points_0500[ ID ==  4923, c('X', 'Y')   := .( -43.56985,-22.85096 ) ]
points_0500[ ID ==  4996, c('X', 'Y')   := .( -43.56452,-22.84659 ) ]
points_0500[ ID ==  5247, c('X', 'Y')   := .( -43.53446,-22.82957 ) ]  # lost 
points_0500[ ID ==  5248, c('X', 'Y')   := .( -43.53120,-22.82825 ) ]  # lost
points_0500[ ID ==  5293, c('X', 'Y')   := .( -43.53033,-22.82763 ) ] # lost

# extra - Cemiterio Israelita de Inhauma
points_0500[ ID ==  4251, c('X', 'Y')   := .( -43.28450,-22.87939 ) ]


# gridDaTA_0500[ ID== 5248]
# ??????????? 2171 , 3536, 3841



# ### traffic zones -------------------------------------------------------------------------------------------
#  access_map( map_grid_trzn )

#   # areas with missing info (0)
#   map_grid_trzn$ID[which(is.na(map_grid_trzn$avg_accessjobs))] %>% length()
#     
#   # manually correct centroids to closest road segment
points_trzn[ ID== 770, c('X', 'Y') := .(-43.36199,-22.97951 ) ]
points_trzn[ ID== 680, c('X', 'Y') := .(-43.26548,-22.95719 ) ]

    



# salva pontos corrigidos
  fwrite(points_0500, "R:/Dropbox/Dout/Data Dout/Spatial Grid/points_id_0500_corrected.csv")
  fwrite(points_1000, "R:/Dropbox/Dout/Data Dout/Spatial Grid/points_id_1000_corrected.csv")
  fwrite(points_2000, "R:/Dropbox/Dout/Data Dout/Spatial Grid/points_id_2000_corrected.csv")
  fwrite(points_4000, "R:/Dropbox/Dout/Data Dout/Spatial Grid/points_id_4000_corrected.csv")
  fwrite(points_trzn, "R:/Dropbox/Dout/Data Dout/Spatial Grid/points_id_trzn_corrected.csv")
  
  
  
  
  

  
  
  
