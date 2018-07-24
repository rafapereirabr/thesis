
# set working Directory
setwd("R:/Dropbox/Dout/Data Dout")



##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")

library(sp)
library(chron)
library(stplanr)
library(lubridate)
library(rgeos)




caju <- "partial"
caju <- ""


paradas_depois_caju <- c("transbra_137","transbra_138","transbra_139","transbra_140","transbra_141","transbra_142")

######## read shapefiles and reproject data -------------------

shapes_sp <- readOGR(dsn="./shapes_brt_transbrasil", layer="Trajetos_BRT__Visualizacao")      #read shape file
stops_sp <- readOGR(dsn="./shapes_brt_transbrasil", layer="Estacoes_BRT__Visualizacao")      #read shape file


#### change projection
  proj4string(shapes_sp)
  proj4string(stops_sp)

  myCRlatlong <- "+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  shapes_sp <- spTransform(shapes_sp, CRS(myCRlatlong))
  stops_sp <- spTransform(stops_sp, CRS(myCRlatlong))


  
# change stops names to avoid conflict with other GTFS stops
  stops_sp@data$OBJECTID <- paste0('transbra_',stops_sp@data$OBJECTID)
  
  
# subset BRT Transbrasil
  #transbrasil_sp <- subset(shapes_sp, Corredor == "TransBrasil")
  transbrasil_sp <- subset(shapes_sp, Nome == "TransBrasil")
  

  plot(stops_sp, col="gray")
  plot(shapes_sp, add=T)
  plot(transbrasil_sp, col="red", add=T)


#### select stops from Transbrasil

# create buffer around transbrasil
  buff_brt <- gBuffer(transbrasil_sp, width= .00286 ) # aprox. 200 meters

# select stops within buffer
  stops_brt_sp <- stops_sp[ buff_brt, ]


plot(shapes_sp)

plot(buff_brt, col="red", add=T)
plot(stops_brt_sp, col="green", add=T)


# # interactivelt view spatial oject
#   mapview::mapview(buff_brt)








######## stops.txt ----------------------  

# convert to sp to data frame
  stops_brt_df <- as.data.frame(stops_brt_sp) %>% setDT()
  head(stops_brt_df)
  
  
  
# subset and rename columns 
  stops_brt_df <- stops_brt_df[,.(OBJECTID, Nome, coords.x1, coords.x2)]
  names(stops_brt_df) <- c('stop_id','stop_name','stop_lon','stop_lat')
  
# Encoding of names
  stops_brt_df[, stop_name := as.character(stop_name)]
  Encoding(stops_brt_df$stop_name)  <- "UTF-8" 
  
  
# add empty columns
  stops_brt_df[, c('stop_code','stop_desc','zone_id','stop_url','location_type','parent_station') := NA ]
  
# reorder columns
  setcolorder(stops_brt_df, c('stop_id','stop_name','stop_lat','stop_lon','stop_code','stop_desc','zone_id','stop_url','location_type','parent_station'))
  head(stops_brt_df)

# add parent_station
  stops_brt_df[ , parent_station := ifelse(stop_id=="transbra_142", "38734844",  # Metro Uruguaiana
                                           ifelse(stop_id=="transbra_117", "9629", NA ))]     # Trem Deodoro
  
  
  
# add Transcarioca stations
 penha <-  matrix(data=c(33154600,"BRT Transcarioca - Penha I",-22.841819,-43.275043,NA,NA,NA,NA,NA,NA), nrow = 1) %>% as.data.frame()
 names(penha) <- names(stops_brt_df)
 stops_brt_df <- rbind(stops_brt_df, penha)

 fundao <-  matrix(data=c(33469197,"BRT Transcarioca - Fundão",-22.839559,-43.239663,NA,NA,NA,NA,NA,NA), nrow = 1) %>% as.data.frame()
 names(fundao) <- names(stops_brt_df)
 stops_brt_df <- rbind(stops_brt_df, fundao)
 
 
  
# lat long numeric
 stops_brt_df$stop_lat <- as.numeric( as.character(stops_brt_df$stop_lat) )
 stops_brt_df$stop_lon <- as.numeric( as.character(stops_brt_df$stop_lon) )
 
 
 
  
  
# save stops.txt
  fwrite(stops_brt_df, "./gtfs_brt_transbrasil/stops.txt")

 
  

### reorder stopds according to TransBrasil stops order  
  
  # subset of columns
  stop_sequence_all <- data.table(stop_id = as.character( stops_brt_df$stop_id),
                                  stop_name = as.character( stops_brt_df$stop_name))
  
  # add stops sequence following diagram in Figura 28 from plan
  stop_sequence_all$sequence <- c(1, 2, 3, 4, 5, 7, 8, 6, 9, 10, 11, 12, 13, 15, 17, 18, 19, 20, 21, 22, 23, 24, 27, 25, 26, 28, 16, 14 )
  
  # reorder
  stop_sequence_all <- stop_sequence_all[order(sequence)]
  
  
  
  
  
  
  
######## shape.txt ----------------------

# sample points regularly spaced over transbrasil_sp with a distance of 30 meters between them
  shapes_points <- spatialEco::sample.line(x=transbrasil_sp, type="regular", longlat=T, d=.03)
  plot(transbrasil_sp)
  plot(shapes_points, col="red", add=T)
  
  
# convert do data frame
  shapes_points_df <- as.data.frame(shapes_points) %>% setDT()
  
# chage colnames
  names(shapes_points_df) <- c('shape_id', 'shape_pt_lon', 'shape_pt_lat')
  
# create shape_id and shape_pt_sequence
  shapes_points_df[, shape_id := as.character(shape_id)][, shape_id := as.character("shapeid_transbra")]
  shapes_points_df[, shape_pt_sequence := 0:nrow(shapes_points_df)]
  
  
  
  
# calculate distances between points
  
  # Function with adapted version of geosphere::distHaversine that fits into a data.table := \\\ https://stackoverflow.com/questions/36817423/how-to-efficiently-calculate-distance-between-pair-of-coordinates-using-data-tab
  # it returns distance in meters
  dt.haversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137){
    radians <- pi/180
    lat_to <- lat_to * radians
    lat_from <- lat_from * radians
    lon_to <- lon_to * radians
    lon_from <- lon_from * radians
    dLat <- (lat_to - lat_from)
    dLon <- (lon_to - lon_from)
    a <- (sin(dLat/2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon/2)^2)
    return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
  }
  
  
# calculate distances 
  shapes_points_df[, shape_dist_traveled := dt.haversine(shape_pt_lat, shape_pt_lon, 
                                           data.table::shift(shape_pt_lat, type = "lag"), 
                                           data.table::shift(shape_pt_lon, type = "lag"))]
  
  shapes_points_df[1, shape_dist_traveled := 0] # add 0 to first point
  shapes_points_df[, shape_dist_traveled := cumsum(shape_dist_traveled)] # cumulative distance
  
  
  
# set colorder
  setcolorder(shapes_points_df, c('shape_id','shape_pt_sequence','shape_pt_lat','shape_pt_lon','shape_dist_traveled'))
  head(shapes_points_df)
  
# save shape.txt 
  fwrite(shapes_points_df, "./gtfs_brt_transbrasil/shapes.txt")
  

  
  
######## Agency.txt ----------------------
info <- paste0("Opplan2014_" ,caju)
  

  
agency_df <- data.table( agency_id = 'agency_rafapereira'
                         , agency_name = info
                         , agency_url = 'https://urbandemographics.blogspot.com/'
                         , agency_timezone = 'America/Sao_Paulo'
                        )
  
# save Agency.txt 
  fwrite(agency_df, "./gtfs_brt_transbrasil/agency.txt")
  
  
######## Calendar.txt ----------------------
  
# empty data.frame
calendar_df <- data.frame( service_id = character()
                         , monday = integer()
                         , tuesday = integer()
                         , wednesday = integer()
                         , thursday = integer()
                         , friday = integer()
                         , saturday = integer()
                         , sunday = integer()
                         , start_date = character()
                         , end_date = character(),
                         stringsAsFactors=FALSE)
  

# add servic of trip_id running on weekdays
  calendar_df[1,] <- c("transbra",1,1,1,1,1,0,0,"20170101","20190101")
  head(calendar_df)

    
  # save routes.txt 
  fwrite(calendar_df, "./gtfs_brt_transbrasil/calendar.txt")
  
  
######## routes.txt ----------------------
  
  
# Read table with services
  routes <- readxl::read_xlsx("./R scripts/paper_4_BRT_Transbrasil/BRT_operational_plan.xlsx", sheet="data")
  setDT(routes)
  
# add string to route_id
  routes[, route_id := paste0('id_', id)]
  
# calculate average speeds of each route
  routes[, avg_speed := km / (ciclo_min /60) ]
  
  
  
# create routes.txt
  routes_df <- data.table(   route_id = routes$route_id
                             , agency_id = 'agency_rafapereira'
                             , route_short_name = routes$route_id
                             , route_long_name = routes$route_long_name
                             , route_type = 3  )

  
  # save routes.txt 
  fwrite(routes_df, "./gtfs_brt_transbrasil/routes.txt")
  
  
######## Trips.txt ----------------------
  
# function to get a route_id and create a two-trip data.table
  # One trip heading towards the city center, the other towards Deodoro  
  get_trip <- function(i) {
                    data.table( route_id = rep(paste0('id_',i), 2)
                                , service_id = rep('transbra', 2)
                                , trip_id = c(paste0('tripid_',i,'_0'), paste0('tripid_',i,'_1')) 
                                , trip_headsign = c('centro', 'deodoro')
                                , direction_id = c(0,1)
                                , shape_id = rep('shapeid_transbra',2))
                                }
  
  
# create trip.txt
  trips_df <- lapply( routes$id, get_trip) %>% rbindlist()
  
  
# save Trips.txt 
  fwrite(trips_df, "./gtfs_brt_transbrasil/trips.txt")
  

  
######## Frequencies.txt ----------------------

  
# Table with morning peak headways
  frequencies_peak_morning <- data.table( route_id = trips_df$route_id
                                        , trip_id = trips_df$trip_id
                                        , start_time = c('07:00:00')
                                        , end_time = c('11:00:00')
                                        )

# get headways reported in the plan
  frequencies_peak_morning[ routes, on= "route_id", headway := i.headway ]

# convert headway to seconds
  frequencies_peak_morning[, headway := format(headway, "%H:%M:%S")]
  frequencies_peak_morning[, headway_secs := lubridate::hms(headway) %>% as.numeric() ]
  
# remove columns we won't use in the file 
  frequencies_peak_morning[, c('route_id', 'headway') := NULL]
  
# generate heads for off-peak ???  
  frequencies_df <- copy(frequencies_peak_morning)
  head(frequencies_df)

# save frequencies.txt   
  fwrite(frequencies_df, "./gtfs_brt_transbrasil/frequencies.txt")
  
  
######## stop_times.txt ----------------------
  stop_seq_1_0 <- stop_sequence_all$stop_id[c(1,2,3,4,5,7,8,10,11,12,13,15,17,18,19,20,21,22,23)]
  stop_seq_1_1 <- rev(stop_seq_1_0)
  
  stop_seq_2_0 <- stop_sequence_all$stop_id[c(1, 2, 3, 4, 5, 24)]
  stop_seq_2_1 <- rev(stop_seq_2_0)

  stop_seq_3_0 <- stop_sequence_all$stop_id[c(1, 2, 3, 4, 5, 27 )]
  stop_seq_3_1 <- rev(stop_seq_3_0)

  stop_seq_4_0 <- stop_sequence_all$stop_id[c(1, 2, 3, 4, 5, 25, 26, 28 )]
  stop_seq_4_1 <- rev(stop_seq_4_0)

  stop_seq_5_0 <- stop_sequence_all$stop_id[c(6, 7, 8, 10, 11, 12, 13, 15, 17, 18, 19, 20, 21, 22, 23 )]
  stop_seq_5_1 <- rev(stop_seq_5_0)
  
  stop_seq_6_0 <- stop_sequence_all$stop_id[c(6, 7, 8, 24)]
  stop_seq_6_1 <- rev(stop_seq_6_0)
  
  stop_seq_7_0 <- stop_sequence_all$stop_id[c(6, 7, 8, 27)]
  stop_seq_7_1 <- rev(stop_seq_7_0)
  
  stop_seq_8_0 <- stop_sequence_all$stop_id[c(6, 7, 8, 25, 26, 28)]
  stop_seq_8_1 <- rev(stop_seq_8_0)
  
  stop_seq_9_0 <- stop_sequence_all$stop_id[c(9, 10, 11, 12, 13, 15, 17, 18, 19, 20, 21, 22, 23 )]
  stop_seq_9_1 <- rev(stop_seq_9_0)
  
  stop_seq_10_0 <- stop_sequence_all$stop_id[c(9, 10, 11, 12, 13, 24)]
  stop_seq_10_1 <- rev(stop_seq_10_0)
  
  stop_seq_11_0 <- stop_sequence_all$stop_id[c(9, 10, 11, 12, 13, 27)]
  stop_seq_11_1 <- rev(stop_seq_11_0)
  
  stop_seq_12_0 <- stop_sequence_all$stop_id[c(9, 10, 11, 12, 13, 25, 26, 28)]
  stop_seq_12_1 <- rev(stop_seq_12_0)
  
  stop_seq_13_0 <- stop_sequence_all$stop_id[c(14, 17, 18, 19, 20, 21, 22, 23, 24 )]
  stop_seq_13_1 <- rev(stop_seq_13_0)
  
  stop_seq_14_0 <- stop_sequence_all$stop_id[c(14, 17, 18, 19, 20, 21, 22, 23, 27 )]
  stop_seq_14_1 <- rev(stop_seq_14_0)
  
  stop_seq_15_0 <- stop_sequence_all$stop_id[c(14, 17, 18, 19, 20, 21, 22, 23, 25, 26, 28 )]
  stop_seq_15_1 <- rev(stop_seq_15_0)
  
  stop_seq_16_0 <- stop_sequence_all$stop_id[c(14, 28 )]
  stop_seq_16_1 <- rev(stop_seq_16_0)
  
  stop_seq_17_0 <- stop_sequence_all$stop_id[c(16, 17, 18, 19, 20, 21, 22, 23 )]
  stop_seq_17_1 <- rev(stop_seq_17_0)
  
  stop_seq_18_0 <- stop_sequence_all$stop_id[c(16, 24 )]
  stop_seq_18_1 <- rev(stop_seq_18_0)
  
  stop_seq_19_0 <- stop_sequence_all$stop_id[c(16, 25, 26, 28 )]
  stop_seq_19_1 <- rev(stop_seq_19_0)
  
  
### Make changes if working on partial scenario --------------
  
  
if (caju == "partial"){   
    
  # update stop sequences
  for( i in ls(pattern="stop_seq_")){ 
                                      temp <- get(i) # get vector
                                      temp <- temp[ !(temp %in% paradas_depois_caju)] # remove stops after caju
                                      assign(i, temp) %>% return() # update vector of stops
                                    }
  # update files                               
  # remove routes that would not exist in the partial scenario
    remove(stop_seq_16_0, stop_seq_16_1, stop_seq_18_0, stop_seq_18_1, stop_seq_19_0, stop_seq_19_1)

  # save stops.txt 
    stops_brt_df <- subset(stops_brt_df, !(stop_id %in% paradas_depois_caju ))
    fwrite(stops_brt_df, "./gtfs_brt_transbrasil/stops.txt")
    
    
  # save routes.txt 
    routes_df <- subset(routes_df, !(route_id %in% c('id_16', 'id_18', 'id_19')))
    fwrite(routes_df, "./gtfs_brt_transbrasil/routes.txt")
    
  # save Trips.txt 
    trips_df <- subset(trips_df, !(route_id %in% c('id_16', 'id_18', 'id_19')))
    fwrite(trips_df, "./gtfs_brt_transbrasil/trips.txt")
    
  # save frequencies.txt  
    frequencies_df <- subset(frequencies_df,trip_id %in% trips_df$trip_id)
    fwrite(frequencies_df, "./gtfs_brt_transbrasil/frequencies.txt")
  
}



# list of all service seqs      
  ls(pattern="stop_seq_")
  
    
# function to get stop times    
  get_stoptimes <- function(i){
      
      # i = "stop_seq_9_0"
      
      id <- substr(i, 9, 13) 
      temp_seq <- get(i)
      
      stops_times_df <- data.frame(trip_id = rep(paste0('tripid',id), length(temp_seq))
                                    , stop_id = temp_seq
                                    , stop_sequence = 1:length(temp_seq)
                                    , timepoint = rep(0, length(temp_seq)) )
      
    
    
# # create stoptimes
#   stops_times_df0 <- data.frame(trip_id = rep('tripid_transbra0', length(stop_sequence_0))
#                                , stop_id = stop_sequence_0
#                                , stop_sequence = 1:length(stop_sequence_0)
#                                , timepoint = rep(0, length(stop_sequence_0))
#                                )
#     
#   stops_times_df1 <- data.frame(trip_id = rep('tripid_transbra1', length(stop_sequence_1))
#                                 , stop_id = stop_sequence_1
#                                 , stop_sequence = 1:length(stop_sequence_1)
#                                 , timepoint = rep(0, length(stop_sequence_1)))
#   
# # rbind all stoo_times
#   stops_times_df <- rbind(stops_times_df0, stops_times_df1) %>% setDT()

      
      

## add empty arrival and departure times
  setDT(stops_times_df)[, c('arrival_time', 'departure_time') := times("00:00:00")]
  head(stops_times_df)
  
  # add first departure time to each trip
    stops_times_df[, arrival_time := c(times("07:00:00"), arrival_time[-1]), by = trip_id]
    stops_times_df[, departure_time := c(times("07:01:00"), departure_time[-1]), by = trip_id]
    
    
      
# get distances between stops
  # add lat long
    stops_times_df <- left_join(stops_times_df, stops_brt_df[, .(stop_id, stop_lat, stop_lon)], by ="stop_id")
    setDT(stops_times_df)

    

    

      
  # calculate distances in km
    stops_times_df[, dist := dt.haversine(stop_lat, stop_lon, 
                                         data.table::shift(stop_lat, type = "lag"), 
                                         data.table::shift(stop_lon, type = "lag"))/1000, by=trip_id]
  
    stops_times_df[, dist := c(0L, dist[-1]), by = trip_id] # distance to first stop = 0
  
  # remove lat long
    stops_times_df[, c('stop_lat', 'stop_lon') := NULL]
    
    
# get travel times between stops in minutes
  embarcation_time = chron::times( 1 / 1440) # 1 minute in 1440 minutes in a day
  
# calculate travel_time in seconds
  
  # add route_id
    stops_times_df[trips_df, on="trip_id", route_id := i.route_id]
  # add average speed
    stops_times_df[routes, on="route_id", avg_speed := i.avg_speed]
  
  stops_times_df[, travel_time := (dist / avg_speed) * 60 ] 
  stops_times_df[, travel_time := c(0L, travel_time[-1]), by = trip_id] # travel time to first stop = 0
  
# convert seconts to time object
  stops_times_df[, travel_time := chron::times( travel_time / 1440)] # 1440 minutes in a day
  head(stops_times_df)        
  
  

# generate sample departure times 
  stops_times_df[,departure_time := cumsum(arrival_time + ifelse(travel_time==0, embarcation_time, travel_time + embarcation_time )), by = trip_id]
  stops_times_df[, arrival_time := departure_time - ifelse(travel_time == 0 , embarcation_time, embarcation_time )]
  
  return(stops_times_df)
  
}
  
  
# get all stoo_times tables
  all_stopstimes <- lapply( ls(pattern="stop_seq_") , get_stoptimes) %>% rbindlist()
  
  
  
# check total distance and total travel time by trip
  # all_stopstimes[ trip_id =="tripid_10_1"]
  all_stopstimes[, .(dist= sum(dist),
                     time= sum(travel_time)), by = trip_id] 
  
  
# final edits
  # organize columns
  all_stopstimes[, c('dist', 'route_id', 'avg_speed', 'travel_time') := NULL] # drop columns
  setcolorder(all_stopstimes,  c('trip_id', 'arrival_time', 'departure_time', 'stop_id', 'stop_sequence', 'timepoint'))
  head(all_stopstimes)
  
  # change times back to character
  all_stopstimes[, c('arrival_time', 'departure_time') := .(as.character(arrival_time), as.character(departure_time))] 
  
  
# save stop_times.txt   
  fwrite(all_stopstimes, "./gtfs_brt_transbrasil/stop_times.txt")
  
  
  
  
  
################ Zip FTGS file -------------------------

  setwd("R:/Dropbox/Dout/Data Dout/gtfs_brt_transbrasil")
  
  
# get .txt files in folder
  txt_files <-list.files(path = ".", pattern = ".txt", full.names = T)

# Save zip files
  zip(zipfile = paste0('./gtfs_brt_transbrasil_',info), files = txt_files)
  


    
  
  
  