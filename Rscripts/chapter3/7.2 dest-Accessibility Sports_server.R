


# This script:
# Calculates accessibility from the destination perspective - Catchment Areas by income deciles
# using different travel time thresholds (20, 60, 120min)




# set working Directory
setwd("R:/Dropbox/Dout/Data Dout")


##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")



##### 7 Compute DESTINATION Accessibility, compute cummulative -------------------------

# Load Matrix
  system.time( matrix500 <- readRDS("./accessibility/matrix500_2014-17_partial.Rds") ) # 
  rm(list=setdiff(ls(), "matrix500"))
  beep()
  gc(reset = T)


  

    

######### get SIZE of Catchment Area to STACKED data ---------------------------


# 1 - All Catchment areas of each Destination across they day
  size_CA_intraday <- matrix500[, .( medtravel_time = median( travel_time, na.rm=T)
                                    , catchm_pop15 = sum( pop[which( travel_time <= 15)], na.rm=T)
                                    , catchm_pop30 = sum( pop[which( travel_time <= 30)], na.rm=T)
                                    , catchm_pop60 = sum( pop[which( travel_time <= 60)], na.rm=T)
                                    , catchm_pop90 = sum( pop[which( travel_time <= 90)], na.rm=T)
                                    , catchm_pop120= sum( pop[which( travel_time <= 120)], na.rm=T)
                                    ),
                                by=.(year, destination, depart_time) ]

  summary(size_CA_intraday$catchm_pop60)
  summary(size_CA_intraday$catchm_pop120)
  

# 2- Median Catchment areas
  size_CA <- size_CA_intraday[, .(med_dtravel_time = median( medtravel_time, na.rm=T)
                                  , med_catchm_pop15 = median( catchm_pop15 , na.rm=T)
                                  , med_catchm_pop30 = median( catchm_pop30 , na.rm=T)
                                  , med_catchm_pop60 = median( catchm_pop60 , na.rm=T)
                                  , med_catchm_pop90 = median( catchm_pop90 , na.rm=T)
                                  , med_catchm_pop120= median( catchm_pop120, na.rm=T)
                                  ),
                              by=.(year, destination)]


head(size_CA)

# quantidade de pessoas que conseguem chegar em um lugar
summary(size_CA$med_dtravel_time)
summary(size_CA$med_catchm_pop60)
summary(size_CA$med_catchm_pop120)



# calculate relative size of catchment area in relation to city size

  # Load Grid Data e get total population of Rio
  gridDATA500hex <- fread("./Spatial Grid/gridDATA500hex.csv") # grid data
  riopop <- sum(gridDATA500hex$pop, na.rm = T)
  

  for (i in c(15,30,60,90,120)) {
                        b <- paste0("med_catchm_pop",i)
                        size_CA[, paste0("med_catchm_pop",i,"_prop"):=  get(b)/ riopop]
                        }
  

  
  # proporcao de pessoas da cidade q conseguem chegar no lugar
  summary(size_CA$med_catchm_pop15_prop) *100 # 0% - 2.7
  summary(size_CA$med_catchm_pop30_prop) *100 # 0% - 11.9%
  summary(size_CA$med_catchm_pop60_prop) *100 # 0% to 45.5%
  summary(size_CA$med_catchm_pop90_prop) *100 # 0% - 68.5%
  summary(size_CA$med_catchm_pop120_prop)*100 # 0% to 78.3%
  



# SAVE .csv access_stacked_grid500_avg ------------------
  fwrite(size_CA, file= "./accessibility/output_catchment_size_500_long.csv", showProgress=T)
  
  summary(size_CA$med_catchm_pop30_prop) * 100
  


  
  gc(reset = T)
######### get SIZE of Catchment Area to grid data ---------------------------
  
  # subset each year
  a2014 <- size_CA[year==2014, 2:13, with=F]
  a2017 <- size_CA[year==2017, 2:13, with=F]

  # rename accessibility columns
  names(a2014)[2:12] <- paste0( names(a2014)[2:12],"_2014")
  names(a2017)[2:12] <- paste0( names(a2017)[2:12],"_2017")
  
  # read grid data
    grid500_data <- fread("./Spatial Grid/gridDATA500hex.csv")
    names(grid500_data)

  

      
  # merge cathcment variables to grid data set  
    output_catchment_500 <- left_join(as.data.frame(grid500_data), a2014, by=c("idhex500"="destination")) %>%
                            left_join(., a2017, by=c("idhex500"="destination"))
  
  
  # replace NA with 0, this is important to calculate change between years of cells that apear only in one year

    cols <- names(output_catchment_500)[28:49]
    setDT(output_catchment_500)[, (cols) := lapply(.SD, function(x) {ifelse(is.na(x),0,x)} ), .SDcols = cols]
    

      
  # calculate ABSOLUTE variation in the size of the catchment area 
    
    for (i in c(15,30,60,90,120)) {
      a <- paste0("med_catchm_pop",i,"_2017")
      b <- paste0("med_catchm_pop",i,"_2014")
      output_catchment_500[, paste0("catch_diff",i,"_2017_2014") :=  get(a) - get(b) ]
      }
    
  summary(output_catchment_500$catch_diff15_2017_2014)
  summary(output_catchment_500$catch_diff30_2017_2014)
  summary(output_catchment_500$catch_diff60_2017_2014)
  summary(output_catchment_500$catch_diff90_2017_2014)
  summary(output_catchment_500$catch_diff120_2017_2014)
  
    
  
  
  
# calculate RELATIVE variation in the size of the catchment area 
  
  for (i in c(15,30,60,90,120)) {
    a <- paste0("med_catchm_pop",i,"_prop_2017")
    b <- paste0("med_catchm_pop",i,"_prop_2014")
    output_catchment_500[, paste0("catch_diff",i,"_2017_2014_prop") :=  get(a) - get(b) ]
  }

# overview
  summary(output_catchment_500$catch_diff15_2017_2014_prop *100) 
  summary(output_catchment_500$catch_diff30_2017_2014_prop *100) 
  summary(output_catchment_500$catch_diff60_2017_2014_prop *100) 
  summary(output_catchment_500$catch_diff120_2017_2014_prop *100)
  
  sum(output_catchment_500$catch_diff30_2017_2014_prop, na.rm=T) 
  sum(output_catchment_500$catch_diff60_2017_2014_prop, na.rm=T) 
  sum(output_catchment_500$catch_diff120_2017_2014_prop, na.rm=T)
# total weighted sum
  sum(output_catchment_500$catch_diff30_2017_2014_prop * output_catchment_500$pop)  
  sum(output_catchment_500$catch_diff60_2017_2014_prop * output_catchment_500$pop)  
  sum(output_catchment_500$catch_diff120_2017_2014_prop * output_catchment_500$pop) 

# SAVE .csv data Access (O\D) data ----------
  fwrite(output_catchment_500, file="./accessibility/output_gridDATA500hex_wide.csv", showProgress=T)
  

  
  
  
  
  

######### get STACKED COMPOSITION of Catchment Area
######### get STACKED COMPOSITION of Catchment Area
######### get STACKED COMPOSITION of Catchment Area ---------------------------

system.time( matrix500 <- readRDS("./accessibility/matrix500_2014-17_partial.Rds") ) # 
rm(list=setdiff(ls(), "matrix500"))
gc(reset = T)
  




# 1 - Calcula catchment area para cada origem a cada partida
# 1 - All Catchment areas of each Destination across they day for each decile
decile_CA_intraday <- matrix500[, .( medtravel_time = median( travel_time, na.rm=T)
                                     , catchm_pop15 = sum( pop[which( travel_time <= 15)], na.rm=T)
                                     , catchm_pop30 = sum( pop[which( travel_time <= 30)], na.rm=T)
                                     , catchm_pop60 = sum( pop[which( travel_time <= 60)], na.rm=T)
                                     , catchm_pop90 = sum( pop[which( travel_time <= 90)], na.rm=T)
                                     , catchm_pop120= sum( pop[which( travel_time <= 120)], na.rm=T)
                                     ),
                                 by=.(year, decile, destination, depart_time) ]


# 2 - Fica com a mediana da catchment area
# 2- day median Catchment area of each Destination and each decile
decile_CA <- decile_CA_intraday[, .(med_dtravel_time = median( medtravel_time, na.rm=T)
                                                  , med_catchm_decile15 = median( catchm_pop15 , na.rm=T)
                                                  , med_catchm_decile30 = median( catchm_pop30 , na.rm=T)
                                                  , med_catchm_decile60 = median( catchm_pop60 , na.rm=T)
                                                  , med_catchm_decile90 = median( catchm_pop90 , na.rm=T)
                                                  , med_catchm_decile120= median( catchm_pop120, na.rm=T)
                                                  ),
                                               by=.(year, decile, destination)]



# check data structure
head(decile_CA) 


# quantidade de pessoas de cada decil que conseguem chegar em um lugar
summary(decile_CA$med_catchm_decile60) # 0 - 26100 - 499500 (min, median, max)


# remove detailed matrix and clean memory
#rm(list=setdiff(ls(), c("decile_CA", "gridDATA500hex")))
gc(reset=TRUE)

# FILL with 0 the data gaps of decile groups not present with 
    # http://stackoverflow.com/questions/10438969/fastest-way-to-add-rows-for-missing-values-in-a-data-frame
    # faster http://stackoverflow.com/questions/28290919/speeding-up-join-by-group-in-data-table-r
    setkeyv(decile_CA, c("year", "destination", "decile"))
    decile_CA <- decile_CA[CJ(unique(year),unique(destination),seq(1:10))] # merge

       
    
    
  # replace NA with 0, this is important to calculate change between years of cells that apear only in one year
    cols <- names(decile_CA)[5:9]
    setDT(decile_CA)[, (cols) := lapply(.SD, function(x) {ifelse(is.na(x),0,x)} ), .SDcols = cols]
    
    
    
        
## calculate proportion of people by income decile within cathchment areas

  # Load Grid Data
  gridDATA500hex <- fread("./Spatial Grid/gridDATA500hex.csv") # grid data
  
  # get TOTALS of population in each income decile
  for ( i in 1:10) { assign(paste0("pop",i), sum(with(gridDATA500hex, pop*I(decile==i)),na.rm = T) ) }

  
# Porportion of accessible people by income in relation to all people with that income in the city
  for (i in c(15,30,60,90,120)) {
    a <- paste0("med_catchm_decile",i)
    decile_CA[ , paste0("med_catchm_decile",i,"_prop") := ifelse(decile==1, get(a)/ pop1,
                                                    ifelse(decile==2, get(a)/ pop2,
                                                           ifelse(decile==3, get(a)/ pop3,
                                                                  ifelse(decile==4, get(a)/ pop4,
                                                                         ifelse(decile==5, get(a)/ pop5,
                                                                                ifelse(decile==6, get(a)/ pop6,
                                                                                       ifelse(decile==7, get(a)/ pop7,
                                                                                              ifelse(decile==8, get(a)/ pop8,
                                                                                                     ifelse(decile==9, get(a)/ pop9,
                                                                                                            ifelse(decile==10, get(a)/ pop10, 0
                                                                                                            ))))))))))] }

head(decile_CA)


# calculate income composition in each destination (in %)
for (i in c(15,30,60,90,120)) {
                                a <- paste0("med_catchm_decile",i)
                                decile_CA[, paste0("incomecomposit",i) := get(a) / sum(get(a)), by=.(year, destination)]
                                }
                                                               
# replace NA with 0, this is important to calculate change between years of cells that apear only in one year
cols <- names(decile_CA)[15:19]
setDT(decile_CA)[, (cols) := lapply(.SD, function(x) {ifelse(is.na(x),0,x)} ), .SDcols = cols]



# check values of pop accessible # NAO PODE DAR MAIS DE 100 % 
  summary(decile_CA$med_catchm_decile60)
  
  summary(decile_CA$med_catchm_decile15_prop)
  summary(decile_CA$med_catchm_decile30_prop)
  summary(decile_CA$med_catchm_decile60_prop)
  summary(decile_CA$med_catchm_decile90_prop)
  summary(decile_CA$med_catchm_decile120_prop)
  summary(decile_CA$incomecomposit60)
  summary(decile_CA$incomecomposit120)
  
    
  
# check distribution of  to School
  qplot(x= med_catchm_decile30_prop, data = subset(decile_CA), facets = . ~ year)
  qplot(x= med_catchm_decile60_prop, data = subset(decile_CA), facets = . ~ year)
  
  
  
  
  
# SAVE .csv catchment_stacked500 ------------------
  fwrite(decile_CA, "./accessibility/output_catchment_composition_500_long.csv", showProgress=T)

  
  
  

  
######### get COMPOSITION of Catchment Area to grid ---------------------------
  
  
###### (2) Each year is a column, Allocate Average Accessibility to gridDATA
  
  # subset each year
  names(decile_CA)
  a2014 <- decile_CA[year==2014, c(2:19), with=F]
  a2017 <- decile_CA[year==2017, c(2:19), with=F]

    
  # rename accessibility columns
  names(a2014)[3:18] <- paste0( names(a2014)[3:18],"_2014")
  names(a2017)[3:18] <- paste0( names(a2017)[3:18],"_2017")


    
  # Merge data with income composition of Catchment Areas
  catchment_500 <- left_join( a2014, a2017, by=c("decile", "destination") )
  
 
  
  
  
# calculate variation in Catchment size by decile (IN ABSOLUTE NUMBERS)
  setDT(catchment_500)
  
  for (i in c(15,30,60,90,120)) {
    a <- paste0("med_catchm_decile",i,"_2017")
    b <- paste0("med_catchm_decile",i,"_2014")
    catchment_500[, paste0("diff_catchm_decile",i,"_2017_2014") :=  get(a) - get(b) ]
  }

  summary(catchment_500$diff_catchm_decile30_2017_2014) # -98700 | 107300
  summary(catchment_500$diff_catchm_decile60_2017_2014) # -186700  | 280300 
  
  
  
  
  
# calculate variation in income composition (IN PERCENTAGE POINTS)
  for (i in c(15,30,60,90,120)) {
    a <- paste0("incomecomposit",i,"_2017")
    b <- paste0("incomecomposit",i,"_2014")
    catchment_500[, paste0("diff_incomecomposit",i,"_2017_2014") := (get(a) - get(b)) *100 ]
  }
  

  summary(catchment_500$diff_incomecomposit30_2017_2014) 
  summary(catchment_500$diff_incomecomposit60_2017_2014) 
  summary(catchment_500$diff_incomecomposit120_2017_2014)
  head(catchment_500)
  
# SAVE csv data with income composition of Catchment Areas ----------
  fwrite(catchment_500, "./accessibility/output_catchment_composition_500_wide.csv", showProgress=T)
  gc(reset = T)
  
  
  
  
  
  