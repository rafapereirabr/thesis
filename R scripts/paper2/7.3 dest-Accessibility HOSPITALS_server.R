
# This script:
# Calculates accessibility from the destination side - Catchment Areas by income deciles
# using different travel time thresholds (20, 60, 120min)





# set working Directory
setwd("R:/Dropbox/Dout/Data Dout")

##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")



##### 7 Compute DESTINATION Accessibility, compute cummulative
##### 7 Compute DESTINATION Accessibility, compute cummulative -------------------------

# Load Travel Matrix
  system.time( matrix500 <- readRDS("./accessibility/matrix500_2014-17_partial.Rds") ) # 
  rm(list=setdiff(ls(), "matrix500"))
  gc(reset = T)

  




#### STACK Matrix by hierarchy level

# separate  data by hierarchy level
  aLow <- matrix500[hosp_low > 0, .(year, depart_time, origin, destination, pop, decile, hosp_low, hosp_med, hosp_high, travel_time) ]
  aMed <- matrix500[hosp_med > 0, .(year, depart_time, origin, destination, pop, decile, hosp_low, hosp_med, hosp_high, travel_time) ]
  aHigh <-matrix500[hosp_high >0, .(year, depart_time, origin, destination, pop, decile, hosp_low, hosp_med, hosp_high, travel_time) ]
  gc(reset = T)
  
  aLow[ , hierarq := "Low" ]
  aMed[ , hierarq := "Medium" ]
  aHigh[ , hierarq := "High" ]
  
  # stack them up
  matrix500_hospt <- rbind(aLow, aMed, aHigh)
  matrix500_hospt[ ,c("hosp_low","hosp_med", "hosp_high") := NULL]
  head(matrix500_hospt)
  
  # clean objects
  rm(matrix500, aLow, aMed, aHigh )
  gc(reset = T)
  
  
  matrix500_hospt <- matrix500_hospt[order(year,origin,depart_time,hierarq )]
  head(matrix500_hospt)
  
# Closest hospital/hierarchy - for every origin, keep only the trip to the closest hospital/hierarchy - minimum travel time to the nearest hospital
  matrix500_hospt2 <- matrix500_hospt[, .( travel_time = min(travel_time, na.rm = T)), by=.(year, depart_time, origin, pop, decile, hierarq)]
  

  
######## Calculate Catchment Area --------------------
  
  # 1 - All Catchment areas of each hospital across they day for each decile
  decile_CA_intraday <- matrix500_hospt2[, .( medtravel_time = median( travel_time, na.rm=T)
                                             , catchm_pop15 = sum( pop[which( travel_time < 15)], na.rm=T)
                                             , catchm_pop30 = sum( pop[which( travel_time < 30)], na.rm=T)
                                             , catchm_pop60 = sum( pop[which( travel_time < 60)], na.rm=T)
                                             , catchm_pop90 = sum( pop[which( travel_time < 90)], na.rm=T)
                                             , catchm_pop120= sum( pop[which( travel_time < 120)], na.rm=T)
                                             ),
                                         by=.(year, decile, depart_time, hierarq)]
  
  

  # Catchment area of each Hospital Hierarchy level
  matrix500_hierarq <- decile_CA_intraday[, .(med_dtravel_time = median( medtravel_time, na.rm=T)
                                            , med_catchm_decile15 = median( catchm_pop15 , na.rm=T)
                                            , med_catchm_decile30 = median( catchm_pop30 , na.rm=T)
                                            , med_catchm_decile60 = median( catchm_pop60 , na.rm=T)
                                            , med_catchm_decile90 = median( catchm_pop90 , na.rm=T)
                                            , med_catchm_decile120= median( catchm_pop120, na.rm=T)
                                            ),
                                          by=.(year, decile, hierarq)][order(year, hierarq, decile)]
  
# remove NAs in decile column
  matrix500_hierarq  <- na.omit(matrix500_hierarq)

# check data structure
head(matrix500_hierarq)




# remove detailed matrix and clean memory
rm(list=setdiff(ls(), c("matrix500_hospt", "matrix500_hierarq")))
gc(reset=TRUE)








## calculate proportion of people by income decile within cathchment areas

  # Load Grid Data
  gridDATA500hex <- fread("./Spatial Grid/gridDATA500hex.csv") # grid data
  
  # get TOTALS of population in each income decile
  for ( i in 1:10) { assign(paste0("pop",i), sum(with(gridDATA500hex,pop*I(decile==i)),na.rm = T) ) }
  
  
  # Porportion of accessible people by income in relation to all people with that income in the city
  for (i in c(15,30,60,90,120)) {
    a <- paste0("med_catchm_decile",i)
    matrix500_hierarq[ , paste0("med_catchm_decile",i,"_prop") := ifelse(decile==1, get(a)/ pop1,
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
  

# min 
summary(matrix500_hierarq$med_catchm_decile15_prop *100)  
summary(matrix500_hierarq$med_catchm_decile30_prop *100)  
summary(matrix500_hierarq$med_catchm_decile60_prop *100)  

# mediana
summary(matrix500_hierarq$med_catchm_decile15_prop *100)   
summary(matrix500_hierarq$med_catchm_decile30_prop *100)   
summary(matrix500_hierarq$med_catchm_decile60_prop *100)   

# summary por nivel de hospital
matrix500_hierarq[,list(min=min(med_catchm_decile60_prop),
                        median=median(med_catchm_decile60_prop),
                        max=max(med_catchm_decile60_prop),
                        sd=sd(med_catchm_decile60_prop)),by=hierarq]

# aqui grafico adicoinando decil no groupby
matrix500_hierarq[,list(min=min(med_catchm_decile30_prop),
                        median=median(med_catchm_decile30_prop),
                        max=max(med_catchm_decile30_prop),
                        sd=sd(med_catchm_decile30_prop)),by=.(year,hierarq)][order(hierarq)]


# calculate income composition in each destination (in %)
for (i in c(15,30,60,90,120)) {
  a <- paste0("med_catchm_decile",i)
  matrix500_hierarq[, paste0("incomecomposit",i) := get(a) / sum(get(a)), by=.(year, hierarq)] 
}





# check values of pop accessible # NAO PODE DAR MAIS DE 100 % 
  summary(matrix500_hierarq$incomecomposit15) *100 
  summary(matrix500_hierarq$incomecomposit30) *100 
  summary(matrix500_hierarq$incomecomposit60) *100 
  summary(matrix500_hierarq$incomecomposit90) *100 
  
  

  tapply(matrix500_hierarq$med_catchm_decile30_prop, matrix500_hierarq$hierarq, summary)
  
  tapply(matrix500_hierarq$incomecomposit60, matrix500_hierarq$decile, summary)
  
  

    
# SAVE .csv catchment_stacked500 ------------------
  fwrite(matrix500_hierarq, file="./accessibility/output_catchment_Hospitals_long.csv")

  

  
      

######### get COMPOSITION of Catchment Area to grid ---------------------------
  
  
###### (2) Each year is a column, Allocate Average Accessibility to gridDATA
  
  # subset each year
  names(matrix500_hierarq)
  a2014 <- matrix500_hierarq[year==2014, c(2:19), with=F]
  a2017 <- matrix500_hierarq[year==2017, c(2:19), with=F]

    
  # rename accessibility columns
  names(a2014)[3:18] <- paste0( names(a2014)[3:18],"_2014")
  names(a2017)[3:18] <- paste0( names(a2017)[3:18],"_2017")


  # Merge data with income composition of Catchment Areas
  catchment_500 <- left_join( a2014, a2017, by=c("decile", "hierarq") )
  


  
  
  
# calculate variation in access by income groups (IN ABSOLUTE NUMBERS)
  setDT(catchment_500)
  
  for (i in c(15,30,60,90,120)) {
    a <- paste0("med_catchm_decile",i,"_2017")
    b <- paste0("med_catchm_decile",i,"_2014")
    catchment_500[, paste0("diff_catchm_decile",i,"_2017_2014") :=  get(a) - get(b) ]
  }

    
  # summary of results
  summary(catchment_500$diff_catchm_decile15_2017_2014)   
  summary(catchment_500$diff_catchm_decile30_2017_2014)  
  summary(catchment_500$diff_catchm_decile60_2017_2014)  
  summary(catchment_500$diff_catchm_decile120_2017_2014) 
  

  
  
  # calculate variation in income compositions (IN PERCENTAGE POINTS)
  for (i in c(15,30,60,90,120)) {
    a <- paste0("incomecomposit",i,"_2017")
    b <- paste0("incomecomposit",i,"_2014")
    catchment_500[, paste0("diff_incomecomposit",i,"_2017_2014") := (get(a) - get(b)) *100 ]
  }

  
  #summary
  summary(catchment_500$diff_incomecomposit15_2017_2014)  
  summary(catchment_500$diff_incomecomposit30_2017_2014)  
  summary(catchment_500$diff_incomecomposit60_2017_2014)  
  summary(catchment_500$diff_incomecomposit120_2017_2014) 
  
  
  
  
  
# SAVE csv data with income composition of Catchment Areas ----------
  #fwrite(catchment_500, "./accessibility/ouput_catchment_500_Hospitals.csv")
  fwrite(catchment_500, file="./accessibility/output_catchment_Hospitals_wide.csv")
  gc(reset = T)
  
  
  

  
  