
# This script:
#  1 reads travel time-matix
#  2 calculates origing accessibility to jobs
#  3 saves accessibility results in long and wider format



# set working Directory
setwd("R:/Dropbox/Dout/Data Dout")


##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")




##################### Load Grid Data -------------------------

  gridDaTA_0500 <- fread("./Spatial Grid/gridDaTA_0500.csv")


##################### get some basic info
  
  # Total jobs, schools, hospitals, etc.
  totaljobs <- sum(gridDaTA_0500$totaljobs, na.rm = T)
  totalschools <- sum(gridDaTA_0500$schools, na.rm = T)
  totalhospitals <- sum(gridDaTA_0500$hospitals, na.rm = T)
  jobs_supmed <- sum(gridDaTA_0500$edusup, gridDaTA_0500$edumed, na.rm = T) 
  jobs_medbas <- sum(gridDaTA_0500$edumed, gridDaTA_0500$edubas, na.rm = T) 


    
#####################  Fun to get Accessibility  -------------------------
gc(reset = T)
  
  
# function to: 
#  1 read travel time-matix
#  2 calculate origing accessibility to jobs
#  3 save accessibility results in long and wider format

get_oaccess <- function(ttfile, gridDATA){
    

    # cat message
    cat("Loading tt matrix\n")
    ttmatrix <- fst::read.fst( ttfile )
    gc(reset = T)
    gc(reset = T)
    beep()
    
    
    ##### 1 Compute ORIGIN Accessibility, compute cummulative
    
    # cat message
    cat("estimating accessibility\n")
    
    
    # 1 - All accessible activities from each ORIGIN across they day
    access_intraday <- setDT(ttmatrix)[, .( decile = decile[1]
                                     , medtravel_time = median( travel_time, na.rm=T)
                                     # , oaccess_hospitals_15 = sum( hospitals[which( travel_time <= 15)], na.rm=T)
                                     # , oaccess_hospitals_30 = sum( hospitals[which( travel_time <= 30)], na.rm=T)
                                     # , oaccess_hospitals_60 = sum( hospitals[which( travel_time <= 60)], na.rm=T)
                                     # , oaccess_hospitals_90 = sum( hospitals[which( travel_time <= 90)], na.rm=T)
                                     # , oaccess_hospitals_120 = sum(hospitals[which( travel_time <= 120)], na.rm=T)
                                     # 
                                     # , oaccess_schools_15 = sum( schools[which( travel_time <= 15)], na.rm=T)
                                     # , oaccess_schools_30 = sum( schools[which( travel_time <= 30)], na.rm=T)
                                     # , oaccess_schools_60 = sum( schools[which( travel_time <= 60)], na.rm=T)
                                     # , oaccess_schools_90 = sum( schools[which( travel_time <= 90)], na.rm=T)
                                     # , oaccess_schools_120 = sum(schools[which( travel_time <= 120)], na.rm=T)
                                     
                                     , oaccess_jobs_15 = sum( totaljobs[which( travel_time <= 15)], na.rm=T)
                                     , oaccess_jobs_30 = sum( totaljobs[which( travel_time <= 30)], na.rm=T)
                                     , oaccess_jobs_60 = sum( totaljobs[which( travel_time <= 60)], na.rm=T)
                                     , oaccess_jobs_90 = sum( totaljobs[which( travel_time <= 90)], na.rm=T)
                                     , oaccess_jobs_120 = sum(totaljobs[which( travel_time <= 120)], na.rm=T)
                                     
                                     , oaccess_edubas_15 = sum( edubas[which( travel_time <= 15)], na.rm=T)
                                     , oaccess_edubas_30 = sum( edubas[which( travel_time <= 30)], na.rm=T)
                                     , oaccess_edubas_60 = sum( edubas[which( travel_time <= 60)], na.rm=T)
                                     , oaccess_edubas_90 = sum( edubas[which( travel_time <= 90)], na.rm=T)
                                     , oaccess_edubas_120 = sum(edubas[which( travel_time <= 120)], na.rm=T)
                                     
                                     , oaccess_edumed_15 = sum( edumed[which( travel_time <= 15)], na.rm=T)
                                     , oaccess_edumed_30 = sum( edumed[which( travel_time <= 30)], na.rm=T)
                                     , oaccess_edumed_60 = sum( edumed[which( travel_time <= 60)], na.rm=T)
                                     , oaccess_edumed_90 = sum( edumed[which( travel_time <= 90)], na.rm=T)
                                     , oaccess_edumed_120 = sum(edumed[which( travel_time <= 120)], na.rm=T)
                                     
                                     , oaccess_edusup_15 = sum( edusup[which( travel_time <= 15)], na.rm=T)
                                     , oaccess_edusup_30 = sum( edusup[which( travel_time <= 30)], na.rm=T)
                                     , oaccess_edusup_60 = sum( edusup[which( travel_time <= 60)], na.rm=T)
                                     , oaccess_edusup_90 = sum( edusup[which( travel_time <= 90)], na.rm=T)
                                     , oaccess_edusup_120 = sum(edusup[which( travel_time <= 120)], na.rm=T)
                                     ),
                                     by=.(year, origin, depart_time) ]

    
    
    # 2 - DAY median access by origin, year
    origin_access_med <- access_intraday[, .( decile = decile[1]
                                              , medtravel_time = median( medtravel_time, na.rm=T)
                                              # , med_oaccess_hospitals_15 = median( oaccess_hospitals_15 , na.rm=T)
                                              # , med_oaccess_hospitals_30 = median( oaccess_hospitals_30 , na.rm=T)
                                              # , med_oaccess_hospitals_60 = median( oaccess_hospitals_60 , na.rm=T)
                                              # , med_oaccess_hospitals_90 = median( oaccess_hospitals_90 , na.rm=T)
                                              # , med_oaccess_hospitals_120 =median( oaccess_hospitals_120, na.rm=T)
                                              # 
                                              # , med_oaccess_schools_15 = median( oaccess_schools_15 , na.rm=T)
                                              # , med_oaccess_schools_30 = median( oaccess_schools_30 , na.rm=T)
                                              # , med_oaccess_schools_60 = median( oaccess_schools_60 , na.rm=T)
                                              # , med_oaccess_schools_90 = median( oaccess_schools_90 , na.rm=T)
                                              # , med_oaccess_schools_120 =median( oaccess_schools_120, na.rm=T)
                                              
                                              , med_oaccess_jobs_15 = median( oaccess_jobs_15 , na.rm=T)
                                              , med_oaccess_jobs_30 = median( oaccess_jobs_30 , na.rm=T)
                                              , med_oaccess_jobs_60 = median( oaccess_jobs_60 , na.rm=T)
                                              , med_oaccess_jobs_90 = median( oaccess_jobs_90 , na.rm=T)
                                              , med_oaccess_jobs_120 =median( oaccess_jobs_120, na.rm=T)
                                              
                                              , med_oaccess_edubas_15 = median( oaccess_edubas_15 , na.rm=T)
                                              , med_oaccess_edubas_30 = median( oaccess_edubas_30 , na.rm=T)
                                              , med_oaccess_edubas_60 = median( oaccess_edubas_60 , na.rm=T)
                                              , med_oaccess_edubas_90 = median( oaccess_edubas_90 , na.rm=T)
                                              , med_oaccess_edubas_120 =median( oaccess_edubas_120, na.rm=T)
                                              
                                              , med_oaccess_edumed_15 = median( oaccess_edumed_15 , na.rm=T)
                                              , med_oaccess_edumed_30 = median( oaccess_edumed_30 , na.rm=T)
                                              , med_oaccess_edumed_60 = median( oaccess_edumed_60 , na.rm=T)
                                              , med_oaccess_edumed_90 = median( oaccess_edumed_90 , na.rm=T)
                                              , med_oaccess_edumed_120 =median( oaccess_edumed_120, na.rm=T)
                                              
                                              , med_oaccess_edusup_15 = median( oaccess_edusup_15 , na.rm=T)
                                              , med_oaccess_edusup_30 = median( oaccess_edusup_30 , na.rm=T)
                                              , med_oaccess_edusup_60 = median( oaccess_edusup_60 , na.rm=T)
                                              , med_oaccess_edusup_90 = median( oaccess_edusup_90 , na.rm=T)
                                              , med_oaccess_edusup_120 =median( oaccess_edusup_120, na.rm=T)
                                              ),
                                         by=.(origin, year)]
    
    
    head(origin_access_med) # check data structure
    
    
    
    
    # remove detailed matrix and clean memory
    rm(ttmatrix)
    gc(reset=TRUE)
    
    
# Compute Accessibility MATCH 
    # high income people = jobs with high and med education
    # low income people = jobs with low and med education
    
    for (i in c(15,30,60,90,120)) {
      a <- paste0("med_oaccess_edubas_",i)
      b <- paste0("med_oaccess_edumed_",i)
      c <- paste0("med_oaccess_edusup_",i)
      origin_access_med[, paste0("med_oaccess_jobsmatch_",i):= ifelse(decile>5, get(b) + get(c), get(b) + get(a))]
    }
    
    summary(origin_access_med$med_oaccess_jobsmatch_60)
    summary(origin_access_med$med_oaccess_jobsmatch_120)
    
    
    
########## Calculate % Accessibility as a proportion of opportunities in the city --------
    
    # for (i in c(15,30,60,90,120)) {
    #   a <- paste0("med_oaccess_hospitals_",i)
    #   origin_access_med[, paste0("prop_med_oaccess_hospitals_",i):= get(a) / totalhospitals ]
    # }
    # 
    # 
    # for (i in c(15,30,60,90,120)) {
    #   a <- paste0("med_oaccess_schools_",i)
    #   origin_access_med[, paste0("prop_med_oaccess_schools_",i):= get(a) / totalschools ]
    # }
    
    for (i in c(15,30,60,90,120)) {
      a <- paste0("med_oaccess_jobs_",i)
      origin_access_med[, paste0("prop_med_oaccess_jobs_",i):= get(a) / totaljobs ]
    }
    
    for (i in c(15,30,60,90,120)) {
      a <- paste0("med_oaccess_jobsmatch_",i)
      origin_access_med[ , paste0("prop_med_oaccess_jobsmatch_",i) := ifelse(decile>5, (get(a)/jobs_supmed) , (get(a)/jobs_medbas) )]
    }
    
    
    # distribution of access to Jobs   # max values have to be <= 100 %  
    # summary(origin_access_med$prop_med_oaccess_schools_60)
    # summary(origin_access_med$prop_med_oaccess_hospitals_60)
    summary(origin_access_med$prop_med_oaccess_jobs_60)
    summary(origin_access_med$prop_med_oaccess_jobsmatch_60)
    

    
######## MERGE Origin data to Access and SAVE ----------------
    
#>>>>> Save data, TWO Options : (1) long format with Years Stacked  , (2) wide format, where Each year is a column
    
###### (1) Years are Stacked  
    
    # select columns with characteristics of origin cells
    gridDATAorig <- gridDATA[ , .(ID, decile , X, Y, elevation, pop, income, prop_poor, unemployment, unemployment_fun, unemployment_med, unemployment_sup) ][order(ID)]
    
    
    
    # merge
    #a <- left_join(origin_access_med, gridDATAorig, by = c("origin"="ID", "decile")) %>% setDT()
    origin_access_med_stacked <- origin_access_med[ gridDATAorig, on = c("origin"="ID", "decile"), nomatch=0]
    head(origin_access_med_stacked)
    
    
    # correct name of polygon id
    setnames(origin_access_med_stacked, "origin", "ID")
    
    
    years <- table(origin_access_med_stacked$year)
    
    
  # accessibility change btwn years
    for (i in c(15,30,60,90,120)) {
      a <- paste0("prop_med_oaccess_jobsmatch_",i)

      origin_access_med_stacked[, paste0("dif_access_ratio_partial_",i) := ( get(a)[year == "2017brt_partial"] / get(a)[year == "2017mix"]), by = ID]
      origin_access_med_stacked[, paste0("dif_access_ratio_full_",i) := ( get(a)[year == "2017brt_full"] / get(a)[year == "2017mix"]), by = ID]
      origin_access_med_stacked[, paste0("dif_access_ratio_fullpart_",i) := ( get(a)[year == "2017brt_full"] / get(a)[year == "2017brt_partial"]), by = ID]
      
      
      origin_access_med_stacked[, paste0("dif_access_ratio_",i) := ifelse( year == "2017brt_partial", ( get(a)[year == "2017brt_partial"] / get(a)[year == "2017mix"]), 
                                                                   ifelse( year == "2017brt_full", ( get(a)[year == "2017brt_full"] / get(a)[year == "2017mix"]) ,
                                                                   ifelse( year == "2017mix", ( get(a)[year == "2017brt_full"] / get(a)[year == "2017brt_partial"]) , NA ))), by = ID] 
                                                                                    
                      
                      }
    
    
    
    
    
#### (1) SAVE long-stacked output oaccess  ------------------
    
    # cat message
    cat("Saving access estimtes - long format \n")
    
    fwrite(origin_access_med_stacked, "./accessibility/output_oAccess_long_500_paper4.csv")
    
    
  
    
    # checking some results 
    #tapply(origin_access_med_stacked$prop_med_oaccess_schools_60, origin_access_med_stacked$year, summary)
    tapply(origin_access_med_stacked$prop_med_oaccess_jobs_60, origin_access_med_stacked$year, summary)
    
    
    
    
###### (2) Each year is a column. We basically add the Average Accessibility info to gridDATA file
    
    # subset each year
    temp <- table(origin_access_med$year)
    
    baseline <- origin_access_med[year=="2017mix", c(1,3:39), with=F]
    partial <- origin_access_med[year=="2017brt_partial", c(1,3:39), with=F]
    full <- origin_access_med[year=="2017brt_full", c(1,3:39), with=F]
    
    # rename accessibility columns
    names(baseline)[3:38] <- paste0( names(baseline)[3:38],"_baseline")
    names(partial)[3:38] <- paste0( names(partial)[3:38],"_partial")
    names(full)[3:38] <- paste0( names(full)[3:38],"_full")
    
    # merge access variables to grid data set  
    origin_access_med_wide <- left_join(as.data.frame(gridDATA), baseline, by=c("ID"="origin", "decile")) %>% 
      left_join(., partial, by=c("ID"="origin",  "decile")) %>% 
      left_join(., full, by=c("ID"="origin",  "decile")) %>% setDT()
    
    
    
     # Remove 0 num ano e infinito na diferenca
       origin_access_med_wide <- subset(origin_access_med_wide, ID != 5445 ) # industrial areas -  fronteira nordeste
       origin_access_med_wide <- subset(origin_access_med_wide, ID != 5446 ) # industrial areas -  fronteira nordeste
       origin_access_med_wide <- subset(origin_access_med_wide, ID != 5463 ) # industrial areas -  fronteira nordeste
       origin_access_med_wide <- subset(origin_access_med_wide, ID != 4689 ) # rural area - north

           
    # Create var with Accessibility changes
    
    
    #!!!!!!!!!!!!!!!!!!! AQUI correcao para NA
    # cols <- names(origin_access_med_wide)[29:139]
    # setDT(origin_access_med_wide)[, (cols) := lapply(.SD, function(x) {ifelse(is.na(x), 0 ,x)} ), .SDcols = cols]
    ## setDT(origin_access_med_wide)[, (cols) := lapply(.SD, function(x) {ifelse( x==0, 0.001 ,x)} ), .SDcols = cols]
    ## setDT(origin_access_med_wide)[, (cols) := lapply(.SD, function(x) {ifelse( x==0, NA ,x)} ), .SDcols = cols]
    
    summary(origin_access_med_wide$med_oaccess_jobs_30_partial)
    summary(origin_access_med_wide$med_oaccess_jobs_30_full)
    
    
  #   # hospitals
  #   # absolute
  #   for (i in c(15,30,60,90,120)) {
  #     b <- paste0("med_oaccess_hospitals_",i,"_baseline")
  #     p <- paste0("med_oaccess_hospitals_",i,"_partial")
  #     f <- paste0("med_oaccess_hospitals_",i,"_full")
  #     
  #     origin_access_med_wide[, paste0("diff_ratio_oaccess_hospitals_partial_",i) := (get(p)) / (get(b)) , by= ID]
  #     origin_access_med_wide[, paste0("diff_ratio_oaccess_hospitals_full_",i) := (get(f)) / (get(b)) , by= ID]
  #     origin_access_med_wide[, paste0("diff_ratio_oaccess_hospitals_fullpart_",i) := (get(f)) / (get(p)) , by= ID]
  #     }
  #   
  #   
  # 
  #   
  # # Schools
  # # absolute
  #   for (i in c(15,30,60,90,120)) {
  #     b <- paste0("med_oaccess_schools_",i,"_baseline")
  #     p <- paste0("med_oaccess_schools_",i,"_partial")
  #     f <- paste0("med_oaccess_schools_",i,"_full")
  #     
  #     origin_access_med_wide[, paste0("diff_ratio_oaccess_schools_partial_",i) := (get(p)) / (get(b)) , by= ID]
  #     origin_access_med_wide[, paste0("diff_ratio_oaccess_schools_full_",i) := (get(f)) / (get(b)) , by= ID]
  #     origin_access_med_wide[, paste0("diff_ratio_oaccess_schools_fullpart_",i) := (get(f)) / (get(p)) , by= ID]
  #   }
  #   
    
    
  # jobsmatch
  # Absolute
    for (i in c(15,30,60,90,120)) {
      b <- paste0("med_oaccess_jobsmatch_",i,"_baseline")
      p <- paste0("med_oaccess_jobsmatch_",i,"_partial")
      f <- paste0("med_oaccess_jobsmatch_",i,"_full")
      
      origin_access_med_wide[, paste0("diff_ratio_oaccess_jobsmatch_partial_",i) := (get(p)) / (get(b)) , by= ID]
      origin_access_med_wide[, paste0("diff_ratio_oaccess_jobsmatch_full_",i) := (get(f)) / (get(b)) , by= ID]
      origin_access_med_wide[, paste0("diff_ratio_oaccess_jobsmatch_fullpart_",i) := (get(f)) / (get(p)) , by= ID]
    }
    

        
    summary( origin_access_med_wide$diff_ratio_oaccess_jobsmatch_full_60 )
    summary( origin_access_med_wide$diff_ratio_oaccess_jobsmatch_partial_60 )

        
#### (2) SAVE wide output oaccess  ------------------
    
    # cat message
    cat("Saving access estimtes - wide format \n")
    
    fwrite(origin_access_med_wide, "./accessibility/output_oAccess_wide_500_paper4.csv")
    
    # return more than one object to Global Envir.


    # get outputs stop_times and stops_edited in a list with name corresponding to year of input data
    newList <- list("oaccess_wide_" = origin_access_med_wide, "oaccess_long_"= origin_access_med_stacked)
    names(newList) <- paste0(names(newList), grid) # attach year to name of output

    # return result of function
    return(list2env(newList ,.GlobalEnv))
}

  
  
  

# file locaton
  my_ttfile <- "./accessibility/matrix_500_paper4.fst"

# Apply function
  oaccess_0500 <- get_oaccess( ttfile= my_ttfile,  gridDATA = gridDaTA_0500 )  

    
  
  
  

  
  
