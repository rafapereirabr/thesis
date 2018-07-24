
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



##################### Load Grid Data -------------------------

  gridDaTA_0200 <- fread("./Spatial Grid/gridDaTA_0200.csv")
  gridDaTA_0500 <- fread("./Spatial Grid/gridDaTA_0500.csv")
  gridDaTA_1000 <- fread("./Spatial Grid/gridDaTA_1000.csv")
  gridDaTA_2000 <- fread("./Spatial Grid/gridDaTA_2000.csv")
  gridDaTA_4000 <- fread("./Spatial Grid/gridDaTA_4000.csv")
  gridDaTA_trzn <- fread("./Spatial Grid/gridDaTA_trzn.csv")


##################### get some basic info
  
  # Total jobs, schools, hospitals, etc.
  totaljobs <- sum(gridDaTA_0200$totaljobs, na.rm = T)
  totalschools <- sum(gridDaTA_0200$schools, na.rm = T)
  totalhospitals <- sum(gridDaTA_0200$hospitals, na.rm = T)
  jobs_supmed <- sum(gridDaTA_0200$edusup, gridDaTA_0200$edumed, na.rm = T) 
  jobs_medbas <- sum(gridDaTA_0200$edumed, gridDaTA_0200$edubas, na.rm = T) 



  # file names
  tt_0500 <- "matrix_0500"
  tt_1000 <- "matrix_1000"
  tt_2000 <- "matrix_2000"
  tt_4000 <- "matrix_4000"
  tt_trzn <- "matrix_trzn"

  

    
#####################  Fun to get Accessibility  -------------------------

 
  
get_oaccess <- function(ttfile, gridDATA){
    
    
    # get grid pre-fix
    grid <- substr(ttfile, 8, 20)
    cat("Working on grid ", grid,"\n")
    
    
    # cat message
    cat("Loading tt matrix\n")
    ttmatrix <- read_rds(paste0("./accessibility/",ttfile,"_201404_2017mix_partial_newtrains.Rds"))
    gc(reset = T)
    
    
    
    ##### 1 Compute ORIGIN Accessibility, compute cummulative
    
    # cat message
    cat("estimating accessibility\n")
    
    
    # 1 - All accessible activities from each ORIGIN across they day
    access_intraday <- ttmatrix[, .( decile = decile[1]
                                     , medtravel_time = median( travel_time, na.rm=T)
                                     , oaccess_hospitals_15 = sum( hospitals[which( travel_time <= 15)], na.rm=T)
                                     , oaccess_hospitals_30 = sum( hospitals[which( travel_time <= 30)], na.rm=T)
                                     , oaccess_hospitals_60 = sum( hospitals[which( travel_time <= 60)], na.rm=T)
                                     , oaccess_hospitals_90 = sum( hospitals[which( travel_time <= 90)], na.rm=T)
                                     , oaccess_hospitals_120 = sum(hospitals[which( travel_time <= 120)], na.rm=T)
                                     
                                     , oaccess_schools_15 = sum( schools[which( travel_time <= 15)], na.rm=T)
                                     , oaccess_schools_30 = sum( schools[which( travel_time <= 30)], na.rm=T)
                                     , oaccess_schools_60 = sum( schools[which( travel_time <= 60)], na.rm=T)
                                     , oaccess_schools_90 = sum( schools[which( travel_time <= 90)], na.rm=T)
                                     , oaccess_schools_120 = sum(schools[which( travel_time <= 120)], na.rm=T)
                                     
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
    
    
    
    
    # DAY median access by origin, year
    origin_access_med <- access_intraday[, .( decile = decile[1]
                                              , medtravel_time = median( medtravel_time, na.rm=T)
                                              , med_oaccess_hospitals_15 = median( oaccess_hospitals_15 , na.rm=T)
                                              , med_oaccess_hospitals_30 = median( oaccess_hospitals_30 , na.rm=T)
                                              , med_oaccess_hospitals_60 = median( oaccess_hospitals_60 , na.rm=T)
                                              , med_oaccess_hospitals_90 = median( oaccess_hospitals_90 , na.rm=T)
                                              , med_oaccess_hospitals_120 =median( oaccess_hospitals_120, na.rm=T)
                                              
                                              , med_oaccess_schools_15 = median( oaccess_schools_15 , na.rm=T)
                                              , med_oaccess_schools_30 = median( oaccess_schools_30 , na.rm=T)
                                              , med_oaccess_schools_60 = median( oaccess_schools_60 , na.rm=T)
                                              , med_oaccess_schools_90 = median( oaccess_schools_90 , na.rm=T)
                                              , med_oaccess_schools_120 =median( oaccess_schools_120, na.rm=T)
                                              
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
    
    for (i in c(15,30,60,90,120)) {
      a <- paste0("med_oaccess_hospitals_",i)
      origin_access_med[, paste0("prop_med_oaccess_hospitals_",i):= get(a) / totalhospitals ]
    }
    
    
    for (i in c(15,30,60,90,120)) {
      a <- paste0("med_oaccess_schools_",i)
      origin_access_med[, paste0("prop_med_oaccess_schools_",i):= get(a) / totalschools ]
    }
    
    for (i in c(15,30,60,90,120)) {
      a <- paste0("med_oaccess_jobs_",i)
      origin_access_med[, paste0("prop_med_oaccess_jobs_",i):= get(a) / totaljobs ]
    }
    
    for (i in c(15,30,60,90,120)) {
      a <- paste0("med_oaccess_jobsmatch_",i)
      origin_access_med[ , paste0("prop_med_oaccess_jobsmatch_",i) := ifelse(decile>5, (get(a)/jobs_supmed) , (get(a)/jobs_medbas) )]
    }
    
    
    # distribution of access to Jobs   # NAO PODE DAR MAIS DE 100 %  
    summary(origin_access_med$prop_med_oaccess_schools_60)
    summary(origin_access_med$prop_med_oaccess_hospitals_60)
    summary(origin_access_med$prop_med_oaccess_jobs_60)
    summary(origin_access_med$prop_med_oaccess_jobsmatch_60)
    

    
######## MERGE Origin data to Access and SAVE ----------------
    
    # Save data, TWO Options : (1) Years are Stacked  , (2) Each year is a column
    
    ###### (1) Years are Stacked  
    
    # select columns with characteristics of origin cells
    gridDATAorig <- gridDATA[ , .(ID, grid, decile , X, Y, elevation, pop, income, prop_poor, unemployment, unemployment_fun, unemployment_med, unemployment_sup) ][order(ID)]
    gc(reset = T)
    
    # merge
    #a <- left_join(origin_access_med, gridDATAorig, by = c("origin"="ID", "decile")) %>% setDT()
    origin_access_med_stacked <- origin_access_med[ gridDATAorig, on = c("origin"="ID", "decile"), nomatch=0]
    head(origin_access_med_stacked)
    
    
    # correct name of polygon id
    setnames(origin_access_med_stacked, "origin", "ID")
    
    
    years <- table(origin_access_med_stacked$year)
    
    
    

    
#### SAVE long-stacked output oaccess  ------------------
    
    # cat message
    cat("Saving access estimtes - long format \n")
    
    fwrite(origin_access_med_stacked, paste0("./accessibility/output_oAccess_long_",grid,"_",names(years)[1],"_",names(years)[2],".csv"))
    
    
  
    
    # checking some results 
    tapply(origin_access_med_stacked$prop_med_oaccess_schools_60, origin_access_med_stacked$year, summary)
    tapply(origin_access_med_stacked$prop_med_oaccess_jobs_60, origin_access_med_stacked$year, summary)
    
    
    
    
    ###### (2) Each year is a column, Allocate Average Accessibility to gridDATA
    
    # subset each year
    temp <- table(origin_access_med$year)
    a2014 <- origin_access_med[year==names(temp)[1], c(1,3:59), with=F]
    a2017 <- origin_access_med[year==names(temp)[2], c(1,3:59), with=F]
    
    # rename accessibility columns
    names(a2014)[3:58] <- paste0( names(a2014)[3:58],"_2014")
    names(a2017)[3:58] <- paste0( names(a2017)[3:58],"_2017")
    
    # merge access variables to grid data set  
    origin_access_med_wide <- left_join(as.data.frame(gridDATA), a2014, by=c("ID"="origin", "decile")) %>% 
                              left_join(., a2017, by=c("ID"="origin",  "decile")) %>% setDT()
    
    
    
     # Remove 0 num ano e infinito na diferenca
     if(ttfile=="matrix_0500"){
       origin_access_med_wide <- subset(origin_access_med_wide, ID != 5445 ) # industrial areas -  fronteira nordeste
       origin_access_med_wide <- subset(origin_access_med_wide, ID != 5446 ) # industrial areas -  fronteira nordeste
       origin_access_med_wide <- subset(origin_access_med_wide, ID != 5463 ) # industrial areas -  fronteira nordeste
       origin_access_med_wide <- subset(origin_access_med_wide, ID != 4689 ) # rural area - north
     }
     if(ttfile=="matrix_1000"){
       origin_access_med_wide <- subset(origin_access_med_wide, ID != 1436 ) # industrial areas -  fronteira nordeste
       origin_access_med_wide <- subset(origin_access_med_wide, ID != 1240 ) # industrial areas -  fronteira nordeste
     }
    
    
   # Create var with Accessibility changes
    
    # hospitals
    # absolute
    for (i in c(15,30,60,90,120)) {
      a <- paste0("med_oaccess_hospitals_",i,"_2017")
      b <- paste0("med_oaccess_hospitals_",i,"_2014")
      origin_access_med_wide[, paste0("diff_ratio_oaccess_hospitals_",i) := (get(a)) / (get(b)) , by= ID]
    }
    # proportion
    for (i in c(15,30,60,90,120)) {
      a <- paste0("prop_med_oaccess_hospitals_",i,"_2017")
      b <- paste0("prop_med_oaccess_hospitals_",i,"_2014")
      origin_access_med_wide[, paste0("diff_minus_oaccess_hospitals_",i):= (get(a) - get(b))*100 , by= ID]
    }
    
    # Schools
    # absolute
    for (i in c(15,30,60,90,120)) {
      a <- paste0("med_oaccess_schools_",i,"_2017")
      b <- paste0("med_oaccess_schools_",i,"_2014")
      origin_access_med_wide[, paste0("diff_ratio_oaccess_schools_",i):= (get(a)) / (get(b)) , by= ID]
    }
    # proportion
    for (i in c(15,30,60,90,120)) {
      a <- paste0("prop_med_oaccess_schools_",i,"_2017")
      b <- paste0("prop_med_oaccess_schools_",i,"_2014")
      origin_access_med_wide[, paste0("diff_minus_oaccess_schools_",i):= (get(a) - get(b))*100, by= ID]
    }
    # jobsmatch
    # Absolute
    for (i in c(15,30,60,90,120)) {
      a <- paste0("med_oaccess_jobsmatch_",i,"_2017")
      b <- paste0("med_oaccess_jobsmatch_",i,"_2014")
      origin_access_med_wide[, paste0("diff_ratio_oaccess_jobsmatch_",i):= (get(a)) / (get(b)) , by= ID]
    }
    # proportion
    for (i in c(15,30,60,90,120)) {
      a <- paste0("prop_med_oaccess_jobsmatch_",i,"_2017")
      b <- paste0("prop_med_oaccess_jobsmatch_",i,"_2014")
      origin_access_med_wide[, paste0("diff_minus_oaccess_jobsmatch_",i):= (get(a) - get(b))*100 , by= ID]
    }
    
    
    a <- origin_access_med_wide$diff_minus_oaccess_jobsmatch_120 #%>% as.numeric()
    b <- (origin_access_med_wide$prop_med_oaccess_jobsmatch_120_2017 - origin_access_med_wide$prop_med_oaccess_jobsmatch_120_2014) *100 #%>% as.numeric()
    
    all.equal(a,b)
    c <-  setdiff(a,b)
    
    
    
    #### SAVE wide output oaccess  ------------------
    
    # cat message
    cat("Saving access estimtes - wide format \n")
    
    fwrite(origin_access_med_wide, paste0("./accessibility/output_oAccess_wide_",grid,"_",names(years)[1],"_",names(years)[2],".csv"))
 
    # return more than one object to Global Envir.


    # get outputs stop_times and stops_edited in a list with name corresponding to year of input data
    newList <- list("oaccess_wide_" = origin_access_med_wide, "oaccess_long_"= origin_access_med_stacked)
    names(newList) <- paste0(names(newList), grid) # attach year to name of output

    # return result of function
    return(list2env(newList ,.GlobalEnv))
}

  
  
  
# Load Matrix
  # gridDATA <- gridDaTA_4000
  # ttfile <- tt_4000

  oaccess_0500 <- get_oaccess( ttfile= tt_0500,  gridDATA = gridDaTA_0500 )  
  oaccess_1000 <- get_oaccess( ttfile= tt_1000,  gridDATA = gridDaTA_1000 )     
  oaccess_2000 <- get_oaccess( ttfile= tt_2000,  gridDATA = gridDaTA_2000 )     
  oaccess_4000 <- get_oaccess( ttfile= tt_4000,  gridDATA = gridDaTA_4000 )     
  oaccess_trzn <- get_oaccess( ttfile= tt_trzn,  gridDATA = gridDaTA_trzn )           
  
  
  
  

  
  

  

#### #### #### #### ####  check results   ------------------

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  origin_access_med_wide <- copy(oaccess_wide_0500)
  
  
summary(origin_access_med_wide$prop_med_oaccess_jobsmatch_60_2014) 
summary(origin_access_med_wide$prop_med_oaccess_jobsmatch_60_2017) 
summary(origin_access_med_wide$diff_minus_oaccess_jobsmatch_60)
summary(origin_access_med_wide$diff_ratio_oaccess_jobsmatch_60) 

# origin_access_med_wide[ diff_ratio_oaccess_jobsmatch_60 > 1689700, .( ID, prop_med_oaccess_jobsmatch_60_2014, prop_med_oaccess_jobsmatch_60_2017, diff_minus_oaccess_jobsmatch_60, med_oaccess_jobsmatch_60_2014,med_oaccess_jobsmatch_60_2017, diff_ratio_oaccess_jobsmatch_60)]
# origin_access_med_wide[ ID =="5347", .( ID, prop_med_oaccess_jobsmatch_60_2014, prop_med_oaccess_jobsmatch_60_2017, diff_minus_oaccess_jobsmatch_60, med_oaccess_jobsmatch_60_2014,med_oaccess_jobsmatch_60_2017, diff_ratio_oaccess_jobsmatch_60)]


#how many cases of lost access to hospitals
  # jobs match
  plot(origin_access_med_wide$diff_minus_oaccess_jobsmatch_60, col = ifelse(origin_access_med_wide$diff_minus_oaccess_jobsmatch_60 < 0,'red','blue'), pch = 19 )
  plot(origin_access_med_wide$diff_ratio_oaccess_jobsmatch_60, col = ifelse(origin_access_med_wide$diff_ratio_oaccess_jobsmatch_60 < 1,'red','blue'), pch = 19 )
  # hopsital
  plot(origin_access_med_wide$diff_minus_oaccess_hospitals_60, col = ifelse(origin_access_med_wide$diff_minus_oaccess_hospitals_60 < 0,'red','blue'), pch = 19 )


# distribution of access to
library(ggjoy)
      
    # jobs
      ggplot(na.omit(origin_access_med_wide, cols=c('decile', 'diff_minus_oaccess_jobsmatch_60')), aes(x = diff_minus_oaccess_jobsmatch_60, y = factor(decile), fill = factor(decile))) +
        geom_joy(scale = 4) + theme_joy() +
        scale_fill_brewer(palette = "RdBu", name = "Income Decile") +
        scale_y_discrete(expand = c(0.01, 0)) +
        scale_x_continuous(expand = c(0, 0)) +
        geom_vline(xintercept=0)
      
      # schools
      ggplot(na.omit(origin_access_med_wide, cols=c('decile', 'diff_minus_oaccess_schools_60')), aes(x = diff_minus_oaccess_schools_60, y = factor(decile), fill = factor(decile))) +
        geom_joy(scale = 4) + theme_joy() +
        scale_fill_brewer(palette = "RdBu", name = "Income Decile") +
        scale_y_discrete(expand = c(0.01, 0)) +
        scale_x_continuous(expand = c(0, 0)) +
        geom_vline(xintercept=0)
      
      
      # hospitals
      ggplot(na.omit(origin_access_med_wide, cols=c('decile', 'diff_minus_oaccess_hospitals_60')), aes(x = diff_minus_oaccess_hospitals_60, y = factor(decile), fill = factor(decile))) +
        geom_joy(scale = 4) + theme_joy() +
        scale_fill_brewer(palette = "RdBu", name = "Income Decile") +
        scale_y_discrete(expand = c(0.01, 0)) +
        scale_x_continuous(expand = c(0, 0)) +
        geom_vline(xintercept=0)
      


##### check plot results -------------

# jobs
ggplot(origin_access_med_wide, aes(x=prop_med_oaccess_jobsmatch_60_2014, 
                               y=prop_med_oaccess_jobsmatch_60_2017), label = idhex500) + 
  geom_point(aes( color=factor(decile), size=pop), alpha = 0.4) +
  scale_colour_brewer(palette="RdBu") +
  geom_abline() +
  coord_equal()

# schools        
ggplot(origin_access_med_wide, aes(x=prop_med_oaccess_schools_60_2014,
                               y=prop_med_oaccess_schools_60_2017), label = idhex500) + 
  geom_point(aes( color=factor(decile), size=pop), alpha = 0.4) +
  scale_colour_brewer(palette="RdBu") +
  theme_minimal() +
  geom_abline()

# hospitals        
ggplot(origin_access_med_wide, aes(x=prop_med_oaccess_hospitals_60_2014, 
                               y=prop_med_oaccess_hospitals_60_2017), label = idhex500) + 
  geom_point(aes( color=factor(decile), size=pop), alpha = 0.4) +
  scale_colour_brewer(palette="RdBu") +
  theme_minimal() +
  geom_abline()


#ggplotly()





