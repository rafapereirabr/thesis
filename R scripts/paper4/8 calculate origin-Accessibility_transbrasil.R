
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
  
  
get_oaccess <- function(gridDaTA_0500){
    


    # cat message
    cat("Loading tt matrix\n")
    ttmatrix <- fst::read.fst(paste0("./accessibility/matrix_500_paper4.fst")) 
    gc(reset = T)
    gc(reset = T)
    beep()
    
    
    gc(reset = T)
    gc(reset = T)
    
    ##### 1 Compute ORIGIN Accessibility, compute cummulative
    
    # cat message
    cat("estimating accessibility\n")
    
    system.time(
    # 1 - All accessible activities from each ORIGIN across they day
    access_intraday <- setDT(ttmatrix)[, .( decile = decile[1]
                                     , medtravel_time = median( travel_time, na.rm=T)

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
    )
    
    
    gc(reset = T)
    gc(reset = T)
    
    
    
    # DAY median access by origin, year
    origin_access_med <- access_intraday[, .( decile = decile[1]
                                              , medtravel_time = median( medtravel_time, na.rm=T)

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
      a <- paste0("med_oaccess_jobs_",i)
      origin_access_med[, paste0("prop_med_oaccess_jobs_",i):= get(a) / totaljobs ]
    }
    
    for (i in c(15,30,60,90,120)) {
      a <- paste0("med_oaccess_jobsmatch_",i)
      origin_access_med[ , paste0("prop_med_oaccess_jobsmatch_",i) := ifelse(decile>5, (get(a)/jobs_supmed) , (get(a)/jobs_medbas) )]
    }
    
    
    # distribution of access to Jobs   # NAO PODE DAR MAIS DE 100 %  
    summary(origin_access_med$prop_med_oaccess_jobs_60)
    summary(origin_access_med$prop_med_oaccess_jobsmatch_60)
    

    
######## MERGE Origin data to Access and SAVE ----------------
    
    # Save data, TWO Options : (1) Years are Stacked  , (2) Each year is a column
    
    ###### (1) Years are Stacked  
    
    # select columns with characteristics of origin cells
    gridDATAorig <- gridDaTA_0500[ , .(ID, decile , X, Y, elevation, pop, income, prop_poor, unemployment, unemployment_fun, unemployment_med, unemployment_sup) ][order(ID)]
    
    
    
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

      origin_access_med_stacked[, paste0("dif_access_ratio_partial_",i) := ( get(a)[year == "partial"] / get(a)[year == "baseline"]), by = ID]
      origin_access_med_stacked[, paste0("dif_access_ratio_full_",i) := ( get(a)[year == "full"] / get(a)[year == "baseline"]), by = ID]
      origin_access_med_stacked[, paste0("dif_access_ratio_fullpart_",i) := ( get(a)[year == "full"] / get(a)[year == "partial"]), by = ID]
      
      
      origin_access_med_stacked[, paste0("dif_access_ratio_",i) := ifelse( year == "partial", ( get(a)[year == "partial"] / get(a)[year == "baseline"]), 
                                                                   ifelse( year == "full", ( get(a)[year == "full"] / get(a)[year == "baseline"]) ,
                                                                   ifelse( year == "baseline", ( get(a)[year == "full"] / get(a)[year == "partial"]) , NA ))), by = ID] 
                                                                                    
                      
                      }
    
    
    
    
    
#### SAVE long-stacked output oaccess  ------------------
    
    # cat message
    cat("Saving access estimtes - long format \n")
    
    fwrite(origin_access_med_stacked, "./accessibility/output_oAccess_long_500_paper4.csv")
    
    
  
    
    # checking some results 
    #tapply(origin_access_med_stacked$prop_med_oaccess_schools_60, origin_access_med_stacked$year, summary)
    tapply(origin_access_med_stacked$prop_med_oaccess_jobs_60, origin_access_med_stacked$year, summary)
    
    
    
    
###### (2) Each year is a column, Allocate Average Accessibility to gridDATA
    
    # subset each year
    temp <- table(origin_access_med$year)
    
    baseline <- origin_access_med[year=="baseline", c(1,3:39), with=F]
    partial <- origin_access_med[year=="partial", c(1,3:39), with=F]
    full <- origin_access_med[year=="full", c(1,3:39), with=F]
    
    # rename accessibility columns
    names(baseline)[3:38] <- paste0( names(baseline)[3:38],"_baseline")
    names(partial)[3:38] <- paste0( names(partial)[3:38],"_partial")
    names(full)[3:38] <- paste0( names(full)[3:38],"_full")
    
    # merge access variables to grid data set  
    origin_access_med_wide <- left_join(as.data.frame(gridDaTA_0500), baseline, by=c("ID"="origin", "decile")) %>% 
      left_join(., partial, by=c("ID"="origin",  "decile")) %>% 
      left_join(., full, by=c("ID"="origin",  "decile")) %>% setDT()
    
    
    
     # Remove 0 num ano e infinito na diferenca
       origin_access_med_wide <- subset(origin_access_med_wide, ID != 5445 ) # industrial areas -  fronteira nordeste
       origin_access_med_wide <- subset(origin_access_med_wide, ID != 5446 ) # industrial areas -  fronteira nordeste
       origin_access_med_wide <- subset(origin_access_med_wide, ID != 5463 ) # industrial areas -  fronteira nordeste
       origin_access_med_wide <- subset(origin_access_med_wide, ID != 4689 ) # rural area - north

           
# Create var with Accessibility changes
    
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
    
  #### SAVE wide output oaccess  ------------------
    
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

  
  
  
# Load Matrix
  oaccess_0500 <- get_oaccess( gridDaTA_0500 )  

    
  
  
  

  
  

  

#### #### #### #### ####  check results   ------------------

  # read data
  oaccess_long_0500 <- fread("./accessibility/output_oAccess_long_500_paper4.csv")
  oaccess_wide_0500 <- fread("./accessibility/output_oAccess_wide_500_paper4.csv")
  
  
  
  
  
summary(oaccess_wide_0500$prop_med_oaccess_jobsmatch_60_baseline) 
summary(oaccess_wide_0500$prop_med_oaccess_jobsmatch_60_partial)
summary(oaccess_wide_0500$prop_med_oaccess_jobsmatch_60_full)





#how many cases of lost access to hospitals
  # jobs match
  plot(oaccess_wide_0500$diff_minus_oaccess_jobsmatch_60, col = ifelse(oaccess_wide_0500$diff_minus_oaccess_jobsmatch_60 < 0,'red','blue'), pch = 19 )
  plot(oaccess_wide_0500$diff_ratio_oaccess_jobsmatch_60, col = ifelse(oaccess_wide_0500$diff_ratio_oaccess_jobsmatch_60 < 1,'red','blue'), pch = 19 )


# distribution of access to
library(ggjoy)
      
    # jobs
  
  test <- subset(oaccess_wide_0500, diff_ratio_oaccess_jobsmatch_partial_60 != 1)
     
  ggplot(na.omit(test, cols=c('decile', 'diff_ratio_oaccess_jobsmatch_partial_60')), aes(x = diff_ratio_oaccess_jobsmatch_partial_60, y = factor(decile), fill = factor(decile))) +
        geom_joy(scale = 4) + theme_joy() +
        scale_fill_brewer(palette = "RdBu", name = "Income Decile") +
        scale_y_discrete(expand = c(0.01, 0)) +
        scale_x_continuous(expand = c(0, 0)) +
        geom_vline(xintercept=0) 

                
      


##### check plot results -------------

# jobs
ggplot(oaccess_wide_0500, aes(x=prop_med_oaccess_jobsmatch_60_baseline, 
                               y=prop_med_oaccess_jobsmatch_60_partial), label = idhex500) + 
  geom_point(aes( color=factor(decile), size=pop), alpha = 0.4) +
  scale_colour_brewer(palette="RdBu") +
  geom_abline() +
  coord_equal()

      
      
      ggplot(oaccess_wide_0500, aes(x=prop_med_oaccess_jobsmatch_60_baseline, 
                                         y=prop_med_oaccess_jobsmatch_60_full), label = idhex500) + 
        geom_point(aes( color=factor(decile), size=pop), alpha = 0.4) +
        scale_colour_brewer(palette="RdBu") +
        geom_abline() +
        coord_equal()
      
      
      ggplot() + 
        geom_point(data=oaccess_wide_0500, aes( x=prop_med_oaccess_jobsmatch_60_baseline, y=prop_med_oaccess_jobsmatch_60_full , size=pop), color="gray", alpha = 0.4) +
       geom_point(data=oaccess_wide_0500, aes( x=prop_med_oaccess_jobsmatch_60_baseline, y=prop_med_oaccess_jobsmatch_60_partial, size=pop, color=factor(decile)), alpha = 0.4) +
        scale_colour_brewer(palette="RdBu") +
        geom_abline() +
        coord_equal()
      
      
      
      
      
      

#### test  MAPS ---------
      
        
        
      # read maps
      muni <- st_read(dsn = './Shapes_IBGE', layer ='muni')
      hex_0500 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_0500')
      
      # Merge access data with sf files  
      hexriopoly500_long  <- left_join( hex_0500 , oaccess_long_0500, by= "ID" )  
      hexriopoly500_wide  <- left_join( hex_0500 , oaccess_wide_0500, by= "ID" )  
      
      
      

# remove basesile scenario
      hexriopoly500_long <- subset(hexriopoly500_long, year != "baseline")
      
      
      # Map difference RATIO
      dif_access_map_ratio <- function(var){  
                                                    ggplot(data= subset(hexriopoly500_long, !is.na(get(var))) ) +
                                                      geom_sf(data= muni,  fill=NA, color="gray70") +
                                                      geom_sf( aes(fill= (get(var)-1)*100, label=ID ), color=NA) +
                                                      scale_fill_gradient2( low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 1, space = "Lab", na.value = "grey50", guide = "colourbar") +
                                                    #  scale_fill_distiller(palette = "Spectral", midpoint = 1) +
                                                      theme_map() +
                                                      facet_wrap(~year, ncol =2)
                                                  }
      
dif_access_map_ratio('dif_access_ratio_30' )
dif_access_map_ratio('dif_access_ratio_60' )
dif_access_map_ratio('dif_access_ratio_90' )
dif_access_map_ratio('dif_access_ratio_120' )
beep()

  ggplotly()
      
  
  ggplot(data= subset(hexriopoly500_long, dif_access_ratio_120<1) ) +
    geom_sf(data= muni,  fill=NA, color="gray70") +
    geom_sf( aes(fill= (dif_access_ratio_120-1)*100, label=ID ), color=NA) +
    scale_fill_gradient2( low = "green", mid = "#f7f7f7", high = "#b2182b", midpoint = 1, space = "Lab", na.value = "grey50", guide = "colourbar") +
    theme_map() +
    facet_wrap(~year, ncol =1)
  
  
      # IDs que tem access muito baixo em 2017mix
      # rodar novamente matrizes de 2017
      # rever resultados

      oaccess_long_0500[ ID == 809, .(ID, year, prop_med_oaccess_jobs_30)]
      oaccess_long_0500[ ID == 721, .(ID, year, prop_med_oaccess_jobs_30)]
      oaccess_long_0500[ ID == 2417, .(ID, year, prop_med_oaccess_jobs_30)]
      oaccess_long_0500[ ID == 2296, .(ID, year, prop_med_oaccess_jobs_30)]
      
      

            
      
      
      dif_access_map_ratio('dif_access_ratio_60' )
      dif_access_map_ratio('dif_access_ratio_90' )
      dif_access_map_ratio('dif_access_ratio_120' )
      beep()
      
      
      
      
      
 # Map relative access
relat_access_map <- function(var){  
        ggplot(data= subset(hexriopoly500_long, !is.na(get(var))) ) +
          geom_sf(data= muni,  fill=NA, color="gray70") +
          geom_sf( aes(fill= get(var)*100, label=ID ), color=NA) +
          scale_fill_gradient2( low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 1, space = "Lab", na.value = "grey50", guide = "colourbar") +
          theme_map() +
          facet_wrap(~year, ncol =2)
      }
      
      
      
relat_access_map('prop_med_oaccess_jobsmatch_30' )
relat_access_map('prop_med_oaccess_jobsmatch_60' )
relat_access_map('prop_med_oaccess_jobsmatch_90' )
relat_access_map('prop_med_oaccess_jobsmatch_120' )
beep()


      
      

      ggplot(data= subset(hexriopoly500_wide, !is.na(diff_ratio_oaccess_jobsmatch_fullpart_30)) ) +
        geom_sf(data= muni,  fill=NA, color="gray70") +
        geom_sf( aes(fill= (diff_ratio_oaccess_jobsmatch_fullpart_30-1)*100, label=ID ), color=NA) +
        scale_fill_gradient2( low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 1, space = "Lab", na.value = "grey50", guide = "colourbar") +
        theme_map()
      
      
      beep()
