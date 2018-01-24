# This script: Reads and binds several Travel-time Matrices
  
  
  
  
# set working Directory
  setwd("R:/Dropbox/Dout/Data Dout")




##################### Load packages -------------------------------------------------------

# source("./R scripts/00_LoadPackages.R")

library(data.table) # to manipulate data frames (read.csv is ultrafast for reading CSV files)
library(ggplot2)    # to make charts and maps
library(sf)
library(Hmisc)        # to compute weighted decile using wtd.quantile
library(dplyr)      # to manipulate data frames
library(tidyr)      # to manipulate data frames
library(magrittr)   # Pipe operations
library(beepr)      # Beeps at the end of the command
library(bit64)
library(readr)
library(ggthemes)
library(parallel)
library(snow)
library(viridis)
library(RColorBrewer)
library(fst)


########## 1 Get a List of all OD Matrix files -------------------------------------------------------


# create fucntion to read OTP outputs and save a single travel-time matrix
  save_traveltime_matrix <- function(grid, data_file, id, conunterfactual){
  

  
  # pattern of string to read files of deaprture time for each grid scale
  pattern = paste0('traveltime_matrix_', grid)
  

  
# get all files of each departure time into a list
  cat("Reading travel-time matrices \n")

  # baseline scenario
    filenames2017 <- list.files("R:/Dropbox/OpenTripPlanner/jython_rio_2017mix", pattern= pattern, full.names=TRUE) 
    
    
  # policy scenarios
    full_scenario <- list.files("R:/Dropbox/OpenTripPlanner/jython_rio_transbrasil_opplan2014", pattern= pattern, full.names=TRUE)
    partial_scenario <- list.files("R:/Dropbox/OpenTripPlanner/jython_rio_transbrasil_opplan2014_partial", pattern= pattern, full.names=TRUE)
    
    # full_scenario <- full_scenario[4:5]
    # partial_scenario <- partial_scenario[4:5]
    policy_files <- c(full_scenario, partial_scenario)
    

  # check the number of cores
  no_cores <- parallel::detectCores() - 1
  cat(paste("using", no_cores, "cores \n"))
  

##########  3.0 read basline matrix
  
  gc(reset = T)
  baseline <- lapply(filenames2017, fread) %>% rbindlist()
  
  # add freq column and reorder columns
  baseline[, freq := "baseline"]
  setcolorder(baseline, c("year", "freq", "depart_time", "origin", "destination", "walk_distance", "travel_time"))
  
  
  
########## 3.1 Read policy matrices
  
  policy_matrices <- lapply(policy_files, fread) %>% rbindlist()
  gc(reset = T)
  
  
########## 3.2 Rbind all matrices
  
  ttmatrix <- rbindlist(list(policy_matrices, baseline))
  head(ttmatrix)
  
  rm(policy_matrices, baseline)
  gc(reset = T)
  
  gc(reset=TRUE)
  gc(reset=TRUE)
  gc(reset=TRUE)
  # beep()
  
  
  
  # Remove 0 num ano e infinito na diferenca
    ttmatrix <- subset(ttmatrix, origin != 5445 ) # industrial areas -  fronteira nordeste
    ttmatrix <- subset(ttmatrix, origin != 5446 ) # industrial areas -  fronteira nordeste
    ttmatrix <- subset(ttmatrix, origin != 5463 ) # industrial areas -  fronteira nordeste
    ttmatrix <- subset(ttmatrix, origin != 4689 ) # rural area - north

  
  
  
  
  # convert time to minutes
  ttmatrix[ , travel_time := travel_time/60]
  summary(ttmatrix$travel_time)
  head(ttmatrix)
  
  
  ## Convert year
  years <- table(ttmatrix$year)
  
  
  
  
  # read jobs data
  grid_data <- fread(paste0("./Spatial Grid/",data_file,".csv"))
  #grid_data <- grid_data[, lapply(.SD, as.numeric)] # all numeric columns
  grid_data <-   grid_data[pop > 0 | totaljobs > 0 | hospitals > 0 | schools > 0,] # only cells with pop+hospitals+schools
  
  grid_data_dest <- grid_data[, .(ID, hospitals, hosp_low, hosp_med, hosp_high, schools, totaljobs, edubas, edumed, edusup)]
  grid_data_orig <- grid_data[, .(ID, grid, pop, income, decile, prop_poor)]
  gc(reset=TRUE)
  
  
# Merge job count with OD Matrix, allocating job counts to Destination
  cat("Merging data to tt matrices  \n")
  
  # merge data using DATA.TABLE (faster)
  # origin
  gc(reset=TRUE)
  gc(reset=TRUE)
  gc(reset=TRUE)
#  ttmatrix <- ttmatrix[grid_data_orig, on=c('origin'='ID'), nomatch=0]
  ttmatrix[grid_data_orig, on=c('origin'='ID'), c("pop", "decile") := list(i.pop, i.decile)]
  
  
  rm(grid_data_orig)
  gc(reset=TRUE)
  gc(reset=TRUE)
  gc(reset=TRUE)
  
  # destination
    # merga_all>>> ttmatrix <- ttmatrix[grid_data_dest, on=c('destination'='ID'), nomatch=0]
    ttmatrix[grid_data_dest, on=c('destination'='ID'), c("totaljobs", "edubas", "edumed", "edusup") := list(i.totaljobs, i.edubas, i.edumed, i.edusup)]
  
  # clean memory
  rm(grid_data_dest)
  gc(reset=TRUE)
  gc(reset=TRUE)
  gc(reset=TRUE)
  
  # beep()
  
  head(ttmatrix)
  
  
  
  # Save Matrix ~ 164
  cat("Saving travel-time matrices \n")
  cat("file size (MB): ", object.size(ttmatrix)/1000000, "\n" )
  
  
   system.time (  fst::write.fst(ttmatrix, path="./accessibility/matrix_500_paper4.fst") )
   # 181.71 sec
  
  # system.time (  fwrite(ttmatrix, paste0("./accessibility/matrix_",grid,"_",names(years)[1],"_",names(years)[2],"_partial_newtrains.csv") ) )

  
  return(ttmatrix)
  #beep()
  gc(reset=TRUE)
}



matrix_0500 <- save_traveltime_matrix(grid='500', data_file='gridDaTA_0500')
gc(reset=TRUE)

gc(reset=TRUE)
gc(reset=TRUE)


