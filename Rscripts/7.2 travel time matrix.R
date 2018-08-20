# This script: 
  # gets the output of OpenTripPlanner / reads and binds several Travel-time Matrices
  # combines a data-frame of travel-time estimates with land-use data for the hexagonal grid


  

# set working Directory
setwd("R:/Dropbox/Dout/Data Dout")

# # SOGE server
# setwd("Z:/Desktop/Data Dout")
# setwd("~/Desktop/Data_Dout")


##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")


          
########## 1 Get a List of all OD Matrix files -------------------------------------------------------


save_traveltime_matrix <- function(grid, data_file, id, conunterfactual){


  # pattern of string to read files of deaprture time for each grid scale
  pattern = paste0('traveltime_matrix_', grid)


# get all files of each departure time into a list
  cat("Reading travel-time matrices \n")
    
  filenames2014 <- list.files("R:/Dropbox/OpenTripPlanner/jython_rio_2014-04", pattern= pattern, full.names=TRUE)
  
  if(conunterfactual==T)
      {
        filenames2017 <- list.files("R:/Dropbox/OpenTripPlanner/jython_rio_2017counterfactual", pattern= pattern, full.names=TRUE) 
      } else {
        filenames2017 <- list.files("R:/Dropbox/OpenTripPlanner/jython_rio_2017mix", pattern= pattern, full.names=TRUE) 
      }
  

    


  
  
########## 3. Read all matrices in Parallel

   ttmatrix <- pblapply(allmatrices, fread) %>% rbindlist()



  gc(reset=TRUE)
  gc(reset=TRUE)
  gc(reset=TRUE)
  beep()
  
    
  
# remove hexagonal cells with no info
  if(grid=="500"){ 
                  ttmatrix <- subset(ttmatrix, origin != 5445 ) # industrial areas -  fronteira nordeste
                  ttmatrix <- subset(ttmatrix, origin != 5446 ) # industrial areas -  fronteira nordeste
                  ttmatrix <- subset(ttmatrix, origin != 5463 ) # industrial areas -  fronteira nordeste
                  ttmatrix <- subset(ttmatrix, origin != 4689 ) # rural area - north
                  }
  if(grid=="1000"){ 
                  ttmatrix <- subset(ttmatrix, origin != 1436 ) # industrial areas -  fronteira nordeste
                  ttmatrix <- subset(ttmatrix, origin != 1240 ) # industrial areas -  fronteira nordeste
                  }
  
  
  
# convert time to minutes
  ttmatrix[ , travel_time := travel_time/60]
  summary(ttmatrix$travel_time)
  head(ttmatrix)
  

# # Convert year
years <- table(ttmatrix$year)
#   ttmatrix[ year ==201408, year := 2014]
#   ttmatrix[ year ==201705, year := 2017]
  


    
  
# read jobs data
  grid_data <- fread(paste0("./Spatial Grid/",data_file,".csv"))
  grid_data <-   grid_data[pop > 0 | totaljobs > 0 | hospitals > 0 | schools > 0,] # only cells with pop+hospitals+schools

  # get land-use data of destinations and origins    
  grid_data_dest <- grid_data[, .(ID, hospitals, hosp_low, hosp_med, hosp_high, schools, totaljobs, edubas, edumed, edusup)]
  grid_data_orig <- grid_data[, .(ID, grid, pop, income, decile, prop_poor)]
  gc(reset=TRUE)
  
  
# Merge job count with OD Matrix, allocating job counts to Destination
cat("Merging data to tt matrices  \n")
  
  # merge data using DATA.TABLE (faster)
    # origin
    gc(reset=TRUE)
    ttmatrix <- ttmatrix[grid_data_orig, on=c('origin'='ID'), nomatch=0]

    gc(reset=TRUE)
    gc(reset=TRUE)
    gc(reset=TRUE)
    
    # destination
    ttmatrix <- ttmatrix[grid_data_dest, on=c('destination'='ID'), nomatch=0]
    
    # clean memory
    rm(grid_data_dest, grid_data_orig)
    gc(reset=TRUE)
    gc(reset=TRUE)
    gc(reset=TRUE)
    
    beep()

    head(ttmatrix)

    

# Save Matrix ~ 164
cat("Saving travel-time matrices \n")
  
  system.time (  write_rds(ttmatrix, paste0("./accessibility/matrix_",grid,"_",names(years)[1],"_",names(years)[2],"_partial_newtrains.Rds") ) )
  return(ttmatrix)
  beep()
  gc(reset=TRUE)
}
  


# Apply function to save travel-time matrices
  ttmatrix_0500 <- save_traveltime_matrix(grid='500', data_file='gridDaTA_0500' ,conunterfactual=T)
  ttmatrix_1000 <- save_traveltime_matrix(grid='1000', data_file='gridDaTA_1000',conunterfactual=T) 
  ttmatrix_2000 <- save_traveltime_matrix(grid='2000', data_file='gridDaTA_2000',conunterfactual=T) 
  ttmatrix_4000 <- save_traveltime_matrix(grid='4000', data_file='gridDaTA_4000',conunterfactual=T) 
  ttmatrix_trzn <- save_traveltime_matrix(grid='trzn', data_file='gridDaTA_trzn',conunterfactual=T)
  gc(reset=TRUE)
  gc(reset=TRUE)

  




  
  