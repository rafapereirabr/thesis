##################### Set working directory -------------------------------------------------------

setwd("R:/Dropbox/Dout/Data Dout")


##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")

library(tmap)
library(stringr)
library(texreg)



# load function to extrat total impact of regression
source("./R scripts/00_extract_sarlm2impact.R")


##################### Regression function -------------------------------------------------------



my_reg_jobs <- function(i, j, t, counterfactual, spat_model){

  # i="0500"; j = "jobs" ; t = 60 ; counterfactual=F ; spat_model="lag"

# annouce grid
  cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> working on grid", i, j, t, "minutes \n")

      
##### get spatial object
  cat("reading shape \n")
  
  if(i=="trzn"){
                grid_map <- readOGR(dsn = './shapefiles_OD_rio_2012', layer =paste0('map_grid_', i))
        } else {
                grid_map <- readOGR(dsn = './Spatial Grid', layer =paste0('hex_grid_', i))  
                }
  
    plot(grid_map)
    
    
    
    
##### # get Grid data
    cat("get grid Data \n")
    
    if(counterfactual==T){
                grid_data <- paste0("./accessibility/output_oAccess_wide_",i,"_201404_2017counterfactual.csv") %>% fread() # fread
        } else{
                grid_data <- paste0("./accessibility/output_oAccess_wide_",i,"_201404_2017mix.csv") %>% fread() # fread
        }
    


        
    # add grid info
    grid_data$grid <- as.character(i) 
    
    # add 0.01 jobs to areas with no job
    grid_data$totaljobs <- as.numeric(grid_data$totaljobs)
    setDT(grid_data)[totaljobs==0, totaljobs := 0.001]

    # add 0.01 jobs to areas with elevation == 0
    setDT(grid_data)[elevation==0, elevation := 0.1]
    
    
# Remove 0 num ano e infinito na diferenca
    if(i=="0500"){
      grid_data <- subset(grid_data, ID != 5445 ) # industrial areas -  fronteira nordeste
      grid_data <- subset(grid_data, ID != 5446 ) # industrial areas -  fronteira nordeste
      grid_data <- subset(grid_data, ID != 5463 ) # industrial areas -  fronteira nordeste
      grid_data <- subset(grid_data, ID != 4689 ) # rural area - north
    }
    if(i=="1000"){
      grid_data <- subset(grid_data, ID != 1436 ) # industrial areas -  fronteira nordeste
      grid_data <- subset(grid_data, ID != 1240 ) # industrial areas -  fronteira nordeste
    }
    
# check elevation
    ggplot() + geom_point(data=grid_data, aes(x=X, y=Y, color=elevation)) + coord_equal()+
    geom_point(data=subset(grid_data, elevation<1), aes(x=X, y=Y), color="red") 
    
    

    
    
######## 3. Spatial join -------------------------
cat("Spatial Join \n")

# only keep grid cells data
grid_map_sub <- subset(grid_map, ID  %in% grid_data$ID)
plot(grid_map_sub, col="gray")

# spatial join
grid_map_sub@data <- join(grid_map_sub@data, grid_data, by="ID")
head(grid_map_sub@data)

# plot choropleth
tmap::qtm(shp= grid_map_sub, fill = "diff_minus_oaccess_jobsmatch_60") 
    
    




# prepare data ------------------
  cat("Prepare data - subset obs \n")

map <- copy(grid_map_sub)

# X var
map$popdens <- map$pop / map$area
map$jobdens <- map$totaljobs / map$area


# name of Y var
  tempvar_ratio <- noquote( paste0("diff_ratio_oaccess_jobsmatch_",t) ) 
  tempvar_minus <- noquote( paste0("diff_minus_oaccess_jobsmatch_",t) ) 

# Y variables
  map$ratio <- map[, paste(tempvar_ratio)][[1]] # ratio
  map$diffaccess <- map[, paste(tempvar_minus)][[1]] # minus


summary(map@data$jobdens)
summary(map@data$popdens)
summary(map@data$pop)
summary(map@data$RDPC)
summary(map@data$elevation)
summary(map@data$diff_minus_oaccess_jobsmatch_60)
summary(map@data$diff_ratio_oaccess_jobsmatch_60)
log(map@data$diff_ratio_oaccess_jobsmatch_60) %>% density(na.rm=T) %>% plot

summary(map@data$ratio)
summary(map@data$diffaccess)



# keep only cells with pop and complete data
  #\ oaccess_wide <- oaccess_wide[, c(1,4,5,7,18,19,22:165)] # soh vars de diff
  plot(map, col="gray")
  map <- subset(map, pop >0)
  map <- subset(map, !is.na(ratio))
  map <- subset(map, ratio != 0 )
  map <- subset(map, !is.na(diffaccess))
  map <- subset(map, !is.na(elevation))
#  map <- subset(map, !is.na(time_to_stop))
  map <- subset(map, elevation > 0 )
  

  plot(map, col="gray")

  

  

### QUEEN neighbours matix ------------------------
  cat("QUEEN neighbours matix \n")
  
nb <- poly2nb(map, queen=TRUE)                   # >>>> neighbours matrix
lw <- nb2listw(nb, style = "W", zero.policy = T) # >>>> weights matrix, row-standardized





# MODEL SPECIFICATION  ------------------------
f1 <- log(ratio) ~ log(RDPC)+log(popdens)+log(jobdens)+log(elevation) #+log(time_to_stop)


### My Linear regression ------------------------
cat("Linear regression \n")



  
  
### Regression model in spdep (cross section)
map$prop_med_oaccess_jobsmatch_60_2017
summary(map$prop_med_oaccess_jobsmatch_60_2017)

plot( map$RDPC, map$prop_med_oaccess_jobsmatch_60_2017)
plot( log(map$RDPC), log(map$prop_med_oaccess_jobsmatch_60_2017))



if (spat_model == "lag"){ 

      ## SAR - Lag model 
         fit_lag <- lagsarlm(f1, data=map, listw=lw, type="lag", method="MC", zero.policy=T, na.action="na.omit")
         
         # impacts
         W <- as(lw, "CsparseMatrix")          # get spatial weights matrix in CsparseMatrix form
         trMC <- trW(W, type="MC", listw = lw) # powers traces of a Spatial weights matrix 
         imp_lag <- spdep::impacts(fit_lag, tr=trMC, R=1000 , tol=.1) %>% summary(zstats=TRUE, short=TRUE) # get model impacts
         # extract   
         myextract <- extract.sarlm2(fit_lag, imp_lag)
      }


if (spat_model == "durbin"){ 
      
     ## Spatial Durbin Model
        fit_durb <- lagsarlm(f1, data=map, listw=lw, type="mixed", method="MC", zero.policy=T, na.action="na.omit")
        summary(fit_durb, Nagelkerke=T)
        
        # impacts
        W <- as(lw, "CsparseMatrix")          # get spatial weights matrix in CsparseMatrix form
        trMC <- trW(W, type="MC", listw = lw) # powers traces of a Spatial weights matrix 
        imp_durb <- spdep::impacts(fit_durb, tr=trMC, R=1000 , tol=.1) %>% summary(zstats=TRUE, short=TRUE) # get model impacts
        # extract   
        myextract <- extract.sarlm2(fit_durb, imp_durb)
    }
        

if (spat_model == "sac"){
  
      ## SAC Model 
        fit_sac <- sacsarlm(f1, data=map, listw=lw, type="sac", method="MC", zero.policy=T, na.action="na.omit")
        screenreg(fit_sac)
        
        # impacts
        W <- as(lw, "CsparseMatrix")          # get spatial weights matrix in CsparseMatrix form
        trMC <- trW(W, type="MC", listw = lw) # powers traces of a Spatial weights matrix 
        imp_sac <- spdep::impacts(fit_sac, tr=trMC, R=1000 , tol=.1) %>% summary(zstats=TRUE, short=TRUE) # get model impacts
        myextract <- extract.sarlm2(fit_sac, imp_sac)
}
        


   return(myextract)
#   return(fit_lag1)  
   
   beep()
   
     
}






my_reg_schools <- function(i, j, t, counterfactual, spat_model){
  
  # i="0500"; j = "jobs" ; t = 60 ; counterfactual = T
  
  # annouce grid
  cat("working on grid", i, j, t, "minutes \n")
  
  
##### get spatial object
  cat("reading shape \n")
  
  if(i=="trzn"){
    grid_map <- readOGR(dsn = './shapefiles_OD_rio_2012', layer =paste0('map_grid_', i))
  } else {
    grid_map <- readOGR(dsn = './Spatial Grid', layer =paste0('hex_grid_', i))  
  }
  
  plot(grid_map)
  
  
  
  
##### # get Grid data
  cat("get grid Data \n")
  
  if(counterfactual==T){
    grid_data <- paste0("./accessibility/output_oAccess_wide_",i,"_201404_2017counterfactual.csv") %>% fread() # fread
  } else{
    grid_data <- paste0("./accessibility/output_oAccess_wide_",i,"_201404_2017mix.csv") %>% fread() # fread
  }  
  
  #grid_data <- read_csv( "./accessibility/output_oAccess_wide_0500_201404_2017mix.csv") # fread
  
  # add grid info
  grid_data$grid <- as.character(i) 
  
  # add 0.01 jobs to areas with no job
  table(grid_data$schools)
  summary(grid_data$schools)
  grid_data$schools <- as.numeric(grid_data$schools)
  setDT(grid_data)[schools ==0, schools := 0.001]
  
  density(grid_data$schools) %>% plot 
  density(log(grid_data$schools)) %>% plot 
  
  
  # add 0.01 jobs to areas with elevation == 0
  setDT(grid_data)[elevation==0, elevation := 0.1]
  
  
  # Remove 0 num ano e infinito na diferenca
  if(i=="0500"){
    grid_data <- subset(grid_data, ID != 5445 ) # industrial areas -  fronteira nordeste
    grid_data <- subset(grid_data, ID != 5446 ) # industrial areas -  fronteira nordeste
    grid_data <- subset(grid_data, ID != 5463 ) # industrial areas -  fronteira nordeste
    grid_data <- subset(grid_data, ID != 4689 ) # rural area - north
  }
  if(i=="1000"){
    grid_data <- subset(grid_data, ID != 1436 ) # industrial areas -  fronteira nordeste
    grid_data <- subset(grid_data, ID != 1240 ) # industrial areas -  fronteira nordeste
  }
  
  # check elevation
  ggplot() + geom_point(data=grid_data, aes(x=X, y=Y, color=elevation)) + coord_equal()+
    geom_point(data=subset(grid_data, elevation<1), aes(x=X, y=Y), color="red") 
  
  
  
# add min time to closest stop
  min_time_to_stop <- fread( paste0("./R scripts/paper_3_maup/min_time_to_nearest_stop_",i,".csv"))
  grid_data <- left_join(grid_data , min_time_to_stop, by=c("ID"="origin"))
  
  
######## 3. Spatial join -------------------------
  cat("Spatial Join \n")
  
  # only keep grid cells data
  grid_map_sub <- subset(grid_map, ID  %in% grid_data$ID)
  plot(grid_map_sub, col="gray")
  
  # spatial join
  grid_map_sub@data <- join(grid_map_sub@data, grid_data, by="ID")
  head(grid_map_sub@data)
  
  # plot choropleth
  tmap::qtm(shp= grid_map_sub, fill = "diff_minus_oaccess_jobsmatch_60") 
  
  
  
  
  
  
# prepare data ------------------
  cat("Prepare data - subset obs \n")
  
  map <- copy(grid_map_sub)
  
  # X var
  map$popdens <- map$pop / map$area
  map$jobdens <- map$totaljobs / map$area
  
  
  # name of Y var
  tempvar_ratio <- noquote( paste0("diff_ratio_oaccess_schools_",t) ) 
  tempvar_minus <- noquote( paste0("diff_minus_oaccess_schools_",t) ) 
  
  # Y variables
  map$ratio <- map[, paste(tempvar_ratio)][[1]] # ratio
  map$diffaccess <- map[, paste(tempvar_minus)][[1]] # minus
  
  
  summary(map@data$jobdens)
  summary(map@data$popdens)
  summary(map@data$pop)
  summary(map@data$RDPC)
  summary(map@data$elevation)
  log(map@data$diff_ratio_oaccess_schools_60) %>% density(na.rm=T) %>% plot
  
  summary(map@data$ratio)
  summary(map@data$diffaccess)
  
  
  
  # keep only cells with pop and complete data
  #\ oaccess_wide <- oaccess_wide[, c(1,4,5,7,18,19,22:165)] # soh vars de diff
  plot(map, col="gray")
  map <- subset(map, pop >0)
  map <- subset(map, !is.na(ratio))
  map <- subset(map, ratio != 0 )
  map <- subset(map, ratio != Inf )
  map <- subset(map, !is.na(diffaccess))
  map <- subset(map, !is.na(elevation))
# map <- subset(map, !is.na(time_to_stop))
  map <- subset(map, elevation > 0 )
  
  plot(map, col="gray")
  
  
  
  
  
### QUEEN neighbours matix ------------------------
  cat("QUEEN neighbours matix \n")
  
  nb <- poly2nb(map, queen=TRUE)                   # >>>> neighbours matrix
  lw <- nb2listw(nb, style = "W", zero.policy = T) # >>>> weights matrix, row-standardized
  
  
  
### My Linear regression ------------------------
  cat("Linear regression \n")
  
 
  
  ### Regression model in spdep (cross section)
  
  # MODEL SPECIFICATION
  f1 <- log(ratio) ~ log(RDPC)+log(popdens)+I(schools)+log(elevation) #+log(time_to_stop)

  
  
if (spat_model == "lag"){ 
    
    ## SAR - Lag model 
    fit_lag <- lagsarlm(f1, data=map, listw=lw, type="lag", method="MC", zero.policy=T, na.action="na.omit")
    
    # impacts
    W <- as(lw, "CsparseMatrix")          # get spatial weights matrix in CsparseMatrix form
    trMC <- trW(W, type="MC", listw = lw) # powers traces of a Spatial weights matrix 
    imp_lag <- spdep::impacts(fit_lag, tr=trMC, R=1000 , tol=.1) %>% summary(zstats=TRUE, short=TRUE) # get model impacts
    # extract   
    myextract <- extract.sarlm2(fit_lag, imp_lag)
  }
  
  
if (spat_model == "durbin"){ 
    
    ## Spatial Durbin Model
    fit_durb <- lagsarlm(f1, data=map, listw=lw, type="mixed", method="MC", zero.policy=T, na.action="na.omit")
    summary(fit_durb, Nagelkerke=T)
    
    # impacts
    W <- as(lw, "CsparseMatrix")          # get spatial weights matrix in CsparseMatrix form
    trMC <- trW(W, type="MC", listw = lw) # powers traces of a Spatial weights matrix 
    imp_durb <- spdep::impacts(fit_durb, tr=trMC, R=1000 , tol=.1) %>% summary(zstats=TRUE, short=TRUE) # get model impacts
    # extract   
    myextract <- extract.sarlm2(fit_durb, imp_durb)
  }
  

  
if (spat_model == "sac"){
    
    ## SAC Model 
    fit_sac <- sacsarlm(f1, data=map, listw=lw, type="sac", method="MC", zero.policy=T, na.action="na.omit")
    screenreg(fit_sac)
    
    # impacts
    W <- as(lw, "CsparseMatrix")          # get spatial weights matrix in CsparseMatrix form
    trMC <- trW(W, type="MC", listw = lw) # powers traces of a Spatial weights matrix 
    imp_sac <- spdep::impacts(fit_sac, tr=trMC, R=1000 , tol=.1) %>% summary(zstats=TRUE, short=TRUE) # get model impacts
    myextract <- extract.sarlm2(fit_sac, imp_sac)
  }
  
  return(myextract)  
  
}



# single regression
models_jobs60 <- lapply(list("0500"), my_reg_jobs, j="jobs", t=60, counterfactual = T, spat_model="sac")



############### multiple regressions

#### SAC
    models_jobs60_sac_mix <- lapply(list("0500", "1000", "2000", "4000", "trzn"), my_reg_jobs, j="jobs", t=60, counterfactual = F, spat_model="sac")
    beep()
    models_jobs60_sac_counter <- lapply(list("0500", "1000", "2000", "4000", "trzn"), my_reg_jobs, j="jobs", t=60, counterfactual = T, spat_model="sac")
    beep()
    
    # save
    htmlreg(models_jobs60_sac_mix, digits = 2, file = "./plots_orig3/tabela_spatialsac_regress_jobs_60_mix_sac.doc")
    htmlreg(models_jobs60_sac_counter, digits = 2, file = "./plots_orig3/tabela_spatialsac_regress_jobs_60_counter_sac.doc")
    
    # check
    screenreg(models_jobs60_sac_mix, digits = 2)    
    
    screenreg(models_jobs60_sac_counter, digits = 2)



#### DURBIN
    models_jobs60_durbin_mix <- lapply(list("0500", "1000", "2000", "4000", "trzn"), my_reg_jobs, j="jobs", t=60, counterfactual = F, spat_model="durbin")
    beep()
    models_jobs60_durbin_counter <- lapply(list("0500", "1000", "2000", "4000", "trzn"), my_reg_jobs, j="jobs", t=60, counterfactual = T, spat_model="durbin")
    beep()
    
    # save
    htmlreg(models_jobs60_durbin_mix, digits = 2, file = "./plots_orig3/tabela_spatialdurbin_regress_jobs_60_mix_durbin.doc")
    htmlreg(models_jobs60_durbin_counter, digits = 2, file = "./plots_orig3/tabela_spatialdurbin_regress_jobs_60_counter_durbin.doc")
    
    # check
    screenreg(models_jobs60_durbin_mix, digits = 2)
    screenreg(models_jobs60_durbin_counter, digits = 2)
    
    
#### LAG
    models_jobs60_lag_mix <- lapply(list("0500", "1000", "2000", "4000", "trzn"), my_reg_jobs, j="jobs", t=60, counterfactual = F, spat_model="lag")
    beep()
    models_jobs60_lag_counter <- lapply(list("0500", "1000", "2000", "4000", "trzn"), my_reg_jobs, j="jobs", t=60, counterfactual = T, spat_model="lag")
    beep()
    
    # save
    htmlreg(models_jobs60_lag_mix, digits = 2, file = "./plots_orig3/tabela_spatiallag_regress_jobs_60_mix_lag.doc")
    htmlreg(models_jobs60_lag_counter, digits = 2, file = "./plots_orig3/tabela_spatiallag_regress_jobs_60_counter_lag.doc")
    
    # check
    screenreg(models_jobs60_lag_mix, digits = 2)
    screenreg(models_jobs60_lag_counter, digits = 2)
    
    

    
    
    
    
    
    
    
###############
### SCHOOLS ###
        
## DURBIN
    models_schools60_durbin_mix <- lapply(list("0500", "1000", "2000", "4000", "trzn"), my_reg_schools, j="schools", t=60, counterfactual = F, spat_model="durbin")
    beep()
    models_schools60_durbin_counter <- lapply(list("0500", "1000", "2000", "4000", "trzn"), my_reg_schools, j="schools", t=60, counterfactual = T, spat_model="durbin")
    beep()
    
    # save
    htmlreg(models_schools60_durbin_mix, digits = 2, file = "./plots_orig3/tabela_spatialdurbin_regress_schools_60_mix_durbin.doc")
    htmlreg(models_schools60_durbin_counter, digits = 2, file = "./plots_orig3/tabela_spatialdurbin_regress_schools_60_counter_durbin.doc")
    
    # check
    screenreg(models_schools60_durbin_mix, digits = 2)    
    screenreg(models_schools60_durbin_counter, digits = 2)
    
    
# plot regressao
  plotreg(models_jobs[1])




