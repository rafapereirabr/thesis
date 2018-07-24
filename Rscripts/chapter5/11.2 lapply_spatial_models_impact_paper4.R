



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






my_reg_jobs <- function(t, i, j, scenario){


  # i="0500"; j = "jobsmatch" ; t = 60 ; scenario="full"

# annouce grid
  cat(">>>>>>>>>>>>>>>   working on grid", i, j, t, "minutes <<<<<<<<<<<<<<<\n")

      
##### get spatial object
  cat("reading shape \n")
  
  grid_map <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_0500')
  
  
  plot(grid_map)
    
    
    
    
##### # get Grid data
    cat("get grid Data \n")
    
    grid_data <- fread("./accessibility/output_oAccess_wide_500_paper4.csv")

    grid_data$totaljobs <- as.numeric(grid_data$totaljobs)
    
    # add min time to closes bRT stop
      min_time_to_stop <- fread("./gtfs_brt_transbrasil/min_time_to_nearest_TransBra_stop.csv")
      grid_data <- left_join(grid_data , min_time_to_stop, by=c("ID"="origin"))
    
    
    
    # add grid info
    grid_data$grid <- as.character(i) 
    grid_data$scenario <- scenario
    
    # add 0.01 jobs to areas with no job
    grid_data$totaljobs <- as.numeric(grid_data$totaljobs)

    # add 0.01 jobs to areas with elevation == 0
    setDT(grid_data)[, elevation := elevation + 0.001 ]
    summary(grid_data$elevation)
    

    
    
# Remove 0 num ano e infinito na diferenca
      grid_data <- subset(grid_data, ID != 5445 ) # industrial areas -  fronteira nordeste
      grid_data <- subset(grid_data, ID != 5446 ) # industrial areas -  fronteira nordeste
      grid_data <- subset(grid_data, ID != 5463 ) # industrial areas -  fronteira nordeste
      grid_data <- subset(grid_data, ID != 4689 ) # rural area - north
    
    
# check elevation
    ggplot() + geom_point(data=grid_data, aes(x=X, y=Y, color=elevation)) + coord_equal()+
    geom_point(data=subset(grid_data, elevation<1), aes(x=X, y=Y), color="red") 
    
    
# pop com time to closest stop Missing
    ggplot() + geom_point(data=grid_data, aes(x=X, y=Y, color=elevation)) + coord_equal()+
      geom_point(data=subset(grid_data, pop >0 & is.na(time_to_stop)), aes(x=X, y=Y), color="red") 
    
    a <- grid_data[pop >0 & is.na(time_to_stop)]
    
    
######## 3. Spatial join -------------------------
cat("Spatial Join \n")

# only keep grid cells data
grid_map_sub <- subset(grid_map, ID  %in% grid_data$ID)
plot(grid_map_sub, col="gray")

# spatial join
grid_map_sub@data <- join(grid_map_sub@data, grid_data, by="ID")
head(grid_map_sub@data)

# # plot choropleth
# tmap::qtm(shp= grid_map_sub, fill = "diff_ratio_oaccess_jobsmatch_full_60") 
    
    




# prepare data ------------------
  cat("Prepare data - subset obs \n")

map <- copy(grid_map_sub)

# Pop density
map@data$pop <- ifelse(map@data$pop == 0, NA, map@data$pop) # areas with no POP sao excluidas da regressao
map$popdens <- map$pop / map$area
summary(map$popdens)

# Job density
map$jobdens <- (map$totaljobs + 0.1) / map$area
summary(map$totaljobs)
summary(map$jobdens)



# name of Y var
  tempvar_ratio <- noquote( paste0("diff_ratio_oaccess_",j,"_",scenario,"_",t) ) 

# Y variables
  map$ratio <- map[, paste(tempvar_ratio)][[1]] # ratio
  summary(map$ratio)
  


  
  
# keep only cells with pop and complete data
  plot(map, col="gray")
  nrow(map@data)
  
  map <- subset(map, !is.na(ratio))
# map <- subset(map, ratio != 1)
  map <- subset(map, elevation > 0 )
  map <- subset(map, !is.na(elevation))
  map <- subset(map, pop > 0 )
  
  plot(map, col="gray")


  
a <- grid_data[pop >0 & is.na(time_to_stop)]
  

### QUEEN neighbours matix ------------------------
  cat("QUEEN neighbours matix \n")
  
nb <- poly2nb(map, queen=TRUE)                   # >>>> neighbours matrix
lw <- nb2listw(nb, style = "W", zero.policy = T) # >>>> weights matrix





# MODEL SPECIFICATION  ------------------------

if( j =="jobsmatch") {f1 <- log(ratio) ~ log(RDPC)+log(popdens)+log(jobdens)+log(elevation)} # +I(time_to_stop)} # log(RDPC) I(decile)
if( j =="schools") {f1 <- log(ratio) ~ log(RDPC)+log(popdens)+I(schools)+log(elevation)}





### My Linear regression ------------------------
cat("Linear regression \n")



# Test spatial dependence in residuals ----------------------
# lm.morantest applies the Moran I test statistic for spatial autocorrelation in the regression residuals
#  lm.morantest(fit_lm1, lw, alternative="two.sided", zero.policy=T)
# p-value is significant -> there is spatial dependence in residuals
  

  
  
### Regression model in spdep (cross section)
cat(" - Spatial Regression  \n")

#plot( map$RDPC, map$ratio)
#plot( log(map$RDPC), log(map$ratio))

## Durbin model

fit_durb <- lagsarlm(f1, data=map, listw=lw, type="Durbin", method="eigen", zero.policy=T, na.action="na.omit")

# impacts
  W <- as(lw, "CsparseMatrix")          # Spatial weights matrix powers traces
  trMC <- trW(W, type="MC", listw = lw) # Spatial weights matrix powers traces
  
  imp <- summary(impacts(fit_durb, tr=trMC, R=1000), zstats=TRUE, short=TRUE) # get model impacts


# ## SAR - Lag model 
#    fit_lag1 <- lagsarlm(f1, data=map, listw=lw, type="lag", zero.policy=T, na.action="na.omit") # method="MC", 
#  
#   map@data$residuals_lag <- residuals(fit_lag1)
#   tmap::qtm(shp= map, fill = "residuals_lag") 
# 
# 
#  # impacts
#    W <- as(lw, "CsparseMatrix")          # Spatial weights matrix powers traces
#    trMC <- trW(W, type="MC", listw = lw) # Spatial weights matrix powers traces
#    imp <- summary(impacts(fit_lag1, tr=trMC, R=1000), zstats=TRUE, short=TRUE) # get model impacts
 
   
# extract   
   #myextract <- extract.sarlm2(fit_lag1, imp)
   myextract <- extract.sarlm2(fit_durb, imp)
   
   return(myextract)
   beep()
}









# multiple regressions

models_jobs_full <- lapply(list(30,60,90,120), my_reg_jobs, j="jobsmatch", i="0500", scenario = "full")
models_jobs_partial <- lapply(list(30,60,90,120), my_reg_jobs, j="jobsmatch", i="0500", scenario = "partial")
beep()

texreg::screenreg(models_jobs_full, digits = 3)
texreg::screenreg(models_jobs_partial, digits = 3)



# Savel multiple regressions

    htmlreg(models_jobs_full, digits = 3, file = "./plots_4_transbra/tabela_Durbin_regress_jobs_full.doc")
    htmlreg(models_jobs_partial, digits = 3, file = "./plots_4_transbra/tabela_Durbin_regress_jobs_partial.doc")
    

    
    
        
    
# test spatial dependence in residuals. If pvalue is sig -> there is depedence. Model is not well specified
    moran.mc( models_jobs_full[[1]]$residuals, listw=lw, nsim=1000, zero.policy=TRUE)
    

    
    


    

  