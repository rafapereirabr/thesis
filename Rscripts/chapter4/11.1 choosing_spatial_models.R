##################### Set working directory -------------------------------------------------------

setwd("R:/Dropbox/Dout/Data Dout")


##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")



library(tmap)
library(stringr)
library(texreg)


# load function to extrat total impact of regression
source("./R scripts/00_extract_sarlm2impact.R")

options(digits=10)   # number of digits to show


### read data -------------------


##### A. Load oAccess data ----------

for (i in c('0500', '1000', '2000', '4000', 'trzn')){
  

  
  
  # Read accesibility data of each grid
  oaccess_wide <- read_csv(paste0("./accessibility/output_oAccess_wide_",i,"_201404_2017mix.csv")) # fread
  
  # add grid info
  oaccess_wide$grid <- as.character(i) 
  
  # add 0.01 jobs to areas with no job
  oaccess_wide$totaljobs <- as.numeric(oaccess_wide$totaljobs)
  setDT(oaccess_wide)[totaljobs==0, totaljobs := 0.001]
  
  

  # assign corresponding names
  assign(paste0("oaccess_wide_",i), oaccess_wide)
  #assign(paste0("oaccess_long_",i), oaccess_long)
  #rm(oaccess_wide, oaccess_long)
  rm(oaccess_wide)
  
}





##### B. Read Spatial Grids ----------

grid_0500 <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_0500')
grid_1000 <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_1000')
grid_2000 <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_2000')
grid_4000 <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_4000')
grid_trzn <- readOGR(dsn = './shapefiles_OD_rio_2012', layer ='map_grid_trzn')




# for (i in c('0500', '1000', '2000', '4000', 'trzn')){
#   
#   for (j in c('jobs', 'schools')){
    
    i="0500"; j="jobs"
    
    # annouce grid
    cat("working on grid", i, j, "\n")
    
    # get spatial object
    grid_map <- get(ls(pattern=paste0("grid_",i)))
    plot(  grid_map)
    
    # get data
    grid_data <- get(ls(pattern=paste0("oaccess_wide_",i)))
    head(grid_data)
    
    
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

map <- copy(grid_map_sub)

summary(map@data$pop)
summary(map@data$RDPC)
summary(map@data$elevation)
summary(map@data$diff_minus_oaccess_jobsmatch_60)
summary(map@data$diff_ratio_oaccess_jobsmatch_60)
log(map@data$diff_ratio_oaccess_jobsmatch_60) %>% density(na.rm=T) %>% plot


map$popdens <- map$pop / map$area
map$jobdens <- map$totaljobs / map$area

summary(map@data$jobdens)


ifelse(j=='jobs', 
       map$ratio <- map@data$diff_ratio_oaccess_jobsmatch_60,
       map$ratio <- map@data$diff_ratio_oaccess_schools_60
        )

ifelse(j=='jobs', 
       map$diffaccess <- map@data$diff_minus_oaccess_jobsmatch_60,
       map$diffaccess <- map@data$diff_minus_oaccess_schools_60
        )

# keep only cells with pop and complete data
  #\ oaccess_wide <- oaccess_wide[, c(1,4,5,7,18,19,22:165)] # soh vars de diff
  plot(map, col="gray")
  map <- subset(map, pop >0)
  map <- subset(map, !is.na(ratio))
  map <- subset(map, ratio != 0)
  map <- subset(map, !is.na(diffaccess))
  map <- subset(map, !is.na(elevation))
#  map <- subset(map, !is.na(time_to_stop))
  map <- subset(map, elevation >0 )
  plot(map, col="gray")

  




### QUEEN neighbours matix ------------------------
nb <- poly2nb(map, queen=TRUE)                   # >>>> neighbours matrix
lw <- nb2listw(nb, style = "W", zero.policy = T) # >>>> weights matrix



# Test spatial dependence in variables   ------------
# p-value is significant -> there is spatial dependence
  moran.test(map$ratio, lw, alternative="two.sided", zero.policy=T)
  moran.test(map$diffaccess, lw, alternative="two.sided", zero.policy=T)
  moran.test(map$RDPC, lw, alternative="two.sided", zero.policy=T)
  moran.test(map$totaljobs, lw, alternative="two.sided", zero.policy=T)
  moran.plot(map$jobdens, lw, alternative="two.sided", zero.policy=T)
  
  
  
  moran.mc( map$ratio, listw=lw, nsim=1000, zero.policy=TRUE)
  moran.mc( map$RDPC, listw=lw, nsim=1000, zero.policy=TRUE)
  moran.mc( map$jobdens, listw=lw, nsim=1000, zero.policy=TRUE)
  
  
### variogram of outcome of interest ------------------------
  
  library(gstat)
  gstat::variogram(map$jobdens ~ 1, locations = coordinates(map), data = map, cloud = F) %>% plot(type = "b", pch = 16, main = "Variogram of PPOV")
  
  #> indicate there is spatial autocorrelation in the var
  
  
  
  
### My Linear regression ------------------------

  f1 <- log(ratio) ~ log(RDPC)+log(popdens)+log(jobdens)+log(elevation)+I(time_to_stop)
  f2 <- log(ratio) ~ log(RDPC)+log(popdens)+log(jobdens)+log(elevation)
  f3 <- log(ratio) ~ log(RDPC)+I(popdens)+I(jobdens)+I(elevation)
  
  
  fit_lm1 <- lm(f1, data=map)
  fit_lm2 <- lm(f2, data=map)
  fit_lm3 <- lm(f3, data=map)
  
  screenreg(list(fit_lm1, fit_lm2, fit_lm3), digits = 4)



# Test spatial dependence in OLS residuals ----------------------
  # lm.morantest applies the Moran I test statistic for spatial autocorrelation in the regression residuals
    lm.morantest(fit_lm1, lw, alternative="two.sided", zero.policy=T)
  
    # interpretation:
    # p-value not significant -> it may well be random with NO spatial dependence
    # positive z value >> positive correlation
    # negative z valuenegative correlation



      

      
#### Run all models ---------------------------------------------------------------
      
    ## Regression models in spdep (cross section)
    # lm                            - Basic non-spatial linear regression model
    # lagsarlm                      - spatial autoregressive (SAR) model [lag model]
    # lagsarlm with type="mixed"    - spatial Durbin model
    # errorsarlm                    - spatial error (SEM) model          [error model]
    # errorsarlm with type="emixed" - spatial Durbin ERROR model
    # lmSLX                         - lm model augmented with the spatially lagged RHS variables
      

# spatial dependence in:
  # Lag:          y
  # SLX:          x
  # error:        error
  
  # Durbin:       y and x
  # Durbin error: x and error
  # Sac:          y and error  
  
  
# base MODEL SPECIFICATION
  f1 <- log(ratio) ~ log(RDPC)+log(popdens)+log(jobdens)+log(elevation)#+log(time_to_stop)
      
      
#### LM model
  fit_lm <- lm( f1, weights = pop, data=map)
  screenreg(fit_lm)
  
  
#### SAR - Lag model 
  fit_lag <- lagsarlm(f1, data=map, listw=lw, type="lag", method="MC", zero.policy=T, na.action="na.omit")
  screenreg(fit_lag)

  # Rho is your spatial autoregressive parameter (it should have significant p-value)
  # likelihood ratio  tells you whether the inclusion of the lagged values improve your model or not
  # LM test (Lagrage test for spatial auto-correlation): significant p-value -> there is autocorrelation
  
  # plot residuals
  map@data$residuals_lag <- residuals(fit_lag)
  tmap::qtm(shp= map, fill = "residuals_lag") 
  
  # impacts
  W <- as(lw, "CsparseMatrix")          # get spatial weights matrix in CsparseMatrix form
  trMC <- trW(W, type="MC", listw = lw) # powers traces of a Spatial weights matrix 
  imp_lag <- spdep::impacts(fit_lag, tr=trMC, R=1000 , tol=.1) %>% summary(zstats=TRUE, short=TRUE) # get model impacts
  extract.sarlm2(fit_lag, imp_lag) %>% screenreg()
  
  
  
  
  
  
#### SAR - Error model
  fit_error <- errorsarlm(f1, data=map, listw=lw, etype="error", method="MC", weights=pop, zero.policy=T, na.action="na.omit")
  
  
#### Spatial Durbin Model
  fit_durb <- lagsarlm(f1, data=map, listw=lw, type="mixed", method="MC", zero.policy=T, na.action="na.omit")
  summary(fit_durb, Nagelkerke=T)

  # impacts
  W <- as(lw, "CsparseMatrix")          # get spatial weights matrix in CsparseMatrix form
  trMC <- trW(W, type="MC", listw = lw) # powers traces of a Spatial weights matrix 
  imp_durb <- spdep::impacts(fit_durb, tr=trMC, R=1000 , tol=.1) %>% summary(zstats=TRUE, short=TRUE) # get model impacts
  extract.sarlm2(fit_durb, imp_durb) %>% screenreg()
  
  
  
  
##### Indirect effects = Spill over effects
# "indirect effects (cross-partials) represent the changes in the dependent variable of 
#  a particular region arising from a one unit increase in an explanatory variable in another region"
#  
# source:  Golgher, A. B., & Voss, P. R. (2016). How to interpret the coefficients of spatial models: 
#          Spillovers, direct and indirect effects. Spatial Demography, 4(3), 175-205.
##### 
  
##### Total effect
#  total effect is the sum of the direct and indirect effects
  
  
  
    
# Spatial Durbin Error Model (SDEM)
  fit_errordurb <- errorsarlm(f1, data=map, listw=lw, etype="emixed", method="MC", weights=pop, zero.policy=T, na.action="na.omit")
  screenreg(fit_errordurb)
  summary(fit_errordurb, Nagelkerke=T)
  
  
# SLX model - #> spatial error model augmented with spatial lag in the right-hand-side variables
  fit_lmSLX <- lmSLX(f1, data=map, listw=lw, weights=pop, zero.policy=T, na.action="na.omit")
  summary(fit_lmSLX)
  
  
#### SAC Model
  fit_sac <- sacsarlm(f1, data=map, listw=lw, type="sac", method="MC", zero.policy=T, na.action="na.omit")
  screenreg(fit_sac)
  
  # impacts
  W <- as(lw, "CsparseMatrix")          # get spatial weights matrix in CsparseMatrix form
  trMC <- trW(W, type="MC", listw = lw) # powers traces of a Spatial weights matrix 
  imp_sac <- spdep::impacts(fit_sac, tr=trMC, R=1000 , tol=.1) %>% summary(zstats=TRUE, short=TRUE) # get model impacts
  extract.sarlm2(fit_sac, imp_sac) %>% screenreg()
  
  
  
# # SMA Model
#   fit_sma <- spautolm(f1, data=map, listw=lw, family="SMA", weights=pop, zero.policy=T, na.action="na.omit")
#   
#   summary(fit_sma)
  
beep()



  
####### Choosing a model ------------------
  
### Lagrange multiplier test
  # to identify which alternative model would be most appropriate (spatial error models Vs spatial lag models)
  #  which of the LMerr and LMlag are significant ?
  # check pvalues of LMerr and LMlag, if necessary check the robust RLMerr and RLMlag
  
  fit_lm <- lm(f1, data=map)
  Lm_test <- lm.LMtests(fit_lm, listw=lw, test="all", zero.policy=T)
  summary(Lm_test)
  
  # result: either Lag or error (-> Durbin ?)
  
### AIC
  # the lower the better
  AICs <- c(AIC(fit_lm),AIC(fit_lag), AIC(fit_error), AIC(fit_durb), AIC(fit_errordurb), AIC(fit_sac), AIC(fit_lmSLX)) # AIC(fit_sma)
  plot(AICs, type="l", lwd=1.5, xaxt="n", xlab="")
  axis(1, at=1:7,labels=) # number of models
  labels<-c("OLS", "Lag","Err", "Durbin","Err Durbin", "SAC",  "lmSLX" ) # "SMA",
  text(1:7, par("usr")[3]-.25, srt=45, adj=0, labels=labels, xpd=T)
  mtext(side=1, text="Model Specification", line=3)
  symbols(x= which.min(AICs), y=AICs[which.min(AICs)], circles=1, fg=2,lwd=2,add=T)
  
  # table
    knitr::kable( data.frame(Models=labels, AIC=round(AICs, 2)) %>% arrange_(., c('AIC')) ) 
  
    # result: either Sac, Lag or Durbin, but AIC values are quite close
    
    
  
### likelihood ratio test
  # if two tests are very close, use likelihood ratio test (the higher Log likelihood the better)
  anova(fit_durb, fit_lag) # whatever
  anova(fit_sac, fit_lag) # SAC
  anova(fit_durb, fit_sac) # SAC
  
  LR.sarlm(fit_sac, fit_durb)
  
  # result: either Sac or Durbin, but AIC and logLik values are quite close
  
  
# Durbin:       y and x
# Durbin error: x and error
# Sac:          y and error  



  
### check which one best minimizes autocorrelation in the OLS residuals
    # test spatial dependence in residuals. If pvalue is sig -> there is depedence. Model is not well specified
  
    moran.test(fit_sac$residuals, listw=lw, alternative="two.sided", zero.policy=T) 
    moran.test(fit_lag$residuals, listw=lw, alternative="two.sided", zero.policy=T)
    moran.test(fit_durb$residuals, listw=lw, alternative="two.sided", zero.policy=T)
  
    # using Monte Carlo simulation (prefered method)
    moran.mc( fit_sac$residuals, listw=lw, nsim=1000, zero.policy=TRUE)
    moran.mc( fit_lag$residuals, listw=lw, nsim=1000, zero.policy=TRUE)
    moran.mc( fit_durb$residuals, listw=lw, nsim=1000, zero.policy=TRUE)
    

        
  

### References
#  https://rpubs.com/corey_sparks/109650 # muito bom para global models || Corey S. Sparks
#  https://sites.google.com/site/econometricsacademy/econometrics-models/spatial-econometrics
#  https://rpubs.com/corey_sparks/246342
#  https://rpubs.com/corey_sparks/250314
#  http://www.people.fas.harvard.edu/~zhukov/Spatial6.pdf


  