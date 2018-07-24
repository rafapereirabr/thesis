##################### Set working directory -------------------------------------------------------

setwd("R:/Dropbox/Dout/Data Dout")


##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")



library(tmap)
library(stringr)
library(texreg)



### read data -------------------


##### A. Load oAccess data ----------

#APAGAR  i="0500"
  
  # Read accesibility data of each grid
  
  oaccess_wide <- fread(paste0("./accessibility/output_oAccess_wide_500_paper4.csv"))
  


  # add 0.01 jobs to areas with no job
  oaccess_wide$totaljobs <- as.numeric(oaccess_wide$totaljobs)

    
  
 




##### B. Read Spatial Grids ----------

grid_0500 <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_0500')




for( i in c('full', 'partial')){
  for (j in c('jobs', 'schools')){
    
#    i="full"; j="jobs"
    
    # annouce grid
    cat("working on grid", i, j, "\n")
    
    # get spatial object
    grid_map <- copy(grid_0500)
    plot(  grid_map)
    
    # get spatial object
    grid_data <- copy(oaccess_wide)
    head(grid_data)
    
    
    # Remove 0 num ano e infinito na diferenca
      grid_data <- subset(grid_data, ID != 5445 ) # industrial areas -  fronteira nordeste
      grid_data <- subset(grid_data, ID != 5446 ) # industrial areas -  fronteira nordeste
      grid_data <- subset(grid_data, ID != 5463 ) # industrial areas -  fronteira nordeste
      grid_data <- subset(grid_data, ID != 4689 ) # rural area - north


    # check elevation
    ggplot() + geom_point(data=grid_data, aes(x=X, y=Y, color=elevation)) + coord_equal()+
    geom_point(data=subset(grid_data, elevation<1), aes(x=X, y=Y), color="red") 
    
    # check jobs == 0
    ggplot() + 
      geom_point(data=grid_data, aes(x=X, y=Y, color=elevation)) + coord_equal()+
      geom_point(data=subset(grid_data, totaljobs==0 & pop >0), aes(x=X, y=Y), color="red") 
  
    # time to stop NA
    ggplot() + 
      geom_point(data=grid_data, aes(x=X, y=Y, color=elevation)) + coord_equal()+
      geom_point(data=subset(grid_data, pop > 0 & is.na(time_to_stop)), aes(x=X, y=Y), color="red") 
    
    
    grid_data[totaljobs==0 & pop >0]
       
######## 3. Spatial join -------------------------
cat("Spatial Join \n")

# only keep grid cells data
grid_map_sub <- subset(grid_map, ID  %in% grid_data$ID)
plot(grid_map_sub, col="gray")

# spatial join
grid_map_sub@data <- join(grid_map_sub@data, grid_data, by="ID")
head(grid_map_sub@data)

# plot choropleth
tmap::qtm(shp= grid_map_sub, fill = "diff_ratio_oaccess_jobsmatch_full_60") 


    


# prepare data ------------------
#

map <- copy(grid_map_sub)

summary(map@data$pop)
summary(map@data$RDPC)
summary(map@data$elevation)
summary( map@data$diff_ratio_oaccess_jobsmatch_full_60 )
summary( map@data$diff_ratio_oaccess_jobsmatch_partial_60 )

log(map@data$diff_ratio_oaccess_jobsmatch_full_60) %>% density(na.rm=T) %>% plot
log(map@data$diff_ratio_oaccess_jobsmatch_partial_60) %>% density(na.rm=T) %>% plot






map$popdens <- map$pop / map$area
map$elevation <- map$elevation + 0.001


map$jobdens <- (map$totaljobs + 0.1) / map$area

map@data$jobdens %>% log() %>% density %>% plot




if(j=='jobs' & i == "full"){ 
                           map$ratio <- map@data$diff_ratio_oaccess_jobsmatch_full_60
                          }

if(j=='jobs' & i == "partial"){ 
                      map$ratio <- map@data$diff_ratio_oaccess_jobsmatch_partial_60
}

if(j=='schools' & i == "full"){ 
  map$ratio <- map@data$diff_ratio_oaccess_schools_full_60
}

if(j=='schools' & i == "partial"){ 
  map$ratio <- map@data$diff_ratio_oaccess_schools_partial_60
}





# keep only cells with pop and complete data
  plot(map, col="gray")
  map@data$pop <- ifelse(map@data$pop == 0, NA, map@data$pop) # reas with no POP sao excluidas da regressao
  map <- subset(map, !is.na(ratio))
  map <- subset(map, ratio != 1)
  
  # map <- subset(map, !is.na(time_to_stop))
  # map@data$time_to_stop <- ifelse(map@data$time_to_stop ==0, 0.1, map@data$time_to_stop)
    

  map <- subset(map, elevation >0 )
  map <- subset(map, !is.na(elevation))
  
  plot(map, col="gray")

  
  
  a <- log(map@data$income) 
  b <- log(map@data$diff_ratio_oaccess_jobsmatch_full_60) 
  plot(a, b)
  
  a <- map@data$decile
  b <- log(map@data$diff_ratio_oaccess_jobsmatch_full_60) 
  plot(a, b)

    
#### Choose W specification ----------------------------------
  
  # (Chi & Zhu, 2008) “suggest using a wide array of neighbor specifications, 
  # then picking the one that maximizes the autocorrelation coefficient”. >>> https://rpubs.com/corey_sparks/246342
  
  # my OLS
  f1_ols <- lm( log(ratio) ~ log(RDPC)+log(popdens)+log(jobdens)+log(elevation), weight=pop,data=map@data)
  
  
  ### How the residuals of OLS vary when using various k nearest neighbors
  #>>> use the point that returns the highest residual Moran’s I value
  n=20; res <- data.frame(k=1:n,I=rep(NA,n))
  
  for(i in 2:n){  cat(i)
                  us_nb<-knearneigh(coordinates(map), k=i)
                  us_nb<-knn2nb(us_nb)
                  us_wt<-nb2listw(us_nb,style="W")
                  res$I[i] <- lm.morantest(f1_ols, listw=us_wt)$estimate[1]
  }
  
  plot(res,type="b",main="Moran's I by neighbor numbers",pch=20,cex=1.5)
  #>>> K5 parece melhor (maximiza Moran's I com 0.6224508102), mas tem ainda q testar Queen, Rook e Distance
  
  
  
  
  
  # Create a good representative set of neighbor types
    us_nb6<-knearneigh(coordinates(map), k=6)
    us_nb6<-knn2nb(us_nb6)
    us_wt6<-nb2listw(us_nb6, style="W")
    
    us_nb5<-knearneigh(coordinates(map), k=5)
    us_nb5<-knn2nb(us_nb5)
    us_wt5<-nb2listw(us_nb5, style="W")
    
    us_nb4<-knearneigh(coordinates(map), k=4)
    us_nb4<-knn2nb(us_nb4)
    us_wt4<-nb2listw(us_nb4, style="W")
    
    us_nb3<-knearneigh(coordinates(map), k=3)
    us_nb3<-knn2nb(us_nb3)
    us_wt3<-nb2listw(us_nb3,style="W")
    
    us_nb2<-knearneigh(coordinates(map), k=2)
    us_nb2<-knn2nb(us_nb2)
    us_wt2<-nb2listw(us_nb2,style="W")
    
    us_nbr<-poly2nb(map, queen=F)
    us_wtr<-nb2listw(us_nbr, zero.policy=T)
    
    us_nbq<-poly2nb(map, queen=T)
    us_wtq<-nb2listw(us_nbr, style="W", zero.policy=T)
    
# create a travel-time distance matrix 
    cat("Loading tt matrix\n")

  # Load TT Matrix
    ttmatrix <- fst::read.fst("./accessibility/matrix_500_paper4.fst")
    gc(reset = T)
    gc(reset = T)
    
  # get median travel times
    ttmatrix <- setDT(ttmatrix)[, .(travel_time=median(travel_time, na.rm = T)), by=.(origin, destination)]
    gc(reset = T)
    gc(reset = T)
    
  # get long matrix of all possible combintions >> thil will help us get a symatric matrix
    allIDs <- unique(map@data$ID)
    length(allIDs)
    all_to_all <- expand.grid(allIDs, allIDs) %>% setDT()
    names(all_to_all) <- c("origin", "destination") 
    all_to_all <- all_to_all[order(origin, destination)] # sort
    head(all_to_all)
    
  
  # add travel times
    all_to_all[ttmatrix, on=c("origin", "destination"), travel_time := i.travel_time]
    head(all_to_all)
   

  # convert to matrix format
    ttmatrix3 <- acast(all_to_all, origin~destination, value.var="travel_time") %>% as.matrix()
    ttmatrix3 <- as.matrix(ttmatrix3)
  

      
    
  # get an inverse matrix of distances, make sure diagonal=0
    W <- 1/ttmatrix3
    diag(W) <- 0
  
    
  # replace NA values with 0, so that areas are not neighbors
    W[!is.finite(W)] <- NA # convert inf into NA
    sum(is.na(W))
    W[is.na(W)] <- 0
    sum(is.na(W))
    

# row-normalized Matrix > fica mais rapido, mas a opcao style="W" de mat2listw ja faz a normalizacao
    rtot <- rowSums(W, na.rm=TRUE)
    W <- W / rtot
    rowSums(W, na.rm=TRUE)
    
    
        

        
### Get Spatial weights Matrix (travel time)
      weights_ttime <- mat2listw(W, row.names = row.names(W), style="W") # W > row-standardised weights
      summary(weights_ttime)
      beep()

      
      
  
  
# Test W specifications
  resi<-c(lm.morantest(
                        f1_ols, listw=us_wtq,zero.policy=T)$estimate[1], # Queen
                        lm.morantest(f1_ols, listw=us_wt2)$estimate[1],
                        lm.morantest(f1_ols, listw=us_wt3)$estimate[1],
                        lm.morantest(f1_ols, listw=us_wt4)$estimate[1],
                        lm.morantest(f1_ols, listw=us_wt5)$estimate[1],
                        lm.morantest(f1_ols, listw=us_wtr,zero.policy=T)$estimate[1] 
                      #  , lm.morantest(f1_ols, listw=weights_ttime,zero.policy=T)$estimate[1] # time distance
            )
    plot(resi, type="l")
    symbols(x= which.max(resi), y=resi[which.max(resi)], circles=1, fg=2,lwd=2,add=T)

        
#Let's look at the local autocorrelation in our residuals
  #get the values of I
  map$residuals <- residuals(f1_ols)
  map$lmfit1<-localmoran(map$residuals, listw=us_wtq, zero.policy=T)[,1]
  brks<-classInt::classIntervals(map$lmfit1, n=5, style="quantile")
  spplot(map, "lmfit1", at=brks$brks
         , col.regions=brewer.pal(5, "RdBu"), main="Local Moran Plot of Residuals")
    
        
  map$residuals <- residuals(f1_ols)
  map$lmfit1<-localmoran(map$residuals, listw=weights_ttime, zero.policy=T)[,1]
  brks<-classInt::classIntervals(map$lmfit1, n=5, style="quantile")
  spplot(map, "lmfit1", at=brks$brks
         , col.regions=brewer.pal(5, "RdBu"), main="Local Moran Plot of Residuals")
  
  

  
  
  


### QUEEN neighbours matix ------------------------
nb <- poly2nb(map, queen=TRUE)                   # >>>> neighbours matrix
lw <- nb2listw(nb, style = "W", zero.policy = T) # >>>> weights matrix



# Test spatial dependence in variables   ------------
# p-value is significant -> there is spatial dependence
  moran.test(map$ratio, lw, alternative="two.sided", zero.policy=T)
  moran.test(map$RDPC, lw, alternative="two.sided", zero.policy=T)
  moran.test(map$elevation, lw, alternative="two.sided", zero.policy=T)
  moran.test(map$jobdens, lw, alternative="two.sided", zero.policy=T)
  moran.test(map$popdens, lw, alternative="two.sided", zero.policy=T)
  moran.test(map$decile, lw, alternative="two.sided", zero.policy=T)
  
  

  
  
  
### variogram of outcome of interest ------------------------
  # it shows how correlated are pairs of spatial observations when you increase the distance (lag) between them
  
  library(gstat)
  
  plot(variogram(map$jobdens ~ 1, locations = coordinates(map), data = map, cloud = F), 
       type = "b", pch = 16, main = "Variogram of PPOV")
  
  plot(variogram(map$ratio ~ 1, locations = coordinates(map), data = map, cloud = F), 
       type = "b", pch = 16, main = "Variogram of PPOV")
  
  

  
  
  
### My Linear regression ------------------------

f1 <- log(ratio) ~ log(RDPC)+log(popdens)+log(jobdens)+log(elevation)
f2 <- log(ratio) ~ log(RDPC)+log(popdens)+log(jobdens)+log(elevation) #+log(time_to_stop)
f3 <- log(ratio) ~ I(decile)+log(popdens)+log(jobdens)+log(elevation) #+log(time_to_stop)


fit_lm1 <- lm(f1, data=map)
fit_lm2 <- lm(f2, data=map)
fit_lm3 <- lm(f3, data=map)

texreg::screenreg(list(fit_lm1, fit_lm2, fit_lm3), digits = 4)








# Test spatial dependence in residuals ----------------------
# lm.morantest applies the Moran I test statistic for spatial autocorrelation in the regression residuals
  lm.morantest(fit_lm1, lw, alternative="two.sided", zero.policy=T)

# interpretation
  # p-value not significar >> it may well be random with NO spatial dependence
  # positive z value >> positive correlation
  # negative z valuenegative correlation




      
### Regression models in spdep (cross section)
# lm                            - Basic non-spatial linear regression model
# lagsarlm                      - spatial autoregressive (SAR) model [lag model]
# lagsarlm with type="mixed"    - spatial Durbin model
# errorsarlm                    - spatial error (SEM) model          [error model]
# errorsarlm with type="emixed" - spatial Durbin ERROR model
# lmSLX                         - lm model augmented with the spatially lagged RHS variables
     
    
    # load function to extrat total impact of regression
    source("./R scripts/00_extract_sarlm2impact.R")
    
    
gc(reset = T)
#### Run all models ---------------------------------------------------------------
      #quando usa matrix de tempo de  zero.policy=T, na.action="na.omit")
    

# MODEL SPECIFICATION
  f1 <- log(ratio) ~ log(RDPC)+log(popdens)+log(jobdens)+log(elevation) #+log(time_to_stop)
# f1 <- log(ratio) ~ I(decile)+log(popdens)+log(jobdens)+log(elevation) #+log(time_to_stop)

      
# LM model
  fit_lm <- lm( f1, weights = pop, data=map)
  summary(fit_lm)
      
     

# SAR - Lag model 
  fit_lag <- lagsarlm(f1, data=map, listw=lw, type="lag", method="MC", zero.policy=T, na.action="na.omit")
  screenreg(fit_lag)
  summary(fit_lag)
  # Rho is your spatial autoregressive parameter (it should have significant p-value)
  # likelihood ratio  tells you whether the inclusion of the lagged values improve your model or not
  # LM test (Lagrage test for spatial auto-correlation): significant p-value -> there is autocorrelation
  
  # plot residuals
    map@data$residuals_lag <- residuals(fit_lag)
    tmap::qtm(shp= map, fill = "residuals_lag") 
  
    # impacts
    W <- as(lw, "CsparseMatrix")          # Spatial weights matrix powers traces
    trMC <- trW(W, type="MC", listw = lw) # Spatial weights matrix powers traces
    imp <- summary(impacts(fit_lag, tr=trMC, R=1000), zstats=TRUE, short=TRUE) # get model impacts
    extract.sarlm2(fit_lag, imp) %>%  screenreg()
    
  
# SAR - Error model
  fit_error <- errorsarlm(f1, data=map, listw=lw, etype="error", method="MC", weights=pop, zero.policy=T, na.action="na.omit")
  screenreg(fit_error)
  
  

  
  
# Spatial Durbin Lag Model
  fit_durb <- lagsarlm(f1, data=map, listw=lw, type="mixed", method="MC", zero.policy=T, na.action="na.omit")
  screenreg(fit_durb)
  summary(fit_durb, Nagelkerke=T)

  # impacts
  W <- as(lw, "CsparseMatrix")          # Spatial weights matrix powers traces
  trMC <- trW(W, type="MC", listw = lw) # Spatial weights matrix powers traces
  imp <- summary(impacts(fit_durb, tr=trMC, R=1000), zstats=TRUE, short=TRUE) # get model impacts
  extract.sarlm2(fit_durb, imp) %>%  screenreg()
  

  
##### Indirect effects = Spill over effects
# "indirect effects (cross-partials) represent the changes in the dependent variable of 
#  a particular region arising from a one unit increase in an explanatory variable in another region"
#  
# source:  Golgher, A. B., & Voss, P. R. (2016). How to interpret the coefficients of spatial models: 
#          Spillovers, direct and indirect effects. Spatial Demography, 4(3), 175-205.
##### 
  
##### Total effect
 # # total effect is the sum of the direct and indirect effects
 #  
 #  
 #  # get table with direct and indirect effects >> https://rpubs.com/corey_sparks/250314
 #  W <- as(lw, "CsparseMatrix")
 #  trMC <- trW(W, type="MC")
 #  im<-impacts(fit_durb, tr=trMC, R=2000)
 #  sums<-summary(im,  zstats=T)
 #  data.frame(sums$res)
 #  data.frame(sums$pzmat)
  
  
  
    
# Spatial Durbin Error Model (SDEM)
  fit_errordurb <- errorsarlm(f1, data=map, listw=lw, etype="emixed", method="MC", weights=pop, zero.policy=T, na.action="na.omit")
  screenreg(fit_errordurb)
  summary(fit_errordurb, Nagelkerke=T)
  
  # impacts
  W <- as(lw, "CsparseMatrix")          # Spatial weights matrix powers traces
  trMC <- trW(W, type="MC", listw = lw) # Spatial weights matrix powers traces
  imp <- summary(impacts(fit_errordurb, tr=trMC, R=1000), zstats=TRUE, short=TRUE) # get model impacts
  extract.sarlm2(fit_errordurb, imp) %>%  screenreg()
  
  
    
# SLX model - #> spatial error model augmented with spatial lag in the right-hand-side variables
  fit_lmSLX <- lmSLX(f1, data=map, listw=lw, weights=pop, zero.policy=T, na.action="na.omit")
  summary(fit_lmSLX)
  
  
# SAC Model 
  fit_sac <- sacsarlm(f1, data=map, listw=lw, type="sac", method="MC", zero.policy=T, na.action="na.omit")
  screenreg(fit_sac)

  # impacts
  W <- as(lw, "CsparseMatrix")          # Spatial weights matrix powers traces
  trMC <- trW(W, type="MC", listw = lw) # Spatial weights matrix powers traces
  imp <- summary(impacts(fit_sac, tr=trMC, R=1000), zstats=TRUE, short=TRUE) # get model impacts
  extract.sarlm2(fit_sac, imp) %>%  screenreg()
  
# # SMA Model
#   fit_sma <- spautolm(f1, data=map, listw=lw, family="SMA", weights=pop, zero.policy=T, na.action="na.omit")
#   
#   summary(fit_sma)
  
beep()


# compare residuals of models
map$residuals_durb <- residuals(fit_durb)
map$residuals_sac <- residuals(fit_sac)

moran.test(map$residuals_durb, lw, alternative="two.sided", zero.policy=T)
moran.test(map$residuals_lag, lw, alternative="two.sided", zero.policy=T)
moran.test(map$residuals_sac, lw, alternative="two.sided", zero.policy=T)

  
####### Choosing a model ------------------
  
### Lagrange multiplier test
  # to identify which alternative model would be most appropriate (spatial error models Vs spatial lag models)
  #  which of the LMerr and LMlag are significant ?
  # check pvalues of LMerr and LMlag, if necessary check the robust RLMerr and RLMlag
  

fit_lm <- lm(log(ratio) ~ log(RDPC) + log(popdens) + log(jobdens) + log(elevation),  data=map)
# fit_lm <- lm(log(ratio) ~ I(decile) + log(popdens) + log(jobdens) + log(elevation),  data=map)



  a <- lm.LMtests(fit_lm, listw=lw, test="all", zero.policy=T)
  summary(a)
  
  # > robust lag model OR SARMA
  summary(fit_lag)

  
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
  
  
  
### likelihood ratio test
  # if two tests are very close, use likelihood ratio test (the higher Log likelihood the better) or lowest AIC
  anova(fit_durb, fit_lag) # OR
  anova(fit_lag, fit_sac)
  
  
  LR.sarlm(fit_sac, fit_error)

  anova(fit_error, fit_lag)
  summary(fit_lag)

  
  }
}
  
  
  
  
