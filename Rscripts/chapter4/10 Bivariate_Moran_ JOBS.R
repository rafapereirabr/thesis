
# set working Directory
setwd("R:/Dropbox/Dout/Data Dout")


##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")






### create Functions fo Bivariate Moran's I ---------------------
# source: https://stackoverflow.com/questions/45177590/map-of-bivariate-spatial-correlation-in-r-bivariate-lisa

  # Bivariate Moran's I
    moran_I <- function(x, y = NULL, W){
      if(is.null(y)) y = x
      
      xp <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
      yp <- (y - mean(y, na.rm=T))/sd(y, na.rm=T)
      W[which(is.na(W))] <- 0
      n <- nrow(W)
      
      global <- (xp%*%W%*%yp)/(n - 1)
      local  <- (xp*W%*%yp)
      
      list(global = global, local  = as.numeric(local))
    }
    
  
  # Permutations for the Bivariate Moran's I
    simula_moran <- function(x, y = NULL, W, nsims = 1000){
      
      if(is.null(y)) y = x
      
      n   = nrow(W)
      IDs = 1:n
      
      xp <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
      W[which(is.na(W))] <- 0
      
      global_sims = NULL
      local_sims  = matrix(NA, nrow = n, ncol=nsims)
      
      ID_sample = sample(IDs, size = n*nsims, replace = T)
      
      y_s = y[ID_sample]
      y_s = matrix(y_s, nrow = n, ncol = nsims)
      y_s <- (y_s - apply(y_s, 1, mean))/apply(y_s, 1, sd)
      
      global_sims  <- as.numeric( (xp%*%W%*%y_s)/(n - 1) )
      local_sims  <- (xp*W%*%y_s)
      
      list(global_sims = global_sims,
           local_sims  = local_sims)
    }


    






##### A. Load oAccess data ----------

for (i in c('0500', '1000', '2000', '4000', 'trzn')){
  
  # Read accesibility data of each grid
  oaccess_wide <- read_csv(paste0("./accessibility/output_oAccess_wide_",i,"_201404_2017mix.csv")) # fread
  
  # add grid info
  oaccess_wide$grid <- as.character(i) 
  
  
  # keep only cells with pop
  oaccess_wide <- oaccess_wide[, c(1,4,5,7,22:165)] # soh vars de diff
  oaccess_wide <- subset(oaccess_wide, pop >0) %>% setDT()  
  oaccess_wide <- subset(oaccess_wide, !is.na(diff_minus_oaccess_jobsmatch_60)) %>% setDT()  
  oaccess_wide <- subset(oaccess_wide, !is.na(diff_minus_oaccess_schools_60)) %>% setDT()  
  
  #oaccess_wide <- na.omit(oaccess_wide) ################################################## temporario. Ideal omitir Na somente na var de interesse
  
  # assign corresponding names
  assign(paste0("oaccess_wide_",i), oaccess_wide)
  #assign(paste0("oaccess_long_",i), oaccess_long)
  #rm(oaccess_wide, oaccess_long)
  rm(oaccess_wide)
  
}



##### B. Read Spatial Grids ----------

# read maps
grid_0500 <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_0500')
grid_1000 <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_1000')
grid_2000 <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_2000')
grid_4000 <- readOGR(dsn = './Spatial Grid', layer ='hex_grid_4000')
grid_trzn <- readOGR(dsn = './shapefiles_OD_rio_2012', layer ='map_grid_trzn')








###############################
# START Function ??????????????????????

# create empty output table 
mytable_jobs <- data.frame(  grid=numeric(0)
                      , pearson_cor = numeric(0)
                      , pearson_pvalue = numeric(0)
                      , Bi_Moran = numeric(0)
                      , Moran_pvalue=numeric(0))


mytable_schools <- data.frame(  grid=numeric(0)
                             , pearson_cor = numeric(0)
                             , pearson_pvalue = numeric(0)
                             , Bi_Moran = numeric(0)
                             , Moran_pvalue=numeric(0))














for (i in c('0500', '1000', '2000', '4000', 'trzn')){
  
  for (j in c('jobs', 'schools')){
  
# annouce grid
  cat("working on grid", i, j, "\n")
  
# get spatial object
  grid_map <- get(ls(pattern=paste0("grid_",i)))
  plot(  grid_map)
  
# get spatial object
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


# name of Y var
  t=60
  ifelse(j=='jobs', tempvar_ratio <- noquote( paste0("diff_ratio_oaccess_jobsmatch_",t) ) ,
                    tempvar_ratio <- noquote( paste0("diff_ratio_oaccess_schools_",t) ) )
  
  
  

# Y variables
  grid_map_sub$ratio <- grid_map_sub[, paste(tempvar_ratio)][[1]] # ratio
  #grid_map_sub$diffaccess <- grid_map_sub[, paste(tempvar_minus)][[1]] # minus
  # plot choropleth
  tmap::qtm(shp= grid_map_sub, fill = "ratio") 
  
  
plot(grid_map_sub, col="gray")
grid_map_sub <- subset(grid_map_sub, pop >0)
grid_map_sub <- subset(grid_map_sub, !is.na(ratio))
grid_map_sub <- subset(grid_map_sub, ratio != 0 )
grid_map_sub <- subset(grid_map_sub, ratio != Inf )

#grid_map_sub <- subset(grid_map_sub, !is.na(diffaccess))
grid_map_sub <- subset(grid_map_sub, !is.na(elevation))

plot(grid_map_sub, col="gray")







######## Spatial Adjancey and Weights matrix  ======================================================
cat("Spatial Adjancey and Weights matrix \n")

# Queen
nb <- poly2nb(grid_map_sub, queen=T)
lw <- nb2listw(nb, style = "B", zero.policy = T)

W  <- as(lw, "symmetricMatrix")
W  <- as.matrix(W/rowSums(W))
W[which(is.na(W))] <- 0


  
#######  6. Simple bivariate correlation  -------------------------
cat("Bivariate Moran \n")

y <- grid_map_sub@data$ratio
x <- grid_map_sub@data$RDPC
n <- length(rownames(grid_map_sub@data))




# simple correlation
  simple_cor <- stats::cor.test(x, y, use="pairwise.complete.obs", type="pearson", na.action="na.omit")

    

  simple_cor[[4]][1] # correlation
  simple_cor[[3]][1] # p-value
  
####### 7. Bivariate Moran #======================================================

# Calculating the index and its simulated distribution
# for global and local values
  
  
  
  m   <- moran_I(x, y, W)
  m[[1]] # global value
  
  m_i <- m[[2]]  # local values
  
  local_sims <- simula_moran(x, y, W)$local_sims

  
# global p-value  
  # get all simulated global moran
  global_sims <- simula_moran(x, y, W)$global_sims

  # What proportion of simulated global values are higher (in absolute value) than the actual index ?
  moran_pvalue <- sum(abs(global_sims) > abs( m[[1]][1] )) / length(global_sims)
    
###### update my output table
  
  
ifelse(j=='jobs',
  # update table with jobs results       
  mytable_jobs[nrow(mytable_jobs)+1, ] <- c(as.character(i)
                                            , simple_cor[[4]][1] # Pearson correlation
                                            , simple_cor[[3]][1] # Pearson p-value
                                            , m[[1]]             # Moran's I global
                                            , moran_pvalue        # Moran's I p-vlue
                                            ) 
  # or update table with schools results       
 , mytable_schools[nrow(mytable_jobs)+1, ] <- c(as.character(i)
                                                , simple_cor[[4]][1] # Pearson correlation
                                                , simple_cor[[3]][1] # Pearson p-value
                                                , m[[1]]             # Moran's I global
                                                , moran_pvalue        # Moran's I p-vlue
                                                ) 
                )
  


 
    
####### 7. Get Significant values -------------------------

  # Identifying the significant values 
  alpha <- .05  # for a 95% confidence interval
  probs <- c(alpha/2, 1-alpha/2)
  intervals <- t( apply(local_sims, 1, function(x) quantile(x, probs=probs)))
  sig        <- ( m_i < intervals[,1] )  | ( m_i > intervals[,2] )
  
  
  
####### 8. Prepare ploting -------------------------
  
  
# Convert shape file into sf object
  grid_map_sf     <- st_as_sf(grid_map_sub)
  grid_map_sf$sig <- sig
  

# Identifying the LISA patterns
  xp <- (x-mean(x))/sd(x)
  yp <- (y-mean(y))/sd(y)
  

  patterns <- as.character( interaction(xp > 0, W%*%yp > 0) )
  patterns <- patterns %>% 
    str_replace_all("TRUE","High") %>% 
    str_replace_all("FALSE","Low")
  
  patterns[grid_map_sf$sig==0] <- "Not significant"
  grid_map_sf$patterns <- patterns
  
  
  
# Explore meaning of each cluster
  # mean
  mean(grid_map_sf$RDPC)
  mean(grid_map_sf$diff_minus_oaccess_jobsmatch_60)
  mean(grid_map_sf$diff_minus_oaccess_schools_60)
  
  # High income, # High access
  summary(grid_map_sf$RDPC[which(grid_map_sf$patterns == "High.High")])
  summary(grid_map_sf$diff_minus_oaccess_jobsmatch_60[which(grid_map_sf$patterns == "High.High")])
  
  # low income, # low access
  summary(grid_map_sf$RDPC[which(grid_map_sf$patterns == "Low.Low")])
  summary(grid_map_sf$diff_minus_oaccess_jobsmatch_60[which(grid_map_sf$patterns == "Low.Low")])
  
  
  # High income, # low access
  summary(grid_map_sf$RDPC[which(grid_map_sf$patterns == "Low.High")])
  summary(grid_map_sf$diff_minus_oaccess_jobsmatch_60[which(grid_map_sf$patterns == "Low.High")])
  
  

# Rename LISA clusters
  #grid_map_sf$patterns2 <- grid_map_sf$patterns
  grid_map_sf$patterns2 <- factor(grid_map_sf$patterns, levels=c("High.High", "High.Low", "Low.High", "Low.Low", "Not significant"),
                                labels=c("High income - High access gain", "High income - Low access gain", "Low income - High access gain","Low income - Low access gain", "Not significant"))



# get map to global enviroment with name associated to grid scale
  assign(paste0("grid_map_sf_",i,"_",j), grid_map_sf)
  }
}
beep()




########### output table ------------------





# Export mytable from R to clipboard
write.table(mytable_jobs, "clipboard", sep="\t", row.names=FALSE)
write.table(mytable_schools, "clipboard", sep="\t", row.names=FALSE)




########### PLOT Map ----------------------  
  

# row bind all maps
  grid_map_sf_jobs <- rbind(grid_map_sf_0500_jobs, grid_map_sf_1000_jobs, grid_map_sf_2000_jobs, grid_map_sf_4000_jobs, grid_map_sf_trzn_jobs)
  grid_map_sf_schools <- rbind(grid_map_sf_0500_schools, grid_map_sf_1000_schools, grid_map_sf_2000_schools, grid_map_sf_4000_schools, grid_map_sf_trzn_schools)

# Grid Labels
    grid_map_sf_schools$grid <- if_else(grid_map_sf_schools$grid=='grid_0500', '0.5 Km',
                      if_else(grid_map_sf_schools$grid=='grid_1000', '1 Km',
                      if_else(grid_map_sf_schools$grid=='grid_2000', '2 Km',
                      if_else(grid_map_sf_schools$grid=='grid_4000', '4 Km',
                      if_else(grid_map_sf_schools$grid=='grid_trzn', 'Traffic zones', 'ERROR')))))

    grid_map_sf_jobs$grid <- if_else(grid_map_sf_jobs$grid=='grid_0500', '0.5 Km',
                              if_else(grid_map_sf_jobs$grid=='grid_1000', '1 Km',
                              if_else(grid_map_sf_jobs$grid=='grid_2000', '2 Km',
                              if_else(grid_map_sf_jobs$grid=='grid_4000', '4 Km',
                              if_else(grid_map_sf_jobs$grid=='grid_trzn', 'Traffic zones', 'ERROR')))))

    
# Load Base Map
  source("./R scripts/00_BaseMap_sf.R")
  gc(reset = T)

  
  
  
# PLOT
  plot_fun <- function(df){
    
    basemap +
      geom_sf(data=df, aes(fill=patterns2), color="NA") +
      scale_fill_manual(values = c("#c22e00", "#ebac97", "#8ed7da", "#108188", "grey80")) + 
      infra_antes + infra_atual +
      facet_wrap(~grid, ncol = 2) +
      theme_opts +
      guides(fill = guide_legend(title="LISA clusters")) +
      
      theme(
        # Legends
        legend.position=c(0.6, 0.03), # horz vert
        legend.direction='vertical',
        legend.box='vertical',
        legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        # axis
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
    
  }
  
  
  
# plot Clusters of jobs
  tempplot <-  plot_fun(grid_map_sf_jobs)
  ggsave(tempplot, file=paste0("./plots_orig3/map9_cluster_jobsmatch_60.png"), dpi = 300, width = 16.5, height = 14, units = "cm")
  
  
# plot Clusters of Schools
  tempplot <-  plot_fun(grid_map_sf_schools)
  ggsave(tempplot, file=paste0("./plots_orig3/map9_cluster_schools_60.png"), dpi = 300, width = 16.5, height = 14, units = "cm")
  

    
    # Explore meaning of each cluster
  # mean
  mean(grid_map_sf$income)
  mean(grid_map_sf_schools$diff_minus_oaccess_jobsmatch_60)
  mean(grid_map_sf_schools$diff_minus_oaccess_schools_60)
  
  