http://rpubs.com/PaulWilliamson/6577
https://github.com/Robinlovelace/spatial-microsim-book



# This Script:
# (a) patializes Socio-Economic data from the Population Census UDHs to Grid Cells, and
# (b) creates the data set of each grid resolution


# 1 Add Socio-economid data from UDHs to Grid Cells 
# 2 Aggregate (reaportion) UDH data to cell level
# 3 Compute Income Deciles
# 4 Save gridDATA 








# Set working directory to SAVE plots
setwd("R:/Dropbox/Dout/Data Dout")





##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")





#################################################################################################
########## 1 Add Socio-economic data from UDHs to Grid Cells ------------------------------------

# 0 read spatialkey
  spatialkey <- fread("./Spatial KEY/spatialkey_phd.csv")
  head(spatialkey)


  
# 1 Distribute UDH data to grid200, mas mantem pop de CT
  

  # 0.1 Get UDHS data
    udh_censusdata <- fread("./UDHS censo2010/RM 633 Rio de Janeiro - Base UDH 2000_2010.csv", colClasses=list(character=1:3))
    
    # keep Only info from 2010 Census
      udh_censusdata <- udh_censusdata[ ANO == "2010" & CODMUN6 == "330455",] 
    
      # select variables
        udh_censusdata <- udh_censusdata[, .(Cod_ID, CODMUN6, ANO, PESO18, PESOTOT, T_FUND25M, T_MED25M, T_SUPER25M, GINI, PMPOB,RDPC, RDPCT, P_FUND, P_MED, P_SUPER, T_DES18M, PEA, P_FORMAL, TRABCC, TRABPUB, TRABSC, IDHM)]
        
        # ANO         Ano do censo
        # CODMUN6     C?digo do Munic?pio
        # Cod_ID      Codigo UDH
        # PESOTOT     Popula??o total
        # PESO18      Popula??o de 18 anos ou mais
        # PEA         PEA - 10 anos ou mais
        # RDPC	      Renda Domiciliar per capita media
        # RDPCT	      Renda Domiciliar per capita media, exceto renda nula
        # T_FUND25M   % de 25 anos ou mais com fundamental completo
        # T_MED25M	  % de 25 anos ou mais com m?dio completo
        # T_SUPER25M  % de 25 anos ou mais com superior completo
        # GINI        ?ndice de Gini
        # PMPOB       % de pobres
        # P_FORMAL	  Grau de formaliza??o dos ocupados - 18 anos ou mais
        # P_FUND	    % dos ocupados com fundamental completo - 18 anos ou mais
        # P_MED	      % dos ocupados com m?dio completo - 18 anos ou mais
        # P_SUPER	    % dos ocupados com superior completo - 18 anos ou mais
        # T_DES18M	  Taxa de desocupa??o - 18 anos ou mais
        # TRABCC	    % de empregados com carteira - 18 anos ou mais
        # TRABPUB	    % de trabalhadores do setor p?blico - 18 anos ou mais
        # TRABSC	    % de empregados sem carteira - 18 anos ou mais
        # IDHM        IDH level

      
    # Compare Total popultion of Rio municipality Gridibge vs UDH
      sum(spatialkey$POP, na.rm=T) #  from Grid Source - 6.148.379 (exclui algumas ilhas)
      sum(udh_censusdata$PESOTOT) # from udh source - 6,320,446 (pega TODO municipio)
      
    # check IDH inequalities in Rio
      summary(udh_censusdata$IDHM)
      summary(udh_censusdata$GINI)
      

# 0.3 MERGE udh data into Spatial key
    class(spatialkey$UDH_ATLAS)
    class(udh_censusdata$Cod_ID)
    spatialkey[, UDH_ATLAS := as.character(UDH_ATLAS)]
    
    DATA <- left_join( spatialkey, udh_censusdata, by=c("UDH_ATLAS"="Cod_ID"))
    
# Change names of variables
    setDT(DATA)
    setnames(DATA,"UDH_ATLAS","udhid") # id UDHs
    setnames(DATA,"POP","pop_grid") # population from census trancts data
    setnames(DATA,"PESOTOT","pop_udh") # population from UDH data

    
# compare total population from UDH and Grid 200m sources

  sum(DATA$pop_grid)                                         # 6.148.379 grid200 \\\\ 6,320,446 , ct source
  sum( DATA[, .(pop_udh[1L]),by=.(udhid)][[2]] ,na.rm = T)   # 6.233.959 , udh source


  # Clean Global Env.
  gc(reset = T)
  
  
  


  
# 2 Aggregate (reaportion) UDH data to GRID  --------------------
  
#DATA[, N := .N, by = udhid] # get number of CTs in each UDH
  
  # Reaportion function
  reaportion <- function(byy, aarea) { 
    
                                     temp <- DATA[, .( #udhid=udhid[1L],
                                                  X = weighted.mean(X200, w=(pop_grid+1), na.rm=T), # get population-weighted centroid
                                                  Y = weighted.mean(Y200, w=(pop_grid+1), na.rm=T), # get population-weighted centroid
                                                  elevation = mean (elevation, na.rm=T) ,
                                                  #area= get(aarea)[1L],
                                                  pop = sum(pop_grid, na.rm = T),                           # pop total
                                                  # masc = sum(MASC, na.rm = T),                              # pop men
                                                  # fem = sum(FEM, na.rm = T),                                # pop women
                                                  income = sum(RDPC*pop_grid)/sum(pop_grid),              # renda media
                                                  RDPC = weighted.mean(RDPC, w=(pop_grid+1), na.rm=T),      # renda dom per capita media
                                                  prop_poor = sum(PMPOB/100*pop_grid)/sum(pop_grid),      # prop poor
                                                  unemployment = sum(T_DES18M/100*pop_grid)/sum(pop_grid),# prop poor
                                                  # T_FUND25M = sum(T_FUND25M/100*pop_grid)/sum(pop_grid),  # % de 25 anos ou mais com fundamental completo
                                                  # T_MED25M = sum(T_MED25M/100*pop_grid)/sum(pop_grid),    # 
                                                  # T_SUPER25M = sum(T_SUPER25M/100*pop_grid)/sum(pop_grid),# 
                                                  P_FORMAL = sum(P_FORMAL/100*pop_grid)/sum(pop_grid),    # 
                                                  unemployment_fun = sum(P_FUND/100*pop_grid)/sum(pop_grid),  # 
                                                  unemployment_med = sum(P_MED/100*pop_grid)/sum(pop_grid),   # 
                                                  unemployment_sup = sum(P_SUPER/100*pop_grid)/sum(pop_grid),# 
                                                  hospitals= sum(hospitals, na.rm = T),
                                                  hosp_low = sum(hosp_low, na.rm=T),
                                                  hosp_med = sum(hosp_med, na.rm=T),
                                                  hosp_high = sum(hosp_high, na.rm=T),
                                                  schools = sum(schools, na.rm = T),
                                                  totaljobs = sum(totaljobs, na.rm = T),
                                                  edubas = sum(edubas, na.rm = T),
                                                  edumed = sum(edumed, na.rm = T),
                                                  edusup = sum(edusup, na.rm = T)),
                                             by = .(get(byy))]
                                     
                                     setnames(temp, "get",  "ID")

                                     return(temp)
                                     }
  
  

# Apply reaportion function to different grid resolutions 
  gridDaTA_0200 <- reaportion(byy="id200")
  gridDaTA_0500 <- reaportion(byy="id_0500" )
  gridDaTA_1000 <- reaportion(byy="id_1000" )
  gridDaTA_2000 <- reaportion(byy="id_2000" )
  gridDaTA_4000 <- reaportion(byy="id_4000" )
  gridDaTA_trzn <- reaportion(byy="id_trzn" )

  
  ggplot() + 
    geom_point(data=gridDaTA_0500, aes(x=X, y=Y, color=elevation)) + 
    coord_equal() + 
    scale_colour_gradientn(colours = terrain.colors(10))

# remove NA (tehre are some 200m grid cells in the periphery that fall outside municipality boundary)
  gridDaTA_0200 <-  gridDaTA_0200[ !is.na( ID ), ]
  gridDaTA_0500 <-  gridDaTA_0500[ !is.na( ID ), ]
  gridDaTA_1000 <-  gridDaTA_1000[ !is.na( ID ), ]
  gridDaTA_2000 <-  gridDaTA_2000[ !is.na( ID ), ]
  gridDaTA_4000 <-  gridDaTA_4000[ !is.na( ID ), ]
  gridDaTA_trzn <-  gridDaTA_trzn[ !is.na( ID ), ]

  
# Clean Global Env.
  gc(reset = T)

  
  summary(gridDaTA_0500$income)
  summary(gridDaTA_0500$RDPC)
  
####### 3 Compute Income Deciles based on household income per capita --------------------


# get the intervals of population-weighted decile
q200  <- wtd.quantile(gridDaTA_0200$RDPC, weights=gridDaTA_0200$pop, probs=c( seq(0.1 , 1 , 0.1) ),
                   type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'),
                   normwt=FALSE, na.rm=T)

q500hex  <- wtd.quantile(gridDaTA_0500$RDPC, weights=gridDaTA_0500$pop, probs=c( seq(0.1 , 1 , 0.1) ), 
                      type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                      normwt=FALSE, na.rm=T)

q1000hex  <- wtd.quantile(gridDaTA_1000$RDPC, weights=gridDaTA_1000$pop, probs=c( seq(0.1 , 1 , 0.1) ), 
                          type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                          normwt=FALSE, na.rm=T)

q2000hex  <- wtd.quantile(gridDaTA_2000$RDPC, weights=gridDaTA_2000$pop, probs=c( seq(0.1 , 1 , 0.1) ), 
                          type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                          normwt=FALSE, na.rm=T)

q4000hex  <- wtd.quantile(gridDaTA_4000$RDPC, weights=gridDaTA_4000$pop, probs=c( seq(0.1 , 1 , 0.1) ), 
                           type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                           normwt=FALSE, na.rm=T)
  
qtraffzone  <- wtd.quantile(gridDaTA_trzn$RDPC, weights=gridDaTA_trzn$pop, probs=c( seq(0.1 , 1 , 0.1) ), 
                          type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                          normwt=FALSE, na.rm=T)

  
# Check income distribution in each decile
  barplot(q200)
  barplot(q500hex)
  barplot(q1000hex)  
  barplot(q2000hex)  
  barplot(q4000hex)  
  barplot(qtraffzone) # hist(qtraffzone) 
  


# create varible in data gridDATA
  gridDaTA_0200[ , decile := 1+ findInterval( income , q200[ -length( q200 ) ] ) ]
  gridDaTA_0500[ , decile := 1 + findInterval( income , q500hex[ -length( q500hex ) ] ) ]
  gridDaTA_1000[ , decile := 1 + findInterval( income , q1000hex[ -length( q1000hex ) ] ) ]
  gridDaTA_2000[ , decile := 1 + findInterval( income , q2000hex[ -length( q2000hex ) ] ) ]
  gridDaTA_4000[ , decile := 1 + findInterval( income , q4000hex[ -length( q4000hex ) ] ) ]
  gridDaTA_trzn[ , decile := 1 + findInterval( income , qtraffzone[ -length( qtraffzone ) ] ) ]
     


# check if pop size in each decile are roughly equal
  gridDaTA_0200[, .(po_in_decile = sum(pop, na.rm=T)), by = decile]
  gridDaTA_0500[, .(po_in_decile = sum(pop, na.rm=T)), by = decile]
  gridDaTA_1000[, .(po_in_decile = sum(pop, na.rm=T)), by = decile]
  gridDaTA_2000[, .(po_in_decile = sum(pop, na.rm=T)), by = decile]
  gridDaTA_4000[, .(po_in_decile = sum(pop, na.rm=T)), by = decile]
  gridDaTA_trzn[, .(po_in_decile = sum(pop, na.rm=T)), by = decile]
  
 
  # average income in each decile 
  gridDaTA_0500[, .(m = weighted.mean(x=income, w=pop, na.rm=T)), by = decile]
  
  gridDaTA_0500[, .(m = weighted.mean(x=income, w=pop, na.rm=T))]
  
  
# check gini of decile
 ineq(gridDaTA_0200$decile,type="Gini", na.rm = T)  
 ineq(gridDaTA_0500$decile,type="Gini", na.rm = T)  
 ineq(gridDaTA_1000$decile,type="Gini", na.rm = T)  
 ineq(gridDaTA_4000$decile,type="Gini", na.rm = T)  
 ineq(gridDaTA_trzn$decile,type="Gini", na.rm = T)  
 
  
# # convert data to numeric
#   gridDaTA_0200 <- gridDaTA_0200[, lapply(.SD, as.numeric)]
#   gridDaTA_0500 <- gridDaTA_0500[, lapply(.SD, as.numeric)]
#   gridDaTA_1000 <- gridDaTA_1000[, lapply(.SD, as.numeric)]
#   gridDaTA_2000 <- gridDaTA_2000[, lapply(.SD, as.numeric)]
#   gridDaTA_4000 <- gridDaTA_4000[, lapply(.SD, as.numeric)]
#   gridDaTA_trzn <- gridDaTA_trzn[, lapply(.SD, as.numeric)]
  
  head(gridDaTA_0500)


# ISSO aqui soh se mapa tiver com problema
# # If proportion of poor == NA, then impute 0
# gridDaTA_0500[is.na(gridDaTA_0500$prop_poor)]<- 0.00000000001
# gridDaTA_0500[prop_poor == 0, prop_poor := 0.00000000001]



######### 4 Save gridDATA   --------------------

fwrite(gridDaTA_0200, "./Spatial Grid/gridDaTA_0200.csv")
fwrite(gridDaTA_0500, "./Spatial Grid/gridDaTA_0500.csv")
fwrite(gridDaTA_1000, "./Spatial Grid/gridDaTA_1000.csv")
fwrite(gridDaTA_2000, "./Spatial Grid/gridDaTA_2000.csv")
fwrite(gridDaTA_4000, "./Spatial Grid/gridDaTA_4000.csv")
fwrite(gridDaTA_trzn, "./Spatial Grid/gridDaTA_trzn.csv")















####################### 5. Map tests ----------------------

# read data
  gridDaTA_0200 <- fread("./Spatial Grid/gridDaTA_0200.csv")
  gridDaTA_0500 <- fread("./Spatial Grid/gridDaTA_0500.csv")
  gridDaTA_1000 <- fread("./Spatial Grid/gridDaTA_1000.csv")
  gridDaTA_2000 <- fread("./Spatial Grid/gridDaTA_2000.csv")
  gridDaTA_4000 <- fread("./Spatial Grid/gridDaTA_4000.csv")
  gridDaTA_trzn <- fread("./Spatial Grid/gridDaTA_trzn.csv")
  
  # # convert id to character to allow join
  #   gridDaTA_0200[, ID := as.character( ID ) ]
  #   gridDaTA_0500[, ID := as.character( ID ) ]
  #   gridDaTA_1000[, ID := as.character( ID ) ]
  #   gridDaTA_2000[, ID := as.character( ID ) ]
  #   gridDaTA_4000[, ID := as.character( ID ) ]
  #   gridDaTA_trzn[, ID := as.character( ID ) ]
    
  
    
      
    
# read maps
    muni <- st_read(dsn = './Shapes_IBGE', layer ='muni')
    
  grid_200    <- st_read(dsn = './Shapes_IBGE', layer ='gridIBGE200')
  hex_0500 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_0500')
  hex_1000 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_1000')
  hex_2000 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_2000')
  hex_4000 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_4000')
  map_trzn <- st_read(dsn = './shapefiles_OD_rio_2012', layer ='map_grid_trzn')
  
  
  
  # lat long and utm projections
    myprojection_latlong <-  "+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
    muni <- st_transform(muni, myprojection_latlong)
    grid_200 <- st_transform(grid_200, myprojection_latlong)
    hex_0500 <- st_transform(hex_0500, myprojection_latlong)
    hex_1000 <- st_transform(hex_1000, myprojection_latlong)
    hex_2000 <- st_transform(hex_2000, myprojection_latlong)
    hex_4000 <- st_transform(hex_4000, myprojection_latlong)
    map_trzn <- st_transform(map_trzn, myprojection_latlong)
    beep()
    
    

# merge
  hex_0500 <- left_join( hex_0500 , gridDaTA_0500 )
  hex_1000 <- left_join( hex_1000 , gridDaTA_1000 )
  hex_2000 <- left_join( hex_2000 , gridDaTA_2000 )
  hex_4000 <- left_join( hex_4000 , gridDaTA_4000 )
  map_trzn <- left_join( map_trzn , gridDaTA_trzn )
    

        
  
  
### PLOT MAP ----

# pop
  # 200
  # ggplot(data= subset(grid_200, POP >0)) +
  #   geom_sf(data= muni,  fill=NA, color="gray70") +
  #   geom_sf(data= subset(grid_200, POP >0), aes(fill=POP ), color=NA) +
  #   scale_fill_distiller( palette="Oranges", guide = "colorbar", name="Population\nDensity", direction = 1) +
  #   theme_map()
  
pop_plot <- function(xxx){   ggplot(data= subset(xxx, pop >0)) +
                                geom_sf(data= muni,  fill=NA, color="gray70") +
                                geom_sf( aes(fill=pop/area ), color="gray70") +
                              #  geom_point( aes(x=X,y=Y), color="red", size=.2) +
                                scale_fill_distiller( palette="Oranges", guide = "colorbar", name="Population\nDensity", direction = 1) +
                                theme_map() } 

pop_plot(hex_0500)
pop_plot(hex_1000)
pop_plot(hex_2000)
pop_plot(hex_4000)
pop_plot(map_trzn)



    
  
  
# decile
  income_plot <- function(xxx){ ggplot(data= subset(xxx, pop >0)) +
                                  geom_sf(data= muni,  fill=NA, color="gray70") +
                                  geom_sf( aes(fill=factor(decile) ), color="gray70") +
                                #  geom_point( aes(x=X,y=Y), color="red", size=.2) +
                                  scale_fill_brewer( palette="RdBu", guide = "legend",  name="Income decile") +
                                  theme_map() } 
                        
income_plot(hex_0500)
income_plot(hex_1000)
income_plot(hex_2000)
income_plot(hex_4000)
income_plot(map_trzn)
     


                 
# jobs
  
jobs <- sum(gridDaTA_0500$totaljobs)
jobs_plot <- function(xxx){   ggplot(data= subset(xxx, totaljobs >0)) +
                                      geom_sf(data= muni,  fill=NA, color="gray70") +
                                      geom_sf( aes(fill=totaljobs/jobs*100, labels=get(names(xxx)[1])), color="gray70") +
                                      #geom_point( aes(x=X,y=Y), color="red") +
                                      scale_fill_distiller( palette="Oranges", guide = "colorbar", name="Population\nDensity", direction = 1) +
                                      theme_map() } 

jobs_plot(hex_0500)
jobs_plot(hex_1000)
jobs_plot(hex_2000)
jobs_plot(hex_4000)
jobs_plot(map_trzn)

ggplotly()

# prai de Botafogo (faix de areia)
gridDaTA_0500[ idhex500== 2223]

  
# check location of centroids
  a <- as.data.frame(map_trzn)
  coordinates(a) = ~X+Y
  proj4string(a) <- CRS (myprojection_latlong)

  # interactive map
  mapView(map_trzn) + a


  
  
  
  
  
  
  
  
  