# Esse Script:

# 1 Abre Rais estabelecimentos geocodificada, fornecida por Vanessa Nadalin Ipea
# 2 Salva base pura na pasta rais
>start from 3 so you do not need to read all data
# 3 Elimina estabelecimentos repetidos na base
# 4 Espacializa Estabs poo grid cell
# 5 Salva base geocodificada com info do grid de 200 e 500
# 6 Map companies

### 1 to 5 in Ipea pc ### 
### 1 to 5 in Ipea pc ### 
### 1 to 5 in Ipea pc ### 



### 0: Set Working Directory ------------

# Ipea Server
  setwd("D:/Users/R1701707/Desktop/Dout/Data Dout")

# pc
  setwd("R:/Dropbox/Dout/Data Dout")



##################### Load packages -------------------------------------------------

source("./R scripts/00_LoadPackages.R")




## 1 Load Rais estabelecimentos geoloc da Van via Servidor Ipea ----------

# read clean data sent by Vanessa    
  estabs_2015 <- fread("./rais/rais2015estab_rj.csv", colClasses=list(character=1:11))

  # read razao social info sent by Vanessa
    razao_social <- fread("./rais/rais2015estab_rj_razao_social.csv", colClasses=list(character=1:4))
  

# replace comma with point in coordinates
  estabs_2015[, longitude := gsub(",", "\\.", longitude)][, latitude := gsub(",", "\\.", latitude)]
  head(estabs_2015)

  
# convert coordinates and number of jobs (qtd_vinc_ativos) to numeric 
  estabs_2015[, longitude := as.numeric(longitude)][, latitude := as.numeric(latitude)]
  estabs_2015[, cnpj_raiz := as.numeric(cnpj_raiz)][, id_estab := as.numeric(id_estab)][, cei_vinc := as.numeric(cei_vinc)]
  razao_social[, cnpj_raiz := as.numeric(cnpj_raiz)][, id_estab := as.numeric(id_estab)][, cei_vinc := as.numeric(cei_vinc)]
  head(estabs_2015)

  gc(reset = T)


# create unique company ID
  estabs_2015[, unique_company_id := paste(cnpj_raiz, id_estab,  cei_vinc, sep = "-")] # 00000000
  razao_social[, unique_company_id := paste(cnpj_raiz, id_estab,  cei_vinc, sep = "-")] # 00000000
  
  head(estabs_2015)
  head(razao_social)
  
# add razao social to companies dataset
  estabs_2015[razao_social, on="unique_company_id", razao_social := razao_social,  ]






##### >>>>>>>>>>>>>>>>>>>>>>> ----------------------
# 3 Elimina estabelecimentos repetidos na base  ----------



# Make sure there are nao tem empresas se repetindo com localizacao diferentes
# numero de de obs deve permanecer o mesmo
# A <- estabs_2015[ , .(unique_company_id, latitude, longitude)]
# A <- unique(A)
# head(estabs_2015)

# only companies with lat long
  estabs_2015 <- subset(estabs_2015, !is.na(latitude))
  estabs_2015 <- subset(estabs_2015, !is.na(longitude)) # 227,471 companies remaining
  estabs_2015 <- unique(estabs_2015)
    
  
  # plot geral no map
    ggplot(estabs_2015, aes(x=longitude, y=latitude)) + stat_binhex() + coord_equal()
  
    
    
# ## Correct companies location ------
#     ### Find empresas pelo CNPJ >>>> http://www.econodata.com.br/lista_empresas/RIO-DE-JANEIRO/RIO-DE-JANEIRO/C/29569621000334-COMUNIDADE-S8
#     # back from the future 666 in this script
#     # correct companies that fall on the beach of Botafogo
#     estabs_2015[ unique_company_id %in% napraia1$unique_company_id,  c("longitude", "latitude") := list(-43.18419,-22.94583)]
#     estabs_2015[ unique_company_id %in% napraia2$unique_company_id, c("longitude", "latitude") := list(-43.18183,-22.94181)]
#     estabs_2015[ unique_company_id=="2558157-2558157034067-NA", c("longitude", "latitude") := list(-43.24147,-22.94230)] # "TELEFONICA BRASIL S A": NA ESTRADA DO SUMARE, 710 RJ CEP 20261280 
#     estabs_2015[ unique_company_id=="6248349-6248349002509-NA", c("longitude", "latitude") := list(-43.21773,-22.94698)] # "TRANSPORTADORA ASSOCIADA DE GAS SA" CEP 20531540 
#     estabs_2015[ unique_company_id=="2709449-2709449001040-NA", c("longitude", "latitude") := list(-43.21773,-22.94698)] # "PETROBRAS TRANSPORTE S A" CEP 20531540 
#     estabs_2015[ unique_company_id=="29569621-29569621000334-NA", c("longitude", "latitude") := list(-43.71053,-22.89262)] # COMUNIDADE S8, >> http://www.econodata.com.br/lista_empresas/RIO-DE-JANEIRO/RIO-DE-JANEIRO/C/29569621000334-COMUNIDADE-S8
#         
    
    
    
#### convert Companies data set into a spatial points data frame
    
    myprojection_latlong <- CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    
    # rename variables Lat Long
      setnames(estabs_2015, "latitude", "lat")
      setnames(estabs_2015, "longitude", "long")
    
      
      

    # Convert DF to a SpatialPointsDataFrame
      coordinates(estabs_2015) <- c("long", "lat")
      estabs_2015@proj4string <- myprojection_latlong
      estabs_2015 <- spTransform(estabs_2015, myprojection_latlong)
    
      
    
  # keep only those companies within Rio municipaltity
    muni <- readOGR(dsn="./Shapes_IBGE", layer="muni")      #read shape file
    muni <- spTransform(muni, myprojection_latlong)
    # filter
    estabs_2015 <- estabs_2015[muni, ] # 227.332 companies remaining 
    plot(estabs_2015)
    plot(muni, add=T)
  
  
  
    
    
    
    
## 4 Add Spatial Variables to Companies database (Lat Long id200, ID200) ------


# Load Spatial Grids
  gridIBGE200 <- readOGR(dsn="./Shapes_IBGE", layer="gridIBGE200")      #read shape file
  gridIBGE200 <- spTransform(gridIBGE200, myprojection_latlong)
  gc(reset=T)
  


# Overlay Grid Files with Data of Companies

  # check equal projection
  proj4string(estabs_2015) == proj4string(gridIBGE200)
  
  
  ### point in grid FIRST round of two
  # intersection
  points <- point.in.poly(estabs_2015, gridIBGE200)


# Convert to Data Frame
  estabs_2015_grid <- as.data.frame(points) %>% setDT()
  head(estabs_2015_grid) # Check Variables


  
  #### TEST na praia 666

  # estabs na praia
  napraia1 <- estabs_2015_grid[ id200 %in% c(29866)] # 29866 29871
  napraia2 <- estabs_2015_grid[ id200 %in% c(29871)] # 29866 29871

  # # quick ggmap
  #   gmapRio <- get_map("Rio de Janeiro", zoom=12)
  #   ggmap( gmapRio ) + geom_point(data=napraia2, aes(x=long, y=lat), color="red")
  
  
    ## Correct companies location ------
    ### Find empresas pelo CNPJ >>>> http://www.econodata.com.br/lista_empresas/RIO-DE-JANEIRO/RIO-DE-JANEIRO/C/29569621000334-COMUNIDADE-S8
    # back from the future 666 in this script
    # correct companies that fall on the beach of Botafogo
    estabs_2015_grid[ unique_company_id %in% napraia1$unique_company_id,  c("long", "lat") := list(-43.18419,-22.94583)]
    estabs_2015_grid[ unique_company_id %in% napraia2$unique_company_id, c("long", "lat") := list(-43.18183,-22.94181)]
    estabs_2015_grid[ unique_company_id=="2558157-2558157034067-NA", c("long", "lat") := list(-43.24147,-22.94230)] # "TELEFONICA BRASIL S A": NA ESTRADA DO SUMARE, 710 RJ CEP 20261280 
    estabs_2015_grid[ unique_company_id=="6248349-6248349002509-NA", c("long", "lat") := list(-43.21773,-22.94698)] # "TRANSPORTADORA ASSOCIADA DE GAS SA" CEP 20531540 
    estabs_2015_grid[ unique_company_id=="2709449-2709449001040-NA", c("long", "lat") := list(-43.21773,-22.94698)] # "PETROBRAS TRANSPORTE S A" CEP 20531540 
    estabs_2015_grid[ unique_company_id=="29569621-29569621000334-NA", c("long", "lat") := list(-43.71053,-22.89262)] # COMUNIDADE S8, >> http://www.econodata.com.br/lista_empresas/RIO-DE-JANEIRO/RIO-DE-JANEIRO/C/29569621000334-COMUNIDADE-S8
    
    estabs_2015_grid[, c('area', 'grid', 'id200') := NULL ]

### point in grid SECOND round
    coordinates(estabs_2015_grid) <- c("long", "lat")
    estabs_2015_grid@proj4string <- myprojection_latlong
    estabs_2015_grid <- spTransform(estabs_2015_grid, myprojection_latlong)
    # check equal projection
    proj4string(estabs_2015_grid) == proj4string(gridIBGE200)
    
    
    # intersection
    points <- point.in.poly(estabs_2015_grid, gridIBGE200)
    
    
    # Convert to Data Frame
    estabs_2015_grid <- as.data.frame(points) %>% setDT()
    head(estabs_2015_grid) # Check Variables
    
  
# Keep only essential variables to latter merge spatial info with individual records
  keep <- c("unique_company_id", "lat", 'long', "id200", "ID_UNICO", "razao_social") # 
  estabs_2015_grid <- estabs_2015_grid[, keep, with=FALSE]
  head(estabs_2015_grid)  # Check Variables
  
  estabs_2015_grid <- unique(estabs_2015_grid)

# quick map
  ggplot(estabs_2015_grid, aes(x=long, y=lat)) + stat_binhex() + coord_equal()
  
  # OR
  temp_plot <- estabs_2015_grid[, .(count=.N, lat=lat[1L], long=long[1L]), by=id200]
  ggplot(temp_plot, aes(x=long, y=lat, color=count)) + geom_point(alpha=0.8) + coord_equal()

  
  
  
  
# 5 Salva base geocodificada com info do grid de 200 e 500  -----------

    fwrite(estabs_2015_grid, file="./rais/rais2015estab_rj_grid.csv")
