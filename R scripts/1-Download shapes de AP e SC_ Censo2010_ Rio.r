pc

#This script downloads the shape files from IBGE 2010 population Census, Brazil
  ###### 0: Load / Install Packages
  ###### 1: Download and unzip files
  ###### 2: Load/Save Shape Municipio
  ###### 3: Load/Save Shape Area de Ponderacao - sampling Areas
  ###### 4: Load/Save Shape Setor Censitario - Census tracts
  ###### 5: Load/Save Shape gridIBGE200 mestros
  ###### 6: Plot all shape files to check if they are correct


# Set Working Directory
  # Ipea Server
  setwd("D:/Users/R1701707/Desktop/Dout/Shapes_IBGE")

  # Lap Top pc
  setwd("R:/Dropbox/Dout/Data Dout")
  
      

##################### Load packages -------------------------------------------------

  source("./R scripts/00_LoadPackages.R")
  

###### 1: Download and unzip IBGE shape files ----------------------
 
  # Municipio
  download.file("ftp://geoftp.ibge.gov.br/malhas_digitais/censo_2010/setores_censitarios/rj/rj_municipios.zip", "Municipios_2010_Rio.zip", quiet = FALSE)
  unzip("Municipios_2010_Rio.zip", exdir="Municipios_2010_Rio")
  
# Area de Ponderacao
  download.file("ftp://geoftp.ibge.gov.br/malhas_digitais/censo_2010/areas_de_ponderacao/municipios_areas_redefinidas/Rio_de_Janeiro.zip", "AreasPonderacao_2010_Rio.zip", quiet = FALSE)
  unzip("AreasPonderacao_2010_Rio.zip", exdir="AreasPonderacao_2010_Rio")
  
# Setores Censitarios
  download.file("ftp://geoftp.ibge.gov.br/malhas_digitais/censo_2010/setores_censitarios/rj/rj_setores_censitarios.zip", "SetorCensitario_2010_Rio.zip", quiet = FALSE)
  unzip("SetorCensitario_2010_Rio.zip", exdir="SetorCensitario_2010_Rio")
  
  
# Download IBGE Grid 200m x 200m
  download.file("ftp://geoftp.ibge.gov.br/recortes_para_fins_estatisticos/grade_estatistica/censo_2010/grade_id26.zip", "gridIBGE_Rio200m.zip", quiet = FALSE)
  unzip("gridIBGE_Rio200m.zip", exdir="gridIBGE_Rio200m")
  beep()
  
  
  
###### 2: Load/Save Shape Municipio ----------------------
  
  # Load Municipio shape file
  state <- readOGR(dsn="./Municipios_2010_Rio", layer="33MUE250GC_SIR")      #read shape file
  
  #Subset only census tracts from Rio Municipality
  muni <- state[state$NM_MUNICIP == "RIO DE JANEIRO", ]
  plot(muni) #plot map
  plot(state) #plot map
  
  
  
# # Assign projection
   proj4string(muni) # Check current projection
   muni <- spTransform(muni, CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
   state <- spTransform(state, CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
   
   
  # Save Municipality Shape File
  writeOGR(muni, dsn = '.', layer ='muni', driver = 'ESRI Shapefile')
  
  
    
    
###### 2.1: Load/Save Shape ESTADO + muni Rio ----------------------
    
# Dissolve all polygons but the municipality of Rio
    
    # Select Muni Rio 
    selected_NM_MUNICIP_indices = which(state@data$NM_MUNICIP == "RIO DE JANEIRO")
    
    # Dissolve state
    state_dissolved <- gUnaryUnion(state, id = state@data$state)
    plot(state_dissolved)
    
    state_r = state_dissolved
    npolygons = 1
    for (selected_NM_MUNICIP_index in selected_NM_MUNICIP_indices){
      state_r@polygons[[npolygons+1]] = state@polygons[[selected_NM_MUNICIP_index]]
      
      npolygons = npolygons + 1
      state_r@plotOrder=c(state_r@plotOrder,as.integer(npolygons))
    }
    
    plot(state_r)
  
  
  # convert spatialpolygon to a SaptialPolygonsDataFrame \\\ http://gis.stackexchange.com/questions/141469/how-to-convert-a-spatialpolygon-to-a-saptialpolygonsdataframe-and-add-a-column-t
    
    
    # Extract polygon ID's
    ( pid <- sapply(slot(state_r, "polygons"), function(x) slot(x, "ID")) )
    
    # Create dataframe with correct rownames
    ( p.df <- data.frame( ID=1:length(state_r), row.names = pid) )    
    
    # Try coersion again and check class
    state <- SpatialPolygonsDataFrame(state_r, p.df)
    class(state)
    plot(state)
    
    
    # Save State Shape File
    writeOGR(state, dsn = '.', layer ='state', driver = 'ESRI Shapefile')
    
 
    
###### 2.2: Load/Save Shape Rio Metro area  ----------------------
    
    
    state_metro <- readOGR(dsn="./Shapes_IBGE/regiao_metro", layer="regiao_metro_rio")  
    metro <- muni <- state_metro[state_metro$RMRJ == "S", ]
    
# seleciona apenas adjacents
    # metro@data$touch <-    gTouches(metro, byid=TRUE, returnDense=FALSE)
    # 
    # 
    # nb2mat(metro, style="B")
    
    
    
    
    
    
    
###### 3: Load/Save Shape Area de Ponderacao - Sampling Areas ----------------------

  # Load Area Pond shape file
    areapond <- readOGR(dsn="./AreasPonderacao_2010_Rio", layer="Rio de Janeiro_area de ponderacao ")      #read shape file
  
  # Assign projection
    proj4string(areapond) # Check current projection
    wgs.84 <- "+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    proj4string(areapond) <- CRS(wgs.84)
    areapond <- spTransform(areapond, CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    
    plot(areapond) #plot map
    
  # Save Area de Ponderacao
    writeOGR(areapond, dsn = '.', layer ='areapond', driver = 'ESRI Shapefile')
  
    
    b <- gUnaryUnion(areapond, id = areapond@data$CD_GEOCODM)
    plot(areapond)
  

    
    
    
    
    
###### 4: Load/Save Shape Setor Censitario - Census tracts ----------------------
    
  # Load Setor Censitario shape file
    setorcens <- readOGR(dsn="./SetorCensitario_2010_Rio", layer="33SEE250GC_SIR")      #read shape file
  
  # Subset only census tracts from Rio Municipality
    setorcens <- setorcens[setorcens$NM_MUNICIP == "RIO DE JANEIRO", ]
  
  # Assign projection
    proj4string(setorcens) # Check current projection
    setorcens <- spTransform(setorcens, CRS("+proj=longlat +zone=23 +datum=WGS84"))
    plot(setorcens)
    
#   setorcens@data$id = rownames(setorcens@data)              #add variable with polygons IDs
#   setorcens.points = fortify(setorcens, region="CD_GEOCODI")        #convert shape into data frame (DF)
#   setorcens.df = left_join(setorcens.points, setorcens@data, by = c("id" = "CD_GEOCODI")); remove(setorcens.points) #Adiciona ao DF todas variaveis do shape
  
  # Save subset with Census tracts of Rio Municipality
  writeOGR(setorcens, dsn = '.', layer ='setorcens', driver = 'ESRI Shapefile')
  

  
  
  
  
  
  
###### 5: Load/Save Shape gridIBGE200 metros    ------------

# load muni
  muni <- readOGR(dsn="./Shapes_IBGE", layer="muni")      #read shape file
  
# read original grid
  gridIBGE <- readOGR(dsn="./Shapes_IBGE/gridIBGE_Rio200m", layer="grade_id26")      #read shape file
  head(gridIBGE@data)
  beep()
  
  # reduce number of variables
    gridIBGE@data$nome_1KM <- NULL
    gridIBGE@data$nome_10KM <- NULL
    gridIBGE@data$nome_100KM <- NULL
    gridIBGE@data$nome_5KM <- NULL
    gridIBGE@data$nome_50KM <- NULL
    gridIBGE@data$nome_500KM <- NULL
    gridIBGE@data$Shape_Area <- NULL
    gridIBGE@data$Shape_Leng <- NULL
    
    
# lat long projection
  gridIBGE <- spTransform(gridIBGE, CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  muni <- spTransform(muni, CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  proj4string(muni)
  proj4string(gridIBGE)
  beep()
  
  # intersect
  gridIBGE200 <- gridIBGE[muni, ]
  # apagar gridIBGE200 <- intersect(muni, gridIBGE) # gridcanvas gridcanvaspoly gridDF
  
  # Create and ID variable of Grid Cells 
  gridIBGE200@data$id200 <- as.numeric( 1:nrow(gridIBGE200@data) )
  
  # check total population
  sum(gridIBGE200@data$POP)
  

    
  # # delete columns we won't use
  # gridIBGE200@data$NM_MUNICIP <- NULL
  # gridIBGE200@data$CD_GEOCODM <- NULL
  # gridIBGE200@data$QUADRANTE <- NULL
  # gridIBGE200@data$ID <- NULL
   
  # check data structure
  head(gridIBGE200@data) # Check Grid Variables
  
  
  # Save Grid as Shape File
  writeOGR(gridIBGE200, dsn = './Shapes_IBGE', layer ='gridIBGE200', driver = 'ESRI Shapefile', overwrite_layer=T)
  
  
###################################3
  #SOH grid cell com pop
  gridIBGE200.pop <- gridIBGE200[gridIBGE200@data$POP >0, 
                                 gridIBGE200.df <- setDT(as.data.frame(gridIBGE200.pop))
                                 
                                 
  
  
  
###### 6: Plot all shape files   ------------
  
# read shape files
  muni <- readOGR(dsn=".", layer="muni")      #read shape file
  areapond <- readOGR(dsn=".", layer="areapond")      #read shape file
  setorcens <- readOGR(dsn=".", layer="setorcens")      #read shape file
  gridIBGE200 <- readOGR(dsn=".", layer="gridIBGE200")      #read shape file
  
# Use the same projection
  muni <- spTransform(muni, CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  areapond <- spTransform(areapond, CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  setorcens <- spTransform(setorcens, CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  gridIBGE200 <- spTransform(gridIBGE200, CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  
  # Plot all boundaries
  ggplot() +
    geom_polygon(data = muni, aes(x=long, y=lat, group = group), color="NA",fill="gray20", alpha=I(.7)) +
    geom_polygon(data = setorcens, aes(x=long, y=lat, group = group), color="green",fill="NA") +
    geom_polygon(data = areapond, aes(x=long, y=lat, group = group), color="red",fill="NA") +
    geom_polygon(data = gridIBGE200, aes(x=long, y=lat, group = group), color="blue",fill="NA") 
  
  # 
  ggplot(setorcens.df) + 
    aes(long,lat,group=group,fill=NM_BAIRRO) + 
    geom_polygon() +
    geom_path(color="white") +
    coord_equal() +
    scale_fill_brewer("Utah Ecoregion") # max 9 colors
  
  
  

  
  
  
    
