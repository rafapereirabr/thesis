

######## ////// Base MAP   ---------------------


##### B. Read SHAPE FILEs ----------



state <- st_read(dsn = './Shapes_IBGE', layer ='state')
muni <- st_read(dsn = './Shapes_IBGE', layer ='muni')
streets <- st_read(dsn = './OSM_rio-de-janeiro', layer ='osm_roads')




# brt <- st_read(dsn="./shapes_brt_transbrasil", layer="Trajetos_BRT__Visualizacao")      #read shape file
# brt_partial <- st_read(dsn="./shapes_brt_transbrasil", layer="Trajetos_BRT__Visualizacao_partial")      #read shape file
# metrolinha4 <- st_read(dsn = './Shapes_BRT/RJ_TP_other', layer ='Lines_Metro_L4')
# metro <- st_read(dsn = './Shapes_BRT/RJ_TP_other', layer ='Lines_Metro') 
# supervia <- st_read(dsn = './Shapes_BRT/RJ_TP_other', layer ='Lines_Supervia')  
# vlt <- st_read(dsn = './Shapes_BRT/RJ_TP_other', layer ='Lines_VLT')  

brt <- readOGR(dsn="./shapes_brt_transbrasil", layer="Trajetos_BRT__Visualizacao")     %>% st_as_sf()
brt_partial <- readOGR(dsn="./shapes_brt_transbrasil", layer="Trajetos_BRT__Visualizacao_partial")     %>% st_as_sf()

 metrolinha4 <- readOGR(dsn = './Shapes_BRT/RJ_TP_other', layer ='Lines_Metro_L4') %>% st_as_sf()
 metro <- readOGR(dsn = './Shapes_BRT/RJ_TP_other', layer ='Lines_Metro')   %>% st_as_sf()
 supervia <- readOGR(dsn = './Shapes_BRT/RJ_TP_other', layer ='Lines_Supervia')   %>% st_as_sf()
 vlt <- readOGR(dsn = './Shapes_BRT/RJ_TP_other', layer ='Lines_VLT')   %>% st_as_sf()

 #green_area <- st_transform(green_area, myprojection_latlong)


# STOPS
 stopsbrt <- readOGR(dsn = './Shapes_BRT', layer ='Stations_BRT_revised')
 stopsbrt_partial <- readOGR(dsn = './Shapes_BRT', layer ='Stations_BRT_revised_partial222222')
 
 
 stopsbrt <- st_as_sf(stopsbrt)
 stopsbrt_partial <- st_as_sf(stopsbrt_partial)

  
 
 
 stopsVLT <- st_read(dsn = './Shapes_IPP/Estacoes_VLT', layer ='Estacoes_VLT')
 stopsMETRO <- st_read(dsn = './Shapes_IPP/Estacoes_Metro', layer ='Estacoes_Metro')
 stopsTREM <- st_read(dsn = './Shapes_IPP/Estacoes_Trem', layer ='Estacoes_Trem')
 


  
# BRT encoding
    # brt$Corredor <- as.character(brt$Corredor)
    # brt$Name <- as.character(brt$Name)
    # Encoding(brt$Corredor)  <- "UTF-8"
    # Encoding(brt$Name)  <- "UTF-8"

  
  brt$Nome <- as.character(brt$Nome)
  brt$Sentido <- as.character(brt$Sentido)
  Encoding(brt$Nome)  <- "UTF-8"
  Encoding(brt$Sentido)  <- "UTF-8"
  
  
  brt_partial$Nome <- as.character(brt_partial$Nome)
  Encoding(brt_partial$Nome)  <- "UTF-8"
  

##### C. projection of SHAPE FILEs ----------

# lat long projection
  myprojection_latlong <- "+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


  
  state <- st_transform(state, myprojection_latlong)
  muni <- st_transform(muni, myprojection_latlong)
  
  brt <- st_transform(brt, myprojection_latlong)
  #st_crs(brt_partial) <- myprojection_latlong
  
  
  brt_partial <- st_transform(brt_partial, myprojection_latlong)
  
  metrolinha4 <- st_transform(metrolinha4, myprojection_latlong)
  metro <- st_transform(metro, myprojection_latlong)
  supervia <- st_transform(supervia, myprojection_latlong)
  vlt <- st_transform(vlt, myprojection_latlong)
  streets <- st_transform(streets, myprojection_latlong)
  
  
  
  stopsbrt_partial <- st_transform(stopsbrt_partial, myprojection_latlong)
  
  stopsbrt <- st_transform(stopsbrt, myprojection_latlong)
  stopsVLT <- st_transform(stopsVLT, myprojection_latlong)
  stopsMETRO <- st_transform(stopsMETRO, myprojection_latlong)
  stopsTREM <- st_transform(stopsTREM, myprojection_latlong)
  

  
  
# subset Metro liha 4, no Gavea station yet
 # plot(metrolinha4)
  metrolinha4$PopupInfo <- as.character(metrolinha4$PopupInfo)
  Encoding(metrolinha4$PopupInfo)  <- "UTF-8" # encode using UTF-8, para lidar com acentos no portugues

  #metrolinha4 <- subset(metrolinha4, !(PopupInfo %like% "Gávea"))
  #plot(metrolinha4)


  #mapView(metrolinha4)

# keep only streets within Rio Municipality
  #streets <- streets[muni, ]


  


  



######### G. locate airports and olympic clusters -------------------------


# get airports location
#airports <- fread("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat")
#airports <- airports[ V5 %in% c("GIG", "SDU"), .(V5, V7, V8)] %>% as.data.frame() # filter only airports in Rio by IATA code
#airports[1,2:3] <-   c(-22.817906, -43.249574)
#airports[2,2:3] <-   c(-22.910461, -43.163133)
airports <- data.frame( IATA= c("GIG", "SDU"),
                        lat= c(-22.817906, -22.910461),
                        long = c(-43.249574, -43.163133),
                        image=c("./aiplane_icon/airplane-4-xxl.png","./aiplane_icon/airplane-4-xxl.png"))


  
# Location of Olympic Venues ||| http://www.rio2016.com/os-jogos/mapa-de-competicoes
olympic_clusters <- data.table(venue=c('Maracana', 'Deodoro', 'Barra', 'Copacabana'), 
                               lat=c(-22.9047107, -22.8691319, -22.992637, -22.9724654), 
                               long=c(-43.249354, -43.4150473, -43.395614, -43.2000244))

gc(reset=TRUE)



######### H. Theme config  --------------------------- 

theme_opts <- list(theme(
  
                panel.border=element_blank(),
                 panel.grid.major= element_line(color = "gray90"),
                # panel.grid.minor=element_blank(),
                
                #panel.background = element_rect(fill = "#e6e8ed", colour = NA),
                panel.background = element_rect(fill = "gray98", colour = NA),
                
                axis.title=element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(), #  axis.text =element_text(size=9, color="gray30"),
                text = element_text(size=9),
                strip.background = element_rect(colour = "white", fill = "white", size = 11), #muda estilo de facet_grid boxes
                strip.text.x = element_text(size = 11, face ="bold"),
                # Legends  
                legend.position=c(0.115, 0.1), # horz vert
                legend.direction='horizontal',
                legend.box='horizontal',
                legend.title=element_text(size=8),
                legend.text=element_text(size=8)),
                #legend.key.size = unit(5, "cm"),
                #plot.title = element_text(size=22)
                guides(fill = guide_colorbar(title.position = "top")),
                coord_sf(xlim = c(-43.8, -43.117908),ylim = c(-23.091709, -22.760493)) # coord_cartesian Coordinates Zoom
              )
              





# scale bar
  scalebar <- ggsn::scalebar(muni, dist = 10, st.size=2, height=0.01, dd2km = TRUE, location="bottomleft" , model = 'WGS84')


############### 2. Create base map --------------------------------
gc(reset = T)


basemap <-  ggplot() +
              geom_sf(data= state, fill="white", color="gray70") +
              geom_image(data=airports, aes(x=long,y=lat, image=image), size=.025) + 
              scalebar +
              theme_map() + 
              theme_opts 


  




############### 3. Base infrastructure --------------------------------

# # Street network
# streetslayer <- c(geom_path(data=streets_df,
#                             aes(x=long, y=lat, group=group), size=.5, color="gray70", alpha=0.2))

# Olympic Venues
venues <- c(geom_point(data=olympic_clusters, aes(long,lat), shape=1, color="gray20", size=20, alpha=0.6))

# Old Network
infra_antes <- c( geom_sf(data=subset(brt, Sentido == "TransOeste - trecho Santa Cruz - Campo Grande - Alvorada"), size=0.3, color="gray30")
                  , geom_sf(data=metro, size=0.3, color="black") 
                  , geom_sf(data=supervia, size=0.3, linetype = 5, color="black")
                )


# working BRT lines and Stops
infra_atual <- c(geom_sf(data=subset(brt, Nome %like% c("TransCarioca|TransOl")), size=0.3, color="#2340e7") 
                 , geom_sf(data=subset(brt, Nome %like% "TransOeste"), size=0.3, color="#2340e7")
                 , geom_sf(data=metrolinha4, size=0.3, color="red")
                 , geom_sf(data=vlt, size=0.3, color="#2340e7") #ef6548 
                 )

# Planned BRTs, metro 4 and VLT
infra_depois <- c( geom_sf(data=subset(brt, Nome %like% "TransBrasil"), size=0.3, linetype = 3, color="red")
                   #geom_path(data=subset(brt, !(Status %in% c("Open" , "Operational"))), aes(x=long, y=lat, group = group), size=0.2, color="red")
)




# ############### 1. Create ggmap base ----------------------------------------
# 
# google_map <- get_googlemap(center = c(lon = -43.4, lat = -22.9), zoom = 10,  scale = 2,
#                             style = c('feature:administrative.locality|element:labels|visibility:off',
#                                       'feature:administrative.neighborhood|element:labels|visibility:off'))
# 
# 
# ggmap(google_map)
# 
# 
# rio <- c(left = -43.79653853, bottom = -23.07665345, right = -43.15022410, top = -22.77554842)
# google_map2 <- get_stamenmap(rio, zoom = 10,  scale = 2, maptype = "toner-lite")
# ggmap(google_map2)
# 
# 
# base_ggmap <- ggmap(google_map) + 
#   geom_polygon(data=muni, aes(long, lat, group = group), fill=NA, color="gray40") +
#   geom_image(data=airports, aes(x=long,y=lat, image=image), size=.025) +
#   theme_opts
# 



# base plot

baseplot <- theme_minimal() +
  theme( 
     #axis.text.y  = element_text(face="bold")
    #,axis.text.x  = element_text(face="bold")
    #,
    panel.grid.minor = element_blank()
    ,strip.text.x = element_text(size = 9, face ="bold")
    ,legend.text = element_text(size = 9)
     , axis.text = element_text(size=7)
     , axis.title = element_text(size=9)
  )




#### Get all transport modes in one sf object -----------------------

# make sure they have same projection
brt_partial <- st_transform(brt_partial, myprojection_latlong)
brt <- st_transform(brt, myprojection_latlong)
vlt <- st_transform(vlt, myprojection_latlong)
metro <- st_transform(metro, myprojection_latlong)
metrolinha4 <- st_transform(metrolinha4, myprojection_latlong)
supervia <- st_transform(supervia, myprojection_latlong)



# keep only geometry column
brt_partial2 <- select(brt_partial,  c('geometry'))
brt2 <- select(brt,  c('geometry'))
vlt2 <- select(vlt,  c('geometry'))
metro2 <- select(metro,  c('geometry'))
metrolinha42 <- select(metrolinha4,  c('geometry'))
supervia2 <- select(supervia,  c('geometry'))

# create name variable
brt_partial2$Nome <- brt_partial$Nome
brt2$Nome <- brt$Nome
vlt2$Nome <- 'vlt'
metro2$Nome <- 'metro'
metrolinha42$Nome <- 'metrolinha4'
supervia2$Nome <- 'rail'

# create transport mode variable
brt_partial2$mode <- 'brt'
brt2$mode <- 'brt'
vlt2$mode <- 'vlt'
metro2$mode <- 'metro'
metrolinha42$mode <- 'metrolinha4'
supervia2$mode <- 'rail'



# remove word sign in TransOlimpica
  table(brt2$Nome)
  brt2$Nome <- ifelse(brt2$Nome == 'TransOlímpica', 'TransOlimpica', brt2$Nome)
  table(brt2$Nome)
  
  brt_partial2$Nome <- as.character(brt_partial2$Nome)
  Encoding(brt_partial2$Nome)  <- "UTF-8"
  table(brt_partial2$Nome)
  brt_partial2$Nome <- ifelse(brt_partial2$Nome == 'TransOlímpica', 'TransOlimpica', brt_partial2$Nome)
  table(brt_partial2$Nome)


  
# rbind into single sf object
pt <- rbind(brt2, vlt2, metro2,  metrolinha42, supervia2)
pt_partial <- rbind(brt_partial2, vlt2, metro2,  metrolinha42, supervia2)

# reorder factor levels
pt$Nome <- factor(pt$Nome, levels=c('TransBrasil', 'TransCarioca', 'TransOeste', 'TransOlimpica','vlt', 'metro', 'metrolinha4', 'rail'),
                  labels = c("TransBrasil", "TransCarioca", 'TrabsOeste', 'TransOlimpica', 'LRT', 'Subway', "Subway Line.4", 'Rail'))

pt_partial$Nome <- factor(pt_partial$Nome, levels=c('TransBrasil', 'TransCarioca', 'TransOeste', 'TransOlimpica','vlt', 'metro', 'metrolinha4', 'rail'),
                  labels = c("TransBrasil", "TransCarioca", 'TrabsOeste', 'TransOlimpica', 'LRT', 'Subway', "Subway Line.4", 'Rail'))


table(pt$Nome)
table(pt_partial$Nome)

# ggplot() +
#   
#   # streets
#   geom_sf(data=pt,  size=1, alpha=.7, aes(color=Nome), show.legend = "line") +
#   scale_color_manual(values=c("#e41a1c", "#ff7f00", "#377eb8", "#984ea3", "#33a02c","gray20", "#f781bf", "gray50"), 
#                      labels=c("TransBrasil", "TransCarioca", 'TrabsOeste', 'TransOlimpica', 'LRT', 'Subway', "Subway Line.4", 'Rail'))

pt_network <- c(  geom_sf(data=pt,  size=1, alpha=.7, aes(color=Nome), show.legend = "line"),
                    scale_color_manual(values=c("#e41a1c", "#ff7f00", "#377eb8", "#984ea3", "#33a02c","gray20", "#f781bf", "gray50"), 
                                       labels=c("TransBrasil", "TransCarioca", 'TrabsOeste', 'TransOlimpica', 'LRT', 'Subway', "Subway Line.4", 'Rail'))
                            )

pt_network_partial <- c(  geom_sf(data=pt_partial,  size=1, alpha=.7, aes(color=Nome), show.legend = "line"),
                          scale_color_manual(values=c("#e41a1c", "#ff7f00", "#377eb8", "#984ea3", "#33a02c","gray20", "#f781bf", "gray50"), 
                                             labels=c("TransBrasil", "TransCarioca", 'TrabsOeste', 'TransOlimpica', 'LRT', 'Subway', "Subway Line.4", 'Rail'))
                      )



# ggplot() + pt_network
# ggplot() + pt_network_partial
# 
# 
# basemap +
#   geom_sf(data=pt,  size=.5, alpha=.7, aes(color=Nome), show.legend = "line") +
#   scale_color_manual(values=c("#e41a1c", "#ff7f00", "#377eb8", "#984ea3", "#33a02c","gray20", "#f781bf", "gray50"), 
#                      labels=c("TransBrasil", "TransCarioca", 'TrabsOeste', 'TransOlimpica', 'LRT', 'Subway', "Subway Line.4", 'Rail')) +
#   theme_opts +
#   theme(
#     panel.background = element_rect(fill = "gray98", colour = NA),
#     
#     # Legends
#     legend.title=element_blank(),
#     legend.position=c(0.93, 0.2), # horz vert
#     legend.direction='vertical',
#     legend.box='vertical',
#     legend.text=element_text(size=8),
#     legend.key.size = unit(.5, "cm"),
#     legend.key.width = unit(.6, "cm"))
