

######## ////// Base MAP   ---------------------


##### B. Read SHAPE FILEs ----------


state <- st_read(dsn = './Shapes_IBGE', layer ='state')
brt <- st_read(dsn = './Shapes_BRT', layer ='Lines_BRT_revised')
muni <- st_read(dsn = './Shapes_IBGE', layer ='muni')







#brt <- readOGR(dsn = './Shapes_BRT', layer ='Lines_BRT_revised')
# stopsbrt <- readOGR(dsn = './Shapes_BRT', layer ='Stations_BRT_revised')
 metrolinha4 <- readOGR(dsn = './Shapes_BRT/RJ_TP_other', layer ='Lines_Metro_L4') %>% st_as_sf()
 metro <- readOGR(dsn = './Shapes_BRT/RJ_TP_other', layer ='Lines_Metro') %>% st_as_sf()
 supervia <- readOGR(dsn = './Shapes_BRT/RJ_TP_other', layer ='Lines_Supervia')  %>% st_as_sf()
 vlt <- readOGR(dsn = './Shapes_BRT/RJ_TP_other', layer ='Lines_VLT')  %>% st_as_sf()
#streets <- st_transform(streets, myprojection_latlong)
#green_area <- st_transform(green_area, myprojection_latlong)

#  streets <- st_read(dsn = './OSM_rio-de-janeiro', layer ='rio-de-janeiro_brazil_osm_roads')
#green_area <- st_read(dsn = './OSM_rio-de-janeiro', layer ='green_area')



# BRT encoding
  brt$Corredor <- as.character(brt$Corredor)
  brt$Name <- as.character(brt$Name)
  Encoding(brt$Corredor)  <- "UTF-8"
  Encoding(brt$Name)  <- "UTF-8"

  
  

##### C. projection of SHAPE FILEs ----------

# lat long projection
  myprojection_latlong <- "+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  state <- st_transform(state, myprojection_latlong)
  muni <- st_transform(muni, myprojection_latlong)
  
  brt <- st_transform(brt, myprojection_latlong)
  metrolinha4 <- st_transform(metrolinha4, myprojection_latlong)
  metro <- st_transform(metro, myprojection_latlong)
  supervia <- st_transform(supervia, myprojection_latlong)
  vlt <- st_transform(vlt, myprojection_latlong)
  
  
  
  


  



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

# ggplot() + geom_image(data=airports, aes(x=long,y=lat, image=image), size=.05) +
#   coord_equal()

# Location of Olympic Venues ||| http://www.rio2016.com/os-jogos/mapa-de-competicoes
olympic_clusters <- data.table(venue=c('Maracana', 'Deodoro', 'Barra', 'Copacabana'), 
                               lat=c(-22.9047107, -22.8691319, -22.992637, -22.9724654), 
                               long=c(-43.249354, -43.4150473, -43.395614, -43.2000244))

gc(reset=TRUE)



######### H. Theme config  --------------------------- 

theme_opts <- list(theme(# panel.background = element_blank(),
  # plot.background = element_rect(fill="#e6e8ed"),
  panel.border=element_blank(),
  panel.background = element_rect(fill = "gray99", colour = NA),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text =element_text(size=7),
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
  coord_sf(xlim = c(-43.828854, -43.117908),ylim = c(-23.091709, -22.760493)) # coord_cartesian Coordinates Zoom
)






# scale bar
  scalebar <- ggsn::scalebar(muni, dist = 10, st.size=2, height=0.001, dd2km = TRUE, model = 'WGS84')


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
venues <- c(geom_point(data=olympic_clusters, aes(long,lat), shape=1, color="gray20", size=13, alpha=0.5))

# Old Network
infra_antes <- c( geom_sf(data=subset(brt, Name == "TransOeste - trecho Santa Cruz - Campo Grande - Alvorada"), size=0.3, color="gray30")
                  , geom_sf(data=metro, size=0.3, color="black") 
                  , geom_sf(data=supervia, size=0.3, linetype = 5, color="black")
)


# working BRT lines and Stops
infra_atual <- c(geom_sf(data=subset(brt, Name %like% c("TransCarioca|TransOl")), size=0.3, color="#2340e7") 
                 , geom_sf(data=subset(brt, Name%like% "TransOeste - trecho Alvorada"), size=0.3, color="#2340e7")
                 , geom_sf(data=metrolinha4, size=0.3, color="red")
                 , geom_sf(data=vlt, size=0.3, color="#2340e7") #ef6548 
)

# Planned BRTs, metro 4 and VLT
infra_depois <- c( geom_sf(data=subset(brt, Name %like% "TransBrasil"), size=0.3, linetype = 3, color="red")
                   #geom_path(data=subset(brt, !(Status %in% c("Open" , "Operational"))), aes(x=long, y=lat, group = group), size=0.2, color="red")
)

# test
basemap + infra_antes + infra_atual + infra_depois + venues + theme_opts
# basemap + infra_atual
beep()








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


