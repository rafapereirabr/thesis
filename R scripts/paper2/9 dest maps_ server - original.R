


##################### set working Directory  -------------------------------------------------------
  setwd("R:/Dropbox/Dout/Data Dout")


##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")

library(ggsn)



  


######## ////// MAPS MAPS MAPS MAPS MAPS MAPS MAPS MAPS MAPS  ---------------------

# Collor palettes
  # http://colorbrewer2.org
  # http://www.colorhexa.com/



##### A. Read Grid data (ccess / catchment)  ----------

# MIX
# output_catchment_wide <- fread("./accessibility/output_catchment_size_500_wide.csv")
# output_catchment_long <- fread("./accessibility/output_catchment_size_500_long.csv")

# COUNTER
output_catchment_wide <- fread("./accessibility/output_catchment_size_500_wide_counter.csv")
output_catchment_long <- fread("./accessibility/output_catchment_size_500_long_counter.csv")



##### B. Read Spatial Grids ----------

# read maps
hex_0500 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_0500')

##### F. Spatial Join data   ----------
long_map <- left_join( hex_0500, output_catchment_long, by=c("ID"="destination")) 
wide_map <- left_join( hex_0500, output_catchment_wide, by="ID") 





##### C. Base Map --------------------------------

source("./R scripts/00_BaseMap_sf_dest.R")

# test
basemap + infra_antes + infra_atual + infra_depois + venues + theme_opts





#### D. Change labels  --------------------------------


long_map$year <- if_else(long_map$year=="201404", 2014, 2017)
table(long_map$year)


# incomde lables
table(wide_map$decile)
table(long_map$decile)

wide_map$decile <- factor(wide_map$decile, levels=c(1:10),
                          labels = c("1 Poorest",2:9,"10 Richest"))  



# test
  ggplot() +
  geom_sf(data=subset(wide_map, pop >0), aes(fill=pop), color=NA) + 
    coord_sf()


gc(reset=T)
beep()

    




############### 1. Create ggmap base ----------------------------------------

map <- get_googlemap(center = c(lon = -43.4, lat = -22.9), zoom = 10,  scale = 2,
                     style = c('feature:administrative.locality|element:labels|visibility:off',
                               'feature:administrative.neighborhood|element:labels|visibility:off'))


ggmap(map)

rio <- c(left = -43.79653853, bottom = -23.07665345, right = -43.15022410, top = -22.77554842)
map2 <- get_stamenmap(rio, zoom = 10,  scale = 2, maptype = "toner-lite")
ggmap(map2)




  
############### Map 2. SPORTS Venues  --------------------------------


#### REFAZER com novos poly IDs !!!!!
sports_venues <- c(  1327 # Olympic Park
                     ,1109 # Rio Centro
                     , 569 # Golf course                                   
                     ,3170 # Maracana
                     ,3797 # Olympic Stadium
                     ,4733 # Deodoro Stadium  
                     ,3177 # Sambodromo - atlhetics 
                     ,2924 # Marina da Gloria
                     ,1149 # Lagoa 
                     )



venuesmap <- output_catchment_wide[ ID %in%  sports_venues, .( ID, X, Y)]

venuesmap[, name := ifelse( ID == 3170 , "Maracana",
                            ifelse(ID == 1149, "Lagoa Stadium",
                          #  ifelse(ID == 1472, "Beach Volleyball - Copacabana",
                            ifelse(ID == 2924, "Marina da Gloria",
                            ifelse(ID == 3177, "Sambodromo",
                            ifelse(ID == 4733, "Deodoro Stadium", # Deodoro Stadium + Aquatic Center
                            ifelse(ID == 1327, "Olympic Park",
                            ifelse(ID == 1109 , "Rio Centro",
                            ifelse(ID ==  569 , "Golf course",
                            ifelse(ID == 3797 , "Olympic Stadium", NA))))))))) ]

venuesmap[, number := 1:nrow(venuesmap)]



mapxx_venues <-

basemap +
  infra_antes + infra_atual  +
  geom_point(data=venuesmap, aes(X, Y), size=6, color="midnightblue") +
  geom_point(data=venuesmap, aes(X, Y), size=5, color="midnightblue") +
  geom_text(data=venuesmap, aes(X, Y, label = number), size=3, color="white") +
  theme_opts


  png(file = "./plots_dest2/mapxx_venues2.png" ,  width = 20, height = 20, units = "cm", res = 800)

  mapxx_venues

  ### 1st column ###

  grid.text((paste(venuesmap$number[1], venuesmap$name[1], sep = " - ")),
            x = unit(.1, "npc"), y = unit(.22, "npc"), just = c("left", "bottom"),
            gp = gpar(fontsize = 9))

  grid.text((paste(venuesmap$number[2], venuesmap$name[2], sep = " - ")),
            x = unit(.1, "npc"), y = unit(.18, "npc"), just = c("left", "bottom"),
            gp = gpar(fontsize = 9))


  grid.text((paste(venuesmap$number[3], venuesmap$name[3], sep = " - ")),
            x = unit(.1, "npc"), y = unit(.14, "npc"), just = c("left", "bottom"),
            gp = gpar(fontsize = 9))

  # grid.text((paste(venuesmap$number[4], venuesmap$name[4], sep = " - ")),
  #           x = unit(.1, "npc"), y = unit(.1, "npc"), just = c("left", "bottom"),
  #           gp = gpar(fontsize = 9))


  ### 2nd column ###

  grid.text((paste(venuesmap$number[4], venuesmap$name[4], sep = " - ")),
            x = unit(.35, "npc"), y = unit(.22, "npc"), just = c("left", "bottom"),
            gp = gpar(fontsize = 9))


  grid.text((paste(venuesmap$number[5], venuesmap$name[5], sep = " - ")),
            x = unit(.35, "npc"), y = unit(.18, "npc"), just = c("left", "bottom"),
            gp = gpar(fontsize = 9))

  grid.text((paste(venuesmap$number[6], venuesmap$name[6], sep = " - ")),
            x = unit(.35, "npc"), y = unit(.14, "npc"), just = c("left", "bottom"),
            gp = gpar(fontsize = 9))

  # grid.text((paste(venuesmap$number[8], venuesmap$name[8], sep = " - ")),
  #           x = unit(.35, "npc"), y = unit(.1, "npc"), just = c("left", "bottom"),
  #           gp = gpar(fontsize = 9))



  ### 3rd column ###


  grid.text((paste(venuesmap$number[7], venuesmap$name[7], sep = " - ")),
            x = unit(.6, "npc"), y = unit(.22, "npc"), just = c("left", "bottom"),
            gp = gpar(fontsize = 9))

  grid.text((paste(venuesmap$number[8], venuesmap$name[8], sep = " - ")),
            x = unit(.6, "npc"), y = unit(.18, "npc"), just = c("left", "bottom"),
            gp = gpar(fontsize = 9))

  grid.text((paste(venuesmap$number[9], venuesmap$name[9], sep = " - ")),
            x = unit(.6, "npc"), y = unit(.14, "npc"), just = c("left", "bottom"),
            gp = gpar(fontsize = 9))

  # grid.text((paste(venuesmap$number[12], venuesmap$name[12], sep = " - ")),
  #           x = unit(.6, "npc"), y = unit(.1, "npc"), just = c("left", "bottom"),
  #           gp = gpar(fontsize = 9))


  dev.off()


  
############### Map 3. Hospitals  --------------------------------
  
# read hospitals dataset
  hospitals_filtered <- fread("./Rio places/hospitals_filtered_correct.csv")

  # keep highest level
  table((hospitals_filtered$hierarq))
  hospitals_filtered[ hierarq %like% "Medium" , hierarq := "Medium"]
  hospitals_filtered[ hierarq %like% "High" , hierarq := "High"]
  

  hospitals_filtered[, hierarq := factor(hierarq, levels=c("Low","Medium","High"))] # Sort Data by decile - this rearranges the plot
  
  
# plot map
  fig_4_Hospitals <- 
    
    ggplot() +
    geom_sf(data= state, fill="white", color="gray70") +
    geom_image(data=airports, aes(x=long,y=lat, image=image), size=.025) + 
    ggsn::scalebar(muni, dist = 10, st.size=3, height=0.01, dd2km = TRUE, location="bottomright" , model = 'WGS84') +
  
  
    geom_point(data=hospitals_filtered, aes(Longitude, Latitude, color=hierarq, label=nome)) + # shape=hierarq
    infra_antes + infra_atual +  # venues + 
  #  theme_map() + 
    theme_opts +
  
  theme(
    legend.position=c(0.01, 0.97)
           # legend.position="top"
    ) +
    guides(color = guide_legend(title = "304 Healthcare Facilities"), shape = guide_legend(title = "304 Healthcare Facilities")) +
    theme(legend.text=element_text(size=11)) +
    theme(legend.title = element_text(size=13)
          ) 
  

                               
# save map
ggsave(fig_4_Hospitals, file="./plots_dest2/fig_4_Hospitals.png" , dpi = 600, width = 25, height = 12.5, units = "cm")







#### Map 4. Population  --------------------------------

tempplot <-
  basemap +
  geom_sf(data= subset(wide_map, pop >0), aes(fill=pop/1000/area), color=NA) +
  scale_fill_distiller( palette="Oranges", guide = "colorbar", direction = 1, name= bquote('Population Density, thousands per'~ Km^2) ) +
  infra_antes + infra_atual + venues +
  theme_opts +
  theme(
    legend.background = element_rect(colour = NA, fill = NA),
    # Legends
    legend.position=c(0.75, 0.01), # canto inf direito
    legend.direction='horizontal',
    legend.box='horizontal',
    legend.title=element_text(size=9),
    legend.text=element_text(size=9),
    legend.key.size = unit(.9, "cm"))

ggsave(tempplot, file=paste0("./plots_dest2/", "map4_POP.png"), dpi = 300, width = 25, height = 12.5, units = "cm")
beep()







#### Map 4. INCOME  --------------------------------



tempplot <-
  basemap + # base_ggmap
  geom_sf(data= subset(wide_map, pop > 0 ) , aes(fill = income), color=NA) +
  scale_fill_distiller( palette="Blues", guide = "colorbar", direction = 1, name="Income per capita") +
  infra_antes + infra_atual +
  theme_opts +
  theme(
    legend.background = element_rect(colour = NA, fill = NA),
    # Legends
    legend.position=c(0.75, 0.01), # canto inf direito
    legend.direction='horizontal',
    legend.box='horizontal',
    legend.title=element_text(size=8),
    legend.text=element_text(size=8),
    legend.key.size = unit(.8, "cm"))



ggsave(tempplot, file=paste0("./plots_dest2/map4_income.png"), dpi = 300, width = 25, height = 12.5, units = "cm")


# % poor

# make sure areas with 0% show in the map
  setDT(wide_map)[, prop_poor := ifelse(is.na(prop_poor),0,prop_poor)]

tempplot <-
  basemap + # base_ggmap
  geom_sf(data= subset(wide_map, pop > 0 ) , aes(fill = prop_poor), color=NA) +
  scale_fill_distiller( palette="Purples", guide = "colorbar", direction = 1, name="% poor", labels = percent_format()) +
  #geom_emoji(data=airports, aes(x=long, y=lat), emoji="1f170", size=0.02) +
  infra_antes + infra_atual +
  theme_opts +
  theme(
    legend.background = element_rect(colour = NA, fill = NA),
    # Legends
    legend.position=c(0.75, 0.01), # canto inf direito
    legend.direction='horizontal',
    legend.box='horizontal',
    legend.title=element_text(size=8),
    legend.text=element_text(size=8),
    legend.key.size = unit(.8, "cm"))


ggsave(tempplot, file=paste0("./plots_dest2/map4_poor.png"), dpi = 300, width = 25, height = 12.5, units = "cm")


## DECILE

tempplot <-
  basemap +
  geom_sf(data= subset(wide_map, pop > 0 ) , aes( fill = factor(decile) ), color=NA, alpha = 0.8) +
  scale_fill_brewer( palette="RdBu", guide = "legend",  name="Income decile") +
  infra_antes + infra_atual +
  theme_opts +
  guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
  theme(
    # Legends
    legend.background = element_rect(colour = NA, fill = NA),
    legend.position=c(0.55, 0.04), # horz vert
    legend.direction='horizontal',
    legend.box='horizontal',
    legend.title=element_text(size=9),
    legend.text=element_text(size=8),
    legend.key.size = unit(.5, "cm"),
    legend.text.align=0.5)



ggsave(tempplot, file=paste0("./plots_dest2/", "map4_decile_poly.png"), dpi = 300, width = 25, height = 12.5, units = "cm")
beep()














############### Map 6. CATCHMENT 2014 vs 2017 --------------------------------

summary(long_map$med_catchm_pop60_prop)


# areas mais acessiveis
  output_catchment_long[med_catchm_pop60_prop > .55]
  
  #> 4130 -> Estcao Cascadura
  #> 4240 -> Estcao Madureira (BRT + TREM)
  #> 3912 -> Estcao Piedade (TREM)

# Populacao que acessa o Maracana
output_catchment_long[destination == 3170 ]

# MUDANCA
mean(output_catchment_wide$catch_diff60_2017_2014_prop)    # media -0.1 ponto percentual
summary(output_catchment_wide$catch_diff60_2017_2014_prop) # min=-14%, max=14% 




for (i in c(30,60,90)) {
  
  tempvar <- noquote( paste0("med_catchm_pop", i,"_prop") )
   
  
  tempplot <- basemap +
              geom_sf(data= subset(long_map, !is.na( get(tempvar) )), aes( fill = get(tempvar) ), color=NA) +
              scale_fill_viridis( name="% of city population", labels = percent_format(),  breaks= seq(0,.6,.2) ) + # limits=c(0, .57)
              #scale_fill_distiller( palette="OrRd", guide = "colorbar", direction = 1, name="% of city population",labels = percent_format()) +
            
              infra_antes + infra_atual +
              theme_opts +
            # facet_grid( . ~year) + theme_opts          # horizontal
              facet_grid( year ~.) +                     # vertical
              theme(strip.text = element_text(size = 12),
                    legend.background = element_rect(colour = NA, fill = NA),
                    #legend.position=c(0.88, 0.06),
                    legend.position=c(0.72, 0.005), # canto inf direito
                    #legend.position=c(0.008, 0.03),  # canto inf esquerdo
                    legend.text=element_text(size=8))
              

# Save Plot  
  if (counter==T){
    ggsave(tempplot, file=paste0("./plots_dest2/", "mapxx_catchment_",i,"_counter.png"), dpi = 300, width = 15, height = 15, units = "cm") 
  
  } else {  
  ggsave(tempplot, file=paste0("./plots_dest2/", "mapxx_catchment_",i,"_mix1.png"), dpi = 300, width = 15, height = 15, units = "cm") 
  }
  # vertical   - width = 15, height = 15 \\\   # horizontal -  width = 30, height = 13
  }



############### Map 7. CATCHMENT Variation --------------------------------

summary(wide_map$catch_diff30_2017_2014_prop)
summary(wide_map$catch_diff60_2017_2014_prop)
summary(wide_map$catch_diff90_2017_2014_prop)

# Rainbow color Palette
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")


for (i in c(30,60,90)) {
  
  tempvar <- noquote( paste0("catch_diff", i,"_2017_2014_prop") )
  #tempvar <- "catch_diff60_2017_2014_prop"
  tempplot <- basemap +
              geom_sf(data= subset(wide_map,  pop >0) , aes( fill = 100* get(tempvar)), color=NA) +
              # scale_fill_viridis( option='B', name="Catchment variation\nin percentage points",  limits=c(-15, 15)  ) +
               scale_fill_gradientn(limits=c(-15, 15), colours = myPalette(100), name="Catchment variation\nin percentage points" ) +# , limits=c(-27, 27) , name="Catchment variation\nin percentage points") + #  
              # scale_fill_distiller( palette="RdBu", guide = "colorbar", name="Catchment variation\nin percentage points",  direction=-1,limits = c(-20, 20 )) +
              infra_antes + infra_atual +
              theme_opts +
    
              theme(strip.text = element_text(size = 12),
                    legend.background = element_rect(colour = NA, fill = NA),
                    #legend.position=c(0.88, 0.06),
                    legend.position=c(0.75, 0.01), # canto inf direito
                    #legend.position=c(0.008, 0.03),  # canto inf esquerdo
                    legend.text=element_text(size=8),
                    legend.key.size = unit(.65, "cm"))
            

    
    
  # Save Plot  
  ggsave(tempplot, file=paste0("./plots_dest2/", "mapxx_catchment_variation_",i,".png"), dpi = 400, width = 25, height = 12.5, units = "cm") 
  # vertical   - width = 15, height = 15 \\\   # horizontal -  width = 30, height = 13
  
}
beep()







names(long_map)
summary(long_map$year)



######### Using ggmap ----------------------------------------


# Olympic Venues
venues <- c(geom_point(data=olympic_clusters, aes(long,lat), shape=1, color="gray20", size=15, alpha=0.8))

# Old Network
infra_antes <- c(geom_path(data=subset(brt_df, Name %like% c("TransOeste")), aes(x=long, y=lat, group = group), size=0.4, color="gray30"),
                 geom_path(data=metro.df, aes(x=long, y=lat, group = group), size=0.4, color="black") 
                 ,geom_path(data=supervia.df, aes(x=long, y=lat, group = group), size=0.4, linetype = 5, color="black")
)
# working BRT lines and Stops
infra_atual <- c(geom_path(data=subset(brt_df, Name %like% c("TransCarioca|TransOl?mpica")), aes(x=long, y=lat, group = group), size=0.4, color="#2340e7") 
                 ,geom_path(data=metrolinha4.df, aes(x=long, y=lat, group = group), size=0.4, color="red")
                 ,geom_path(data=vlt.df, aes(x=long, y=lat, group = group), size=0.4, color="red") #ef6548 
)
# Planned BRTs, metro 4 and VLT
infra_depois <- c( geom_path(data=subset(brt_df, Name %like% "TransBrasil"), aes(x=long, y=lat, group = group), size=0.4, linetype = 3, color="red")
                   #geom_path(data=subset(brt_df, !(Status %in% c("Open" , "Operational"))), aes(x=long, y=lat, group = group), size=0.2, color="red")
)

so_ggmap <- base_ggmap
passado <-   base_ggmap + infra_antes
presente <- base_ggmap + infra_antes + infra_atual + venues
futuro <-   base_ggmap + infra_antes + infra_atual + infra_depois + venues

ggsave(so_ggmap, file=paste0("./plots_dest2/", "a_so_ggmap.png"), dpi = 800, width = 19, height = 10, units = "cm") 
ggsave(passado , file=paste0("./plots_dest2/", "a_passado.png"), dpi = 800, width = 19, height = 10, units = "cm") 
ggsave(presente, file=paste0("./plots_dest2/", "a_presente.png"), dpi = 800, width = 19, height = 10, units = "cm") 
ggsave(futuro  , file=paste0("./plots_dest2/", "a_futuro.png"), dpi = 800, width = 19, height = 10, units = "cm") 





ggmap(map) + 
  geom_polygon(data= subset(long_map, !is.na(avg_propaccessJobsmatch)), 
               aes(long, lat, group = group, fill = avg_propaccessJobsmatch), alpha=0.8) +
  scale_fill_gradient( low = "#fef0d9",   high = "#bd0026", na.value = "white",
                       guide = guide_legend(title = "Population")) + # pop RED
  facet_grid(. ~ year) +
  theme_opts +
  theme(legend.position="bottom")


ggmap(map) +
  geom_polygon(data= subset(long_map, !is.na(med_catchm_pop90)), 
               aes(long, lat, group = group, fill = med_catchm_pop90), alpha=0.8) +
  scale_fill_distiller( palette="OrRd", guide = "colorbar", direction = 1, name="Avg. Accessibility (%)") +
  # Infra
  #geom_emoji(data=airports, aes(V8, V7), emoji="1f170", size=0.02) + # airports
  #infra_antes  + # Transport invest
  facet_grid(year~.) +
  theme(legend.justification = c(0, 0), legend.position = c(0, 0)) + 
  theme_opts


###### Infra

a <-  ggmap(map) +# infra_antes + infra_atual + infra_depois +
  scale_x_continuous(limits = c(-43.79653853, -43.15022410), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-23.07665345 , -22.77554842), expand = c(0, 0)) 

ggmap(map) + theme_opts
geom_polygon(data= subset(wide_map, pop > 0 ) , aes(long, lat, group = group, fill = pop/1000 )) +
  scale_fill_distiller( palette="Oranges", guide = "colorbar", direction = 1, name="Population in thousands") +
  scale_x_continuous(limits = c(-43.79653853, -43.15022410), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-23.07665345 , -22.77554842), expand = c(0, 0)) 



ggsave(a, file=paste0("./plots_dest2/", "mapxx_population_future_ggmap.png"), dpi = 800,
       width = 19, height = 10, units = "cm") 


