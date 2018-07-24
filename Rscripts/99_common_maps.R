
##################### set working Directory  -------------------------------------------------------
setwd("R:/Dropbox/Dout/Data Dout")


##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")


library(cowplot)
library(ggpubr)


##### C. Base Map --------------------------------
gc(reset = T)
source("./R scripts/00_BaseMap_sf_dest.R")
gc(reset = T)




##### A. Read Grid data ----------

output_oAccess_wide <- fread("./accessibility/output_oAccess_wide_500_paper4.csv")


##### B. Read Spatial Grids ----------
hex_0500 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_0500')

##### F. Spatial Join data   ----------
wide_map <- left_join( hex_0500, output_oAccess_wide, by="ID") 



#### C. Change labels  --------------------------------

# income lables
table(wide_map$decile)
wide_map$decile <- factor(wide_map$decile, levels=c(1:10),
                          labels = c("1 Poorest",2:9,"10 Richest"))  






############### 1. generate world map ----------------------------------------

source("./R scripts/00_world_map.R")





############### 2. Create ggmap base ----------------------------------------

# GOOGLE MAPS
map <- get_googlemap(center = c(lon = -43.4, lat = -22.9), zoom = 10,  scale = 2, size=c(640, 640),
                     style = c('feature:administrative.locality|element:labels|visibility:off',
                               'feature:administrative.neighborhood|element:labels|visibility:off'))

ggmap(map)


# # STAMEN
# rio_box <- c(left = -43.79653853, bottom = -23.07665345, right = -43.15022410, top = -22.77554842)
# 
# map_stamenmap <- get_stamenmap(bbox=rio_box, zoom = 10,  scale = 2,
#                      style = c('feature:administrative.locality|element:labels|visibility:off',
#                                'feature:administrative.neighborhood|element:labels|visibility:off'))
# 
# ggmap(map_stamenmap)
# map2 <- get_stamenmap(rio, zoom = 10,  scale = 2, maptype = "toner-lite")




############### 3. Create Core map ----------------------------------------

tempplot <-   ggmap(map) +  # basemap +
              # muni
                geom_sf(data= muni, fill=NA, color="black", linetype = "dashed", inherit.aes = FALSE) + theme_opts +
  
              # PT network
         #       geom_sf(data=pt,  size=.7, alpha=.7, aes(color=Nome), show.legend = "line", inherit.aes = FALSE) +
        #        scale_color_manual(values=c("#e41a1c", "#ff7f00", "#377eb8", "#984ea3", "#33a02c","gray20", "#f781bf", "gray50"), 
         #                          labels=c("TransBrasil", "TransCarioca", 'TrabsOeste', 'TransOlimpica', 'LRT', 'Subway', "Subway Line 4", 'Rail')) +
              # Legend
                theme_opts +
                theme(
                  panel.background = element_rect(fill = "gray98", colour = NA),
                  
                  # Legends
                  legend.title=element_blank(),
                  legend.position=c(0.88, 0.16), # horz vert
                  legend.direction='vertical',
                  legend.box='vertical',
                  legend.text=element_text(size=8),
                  legend.key.size = unit(.5, "cm"),
                  legend.key.width = unit(.74, "cm"),
                  legend.background = element_rect(fill="white")
                ) + 

        coord_sf(xlim = c(-43.8, -43.08),ylim = c(-23.091709, -22.755)) + # coord_cartesian Coordinates Zoom
                
    ggsn::scalebar(muni, dist = 10, st.size=2.5, height=0.01, dd2km = TRUE, model = 'WGS84',
                   anchor= c(x=-43.25, y=-23.08) )


############### 4. Map composition

tempplot_compos <- 
  ggdraw() +
  draw_plot(tempplot) +
  draw_plot(world_map, x=-.42, y=.34,  scale=.29)

# save plot
ggsave(tempplot_compos, file= "./plots_in_common/map_Rio_infra_topo.png", dpi = 400, width = 25, height = 13, units = "cm")
ggsave(tempplot_compos, file= "./plots_in_common/map_Rio_topo.png", dpi = 400, width = 25, height = 13, units = "cm")







#### Map 4. Population  --------------------------------

tempplot <-
  basemap +
  
  geom_sf(data= subset(wide_map, pop >0), aes(fill=pop/1000/area), alpha=1, color=NA) +
  scale_fill_distiller( palette="Oranges", guide = "colorbar", direction = 1, name= bquote('Population Density, thousands per'~ Km^2) ) +

  # PT network
  geom_sf(data=subset(pt, Nome != "TransBrasil"),  size=.5, alpha=.2, color="black", show.legend = F, inherit.aes = FALSE) +
  
  # streets
#  geom_sf(data=streets, color="gray", size=0.1, alpha=.7) +
  
  # infra_antes + infra_atual + venues +
  theme_opts +
  theme(
    legend.background = element_rect(colour = NA, fill = NA),
    # Legends
    legend.position=c(0.75, 0.12), # canto inf direito
    legend.direction='horizontal',
    legend.box='horizontal',
    legend.title=element_text(size=9),
    legend.text=element_text(size=9),
    legend.key.size = unit(.9, "cm"))

ggsave(tempplot, file= "./plots_in_common/map1_POP_intro.png", dpi = 300, width = 25, height = 12.5, units = "cm")
beep()


tempplot_compos <- 
  ggdraw() +
  draw_plot(tempplot) +
  draw_plot(world_map, x=-.42, y=.34,  scale=.29)

ggsave(tempplot_compos, file= "./plots_in_common/map1_POP_intro_globe.png", dpi = 300, width = 25, height = 12.5, units = "cm")















#### Map 2. Income Decile  --------------------------------


## DECILE

tempplot <-
  basemap +
  geom_sf(data= subset(wide_map, pop > 0 ) , aes( fill = factor(decile) ), color=NA, alpha = 0.8) +
  scale_fill_brewer( palette="RdBu", guide = "legend",  name="Income decile") +

  # PT network
    geom_sf(data=subset(pt, Nome != "TransBrasil"),  size=.6, alpha=.7, color="black", show.legend = F, inherit.aes = FALSE) +
  
  theme_opts +
  guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
  theme(
    # Legends
    legend.background = element_rect(colour = NA, fill = NA),
    legend.position=c(0.75, 0.1), # horz vert
    legend.direction='horizontal',
    legend.box='horizontal',
    legend.title=element_text(size=9),
    legend.text=element_text(size=8),
    legend.key.size = unit(.5, "cm"),
    legend.text.align=0.5)



ggsave(tempplot, file= "./plots_in_common/map2_income_decile.png", dpi = 300, width = 25, height = 12.5, units = "cm")
beep()







# ###############  Composite MAP  - Companies | Hospitals | Schools | Transit System #  --------------------------------
# 
# ############### Fig.2a Base Transit System
# 
# # read GTFS data
#   rail_gtfs <- fread("GTFS data/GTFS Rio feed_supervia 2014-12/shapes.txt")
#   metro_gtfs <- fread("GTFS data/GTFS Rio feed_metro 2014-12/shapes.txt")
#   bus_gtfs <- fread("GTFS data/GTFS Rio feed_bus 2014-11/shapes.txt")
# #convert to numeric
#   rail_gtfs <- rail_gtfs[, lapply(.SD, as.numeric)]
#   metro_gtfs <- metro_gtfs[, lapply(.SD, as.numeric)]
#   bus_gtfs <- bus_gtfs[, lapply(.SD, as.numeric)]
# 
# # plot map
#   basemap_transitsystem <- 
#       basemap + 
#       geom_path(data= bus_gtfs, aes(shape_pt_lon, shape_pt_lat, group=shape_id), color= "#225ea8", size=0.1, alpha=0.3) +
#       geom_path(data= rail_gtfs, aes(shape_pt_lon, shape_pt_lat, group=shape_id), color= "#225ea8", size=0.1, alpha=0.3) +
#       geom_path(data= metro_gtfs, aes(shape_pt_lon, shape_pt_lat, group=shape_id), color= "#225ea8", size=0.1, alpha=0.3) +
#       ggtitle("Transity System")
#   
#   
# ############### Fig.2a Hospitals 
# 
# # read hospitals dataset
# hospitals_filtered <- fread("./Rio places/hospitals_filtered.csv")
# 
# # plot map
# fig2_hospitals <- basemap +
#   geom_point(data= hospitals_filtered, aes(Longitude, Latitude), color = "#67000d", size=.05, alpha=0.7) +
#   ggtitle("Hospitals")
# 
# 
# ############### Fig.2b Schools  
# 
# # read hospitals dataset
# schools_filtered <- fread("./Rio places/schools_filtered.csv")
# 
# # plot map
# fig2_schools <- basemap +
#   geom_point(data= schools_filtered, aes(Longitude, Latitude), color = "#00441b", size=.05, alpha=0.7) +
#   ggtitle("Schools")
# 
# 
# 
# ############### Fig.2c Companies
# 
#   # Rainbow color Palette
#   myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
# 
# 
# # read Companies (RAIS) dataset
# # keep empresas com vinculo ativo e NAO erro de registo
#   rais2012estab_rj_grid <- fread("./rais/rais2012estab_rj_grid.csv", colClasses=list(character = 1:7))
#   rais2012estab_rj_grid <- rais2012estab_rj_grid[qtd_vinc_ativos >0 & qtd_vinc_ativos < 10000 , ]
# 
#   rais2012estab_rj_grid[, lat := as.numeric(lat)]
#   rais2012estab_rj_grid[, long := as.numeric(long)]
#   rais2012estab_rj_grid[, qtd_vinc_ativos := as.numeric(qtd_vinc_ativos)]
# 
# # plot map
# fig2_companies <- 
#   basemap +
#   geom_point(data= rais2012estab_rj_grid, aes(long, lat), color = "#08306b", size=.05, alpha=0.2) +
#   ggtitle("Jobs")
# 
# 
# basemap +
#   #geom_point(data= rais2012estab_rj_grid, aes(long, lat, color =qtd_vinc_ativos), size=.05, alpha=0.2) +
#   
#   ggplot()+
#   geom_contour(data= rais2012estab_rj_grid,  aes(long, lat, z =qtd_vinc_ativos))
# 
# 
#   geom_density2d(data= rais2012estab_rj_grid) +
#   stat_density2d(data= rais2012estab_rj_grid,
#                  aes(fill = ..level.., alpha = ..level..),
#                  size = 0.01, bins = 16, geom = 'polygon') +
#   scale_color_gradientn(colours = myPalette(100) ,  name="Catchment variation\nin percentage points") +
#   ggtitle("Jobs")
# 
# 
# 
# #### Grid MAPS of FIGURE 1
# fig2 <- grid.arrange(basemap_transitsystem, fig2_schools, fig2_hospitals, fig2_companies, ncol=2, nrow=2)
# 
# # save map
# ggsave(fig2, file="./plots_dest2/fig2.png" , dpi = 800,
#        width = 20, height = 15, units = "cm")
# 
# beep()
