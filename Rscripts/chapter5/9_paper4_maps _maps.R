
  
##################### set working Directory  -------------------------------------------------------
  setwd("R:/Dropbox/Dout/Data Dout")


##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")


library(cowplot)
library(ggpubr)





######## ////// MAPS MAPS MAPS MAPS MAPS MAPS MAPS MAPS MAPS  ---------------------

# Collor palettes
  # http://colorbrewer2.org
  # http://www.colorhexa.com/



##### A. Read Grid data (ccess / catchment)  ----------

output_oAccess_wide <- fread("./accessibility/output_oAccess_wide_500_paper4.csv")
output_oAccess_long <- fread("./accessibility/output_oAccess_long_500_paper4.csv")



### Gini income inequality ponderado pela pop

temp <- output_oAccess_wide[, .(pop, income)]
temp <- na.omit(temp)

reldist::gini(x = temp$income, weights = temp$pop )






table(output_oAccess_long$year)
names(output_oAccess_long)

total_pop <- sum(output_oAccess_wide$pop, na.rm = T)


# check if they have same distributions
  summary(output_oAccess_wide$diff_ratio_oaccess_jobsmatch_full_60) 
  summary(output_oAccess_long$dif_access_ratio_full_60) 
  setDT(output_oAccess_long)[ year == "full", .(dif_access_ratio_60)][[1]] %>%  summary()

  

  
  # plot
  output_oAccess_wide$diff_ratio_oaccess_jobsmatch_full_60 %>% sort %>% plot
  setDT(output_oAccess_long)[ year == "full", .(dif_access_ratio_60)][[1]] %>% sort %>% plot(add=T, col="red")
  
  setDT(output_oAccess_long)[ decile==1 & year == "full", .(dif_access_ratio_60)][[1]] %>%  summary()
  setDT(output_oAccess_long)[ decile==1 & year == "partial", .(dif_access_ratio_60)][[1]] %>%  summary()
  
  setDT(output_oAccess_long)[ dif_access_ratio_60 !=1 & decile==1 & year == "full", .(dif_access_ratio_60)][[1]] %>%  summary()
  setDT(output_oAccess_long)[ dif_access_ratio_60 !=1 & decile==1 & year == "partial", .(dif_access_ratio_60)][[1]] %>%  summary()
  
  
  
  
  
##### B. Read Spatial Grids ----------

# read maps
hex_0500 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_0500')

##### F. Spatial Join data   ----------
wide_map <- left_join( hex_0500, output_oAccess_wide, by="ID") 

long_map <- left_join( hex_0500, output_oAccess_long, by=c("ID")) 




#### C. Change labels  --------------------------------

# income lables
table(wide_map$decile)
table(long_map$decile)

wide_map$decile <- factor(wide_map$decile, levels=c(1:10),
                          labels = c("1 Poorest",2:9,"10 Richest"))  

long_map$decile <- factor(long_map$decile, levels=c(1:10),
                          labels = c("1 Poorest",2:9,"10 Richest"))  


table(long_map$year)



long_map$year <- factor(long_map$year, levels=c("baseline", "partial", "full"),
                          labels = c("Baseline", "Partial", "Full"))  


table(long_map$year)





##### C. Base Map --------------------------------
gc(reset = T)
source("./R scripts/00_BaseMap_sf_dest.R")
gc(reset = T)

# test
# basemap + infra_antes + infra_atual + infra_depois + venues + theme_opts



# # test
#   ggplot() +
#   geom_sf(data=subset(wide_map, pop >0), aes(fill=pop), color=NA) + 
#     coord_sf()


gc(reset=T)
beep()



############### Map 1. transport corridors  --------------------------------


tempplot <- 
  
  basemap +
    # streets
    geom_sf(data=streets, color="gray80", size=0.1) +
      
    # PT network
      geom_sf(data=pt,  size=.5, alpha=.7, aes(color=Nome), show.legend = "line") +
      scale_color_manual(values=c("#e41a1c", "#ff7f00", "#377eb8", "#984ea3", "#33a02c","gray20", "#f781bf", "gray50"), 
                         labels=c("TransBrasil", "TransCarioca", 'TrabsOeste', 'TransOlimpica', 'LRT', 'Subway', "Subway Line.4", 'Rail')) +
      theme_opts +
      theme(
            panel.background = element_rect(fill = "gray98", colour = NA),

          # Legends
          legend.title=element_blank(),
          legend.position=c(0.93, 0.2), # horz vert
          legend.direction='vertical',
          legend.box='vertical',
          legend.text=element_text(size=8),
          legend.key.size = unit(.5, "cm"),
          legend.key.width = unit(.6, "cm"))


# generate world map
source("./R scripts/00_world_map.R")



tempplot_compos <- 
ggdraw() +
  draw_plot(tempplot) +
  draw_plot(world_map, x=-.42, y=.34,  scale=.29)  

  


# save plot
  ggsave(tempplot, file=paste0("./plots_4_transbra/", "map1_infra legend.png"), dpi = 400, width = 25, height = 12.5, units = "cm")
  ggsave(tempplot_compos, file=paste0("./plots_4_transbra/", "map1_infra legend_compos.png"), dpi = 400, width = 25, height = 12.5, units = "cm")
  beep()







############### Map 2. TransBrasil  --------------------------------


templot <- 
  
    basemap +
    
    # streets
      geom_sf(data=streets, color="gray", size=0.1, alpha=.7) +
    
  
    # METRO
    geom_sf(data=metro, size=0.3, color="gray20") +
    geom_sf(data=metrolinha4, size=0.3, color="#f781bf") +
    geom_sf(data=stopsMETRO, size=1,  color="gray20", alpha=0.9) +
    geom_sf(data=subset(stopsMETRO, Obs =="Nova Linha 4"), size=1,  color="#f781bf", alpha=0.9) +
  
    # TREM
    geom_sf(data=supervia, size=0.3, linetype = 5, color="gray50") +
    geom_sf(data=stopsTREM, size=1,  color="gray50", alpha=0.9) +
    
    # VLT
    geom_sf(data=vlt, size=0.3, color="#33a02c") +#ef6548
    geom_sf(data=stopsVLT, size=1,  color="#33a02c", alpha=0.9) +
    
    # BRTs
      geom_sf(data=subset(brt, Nome %like% c("TransOl")), size=0.3, color="#984ea3", alpha=0.5) +
      geom_sf(data=subset(brt, Nome %like% c("TransCarioca")), size=0.3, color="#ff7f00", alpha=0.5) +
      geom_sf(data=subset(brt, Nome%like% "TransOeste"), size=0.3, color="#377eb8", alpha=0.5) +
      geom_sf(data=subset(brt, Nome %like% "TransBrasil"), size=0.3, color="#e41a1c", alpha=0.8) + # TRANSBRASIL
    
    # BRT stops
    geom_sf(data=subset(stopsbrt, Corredor %like% "TransOl"), size=1,  color="#984ea3", alpha=0.5) +
    geom_sf(data=subset(stopsbrt, Corredor %like% "TransCarioca"), size=1,  color="#ff7f00", alpha=0.5) +
    geom_sf(data=subset(stopsbrt, Corredor %like% "TransOeste"), size=1,  color="#377eb8", alpha=0.5) +
    geom_sf(data=subset(stopsbrt, Corredor %like% "TransBrasil"), size=1,  color="#e41a1c") +
    # geom_sf(data=stopsBRT, size=0.00001, color="#e41a1c") +
    
    theme_opts +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank()) +
    
    coord_sf(xlim = c(-43.45, -43.15),ylim = c(-22.95, -22.8)) # zoom
  
    

# add scale to zoomed map
    templot +  ggsn::scalebar(dist = 5, st.size=2, height=0.01, y.min=-22.9,y.max=-22.95, x.max=-43.15, x.min= -43.2, dd2km = TRUE,  model = 'WGS84')
  

# save plot
ggsave(templot, file=paste0("./plots_4_transbra/", "map2_transbrasil.png"), dpi = 400, width = 24, height = 12.5, units = "cm")
beep()




  







  



  
############### Map 3. oAccess jobsmatch 2014 vs 2017 -------------------------------
  
  
for (i in c(30,60,90,120)) {
    

  # temp_df <- subset(long_map, year=="before")
  # temp_df <- subset(long_map, year != "Baseline")
  
# JOBS match
  tempvar <- noquote( paste0("prop_med_oaccess_jobsmatch_", i) )
  
tempplot <- 
      basemap +
      geom_sf(data= subset(long_map, pop > 0), aes(fill=get(tempvar)), alpha=.85, color=NA) +
      scale_fill_viridis( option="A", name="Proportion of jobs accessible", labels = percent_format())+#, limits=c(0, .6), breaks= seq(0,.6,.1) ) +
      infra_antes + infra_atual +
      theme_opts +
      facet_wrap(~year, ncol = 1) +
      #  theme(legend.position="top") +
      
      theme(
        # Legends
        legend.position=c(0.75, 0.07), # horz vert
        legend.direction='horizontal',
        legend.box='horizontal',
        legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        # axis
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
    
    ggsave(tempplot, file=paste0("./plots_4_transbra/map4_oAcess_jobsmatch_", i,".png"), dpi = 300,
           width = 16, height = 18, units = "cm")
    
    
  }
  beep()
  
  
  
  


  
  
  
  
  
############### Map 4. Jobsmatch Variation --------------------------------
  
  # # color scheme 
  # Uber (divergent)
#  uber_scale_diver <- c(scale_fill_gradient2(low = "#108188", mid = "#eee7e5", high = "#c22e00", midpoint = 0, guide = "colourbar"))
#  uber_scale_diver <- c(scale_fill_gradient2(low = "#c22e00", mid = "#eee7e5", high = "#108188", midpoint = 0, guide = "colourbar"))
  # Rainbow color Palette
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
  
  summary(wide_map$diff_ratio_oaccess_jobsmatch_full_90)
  summary(wide_map$diff_ratio_oaccess_jobsmatch_partial_90)
  
  
  
for (i in c(30, 60, 90, 120)) {
  
  ##### Zoom
  
  # get variable
    temp_df <- subset(long_map, year != "Baseline")
  
  # convert scale 
    tempvar <- noquote( paste0("dif_access_ratio_",i) ) 
    temp_df$test <- (temp_df[, paste(tempvar)][[1]] - 1)
    max_val <-  max(temp_df$test, na.rm = T) %>% round(digits=1) # get max e min scale value
    # max_val <- 1.3
    summary(temp_df$test)
    
    # extreme values to -1 and 1
      #wide_map$test <- ifelse(wide_map$test >1, 1, wide_map$test)
      #wide_map$test <- ifelse(wide_map$test < -1, -1, wide_map$test)
    
    max_val <- 2.3
# partial plot
  df_partial <- subset(temp_df, year == "Partial")
  table(df_partial$year)
    

tempplot_partial <-
      
      basemap +
      geom_sf(data= subset(df_partial, test>0 ), aes(fill= test, label=ID), alpha=0.75, color=NA, show.legend=F) +
      scale_fill_distiller(  limits=c(0, max_val),labels=percent, palette="Reds",  direction = 1, name="Accessibility variation") + 
      
      # streets
        geom_sf(data=streets, color="gray80", size=0.1, alpha=.7) +
      
    # METRO
      geom_sf(data=metro, size=0.3, color="gray20") +
      geom_sf(data=metrolinha4, size=0.3, color="#f781bf") +
      geom_sf(data=stopsMETRO, size=1,  color="gray20", alpha=0.9) +
      geom_sf(data=subset(stopsMETRO, Obs =="Nova Linha 4"), size=1,  color="#f781bf", alpha=0.9) +
  
      # TREM
        geom_sf(data=supervia, size=0.3, linetype = 5, color="gray50") +
        geom_sf(data=stopsTREM, size=1,  color="gray50", alpha=0.9) +
        
      # VLT
        geom_sf(data=vlt, size=0.3, color="#33a02c") +#ef6548
        geom_sf(data=stopsVLT, size=1,  color="#33a02c", alpha=0.9) +
      
      # BRTs
        geom_sf(data=subset(brt, Nome %like% c("TransOl")), size=0.3, color="#984ea3", alpha=0.5) +
        geom_sf(data=subset(brt, Nome %like% c("TransCarioca")), size=0.3, color="#ff7f00", alpha=0.5) +
        geom_sf(data=subset(brt, Nome%like% "TransOeste"), size=0.3, color="#377eb8") +
        geom_sf(data=subset(brt_partial, Nome %like% "TransBrasil"), size=0.3, color="#e41a1c", alpha=0.8) + # TRANSBRASIL
      
  
      # BRT stops
        geom_sf(data=subset(stopsbrt, Corredor %like% "TransOl"), size=1,  color="#984ea3", alpha=0.5) +
        geom_sf(data=subset(stopsbrt, Corredor %like% "TransCarioca"), size=1,  color="#ff7f00", alpha=0.5) +
        geom_sf(data=subset(stopsbrt, Corredor %like% "TransOeste"), size=1,  color="#377eb8", alpha=0.5) +
        geom_sf(data=subset(stopsbrt_partial, Corredor %like% "TransBrasil"), size=1,  color="#e41a1c") +
      
        theme_opts +
        theme(axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank()) +
        #facet_wrap(~year, ncol =1) +
      
        coord_sf(xlim = c(-43.45, -43.15),ylim = c(-22.95, -22.8))  # zoom



# add scale to zoomed map
tempplot_partial <- tempplot_partial +  ggsn::scalebar(dist = 5, st.size=2, height=0.01, y.min=-22.9,y.max=-22.95, x.max=-43.25, x.min= -43.3, dd2km = TRUE,  model = 'WGS84')


# partial plot
  df_full <- subset(temp_df, year == "Full")
  table(df_full$year)


tempplot_full <-
  
    basemap +
    geom_sf(data= subset(df_full, test<0 ), aes(fill= test, label=ID), alpha=0.75, color=NA) +
    scale_fill_distiller(  limits=c(0, max_val),labels=percent, palette="Reds",  direction = 1, name="Accessibility variation") + 
    
    # streets
#      geom_sf(data=streets, color="gray80", size=0.1, alpha=.7) +
    

    # METRO
      geom_sf(data=metro, size=0.3, color="gray20") +
      geom_sf(data=metrolinha4, size=0.3, color="#f781bf") +
      geom_sf(data=stopsMETRO, size=1,  color="gray20", alpha=0.9) +
      geom_sf(data=subset(stopsMETRO, Obs =="Nova Linha 4"), size=1,  color="#f781bf", alpha=0.9) +
  
    # TREM
      geom_sf(data=supervia, size=0.3, linetype = 5, color="gray50") +
      geom_sf(data=stopsTREM, size=1,  color="gray50", alpha=0.9) +
    
    # VLT
      geom_sf(data=vlt, size=0.3, color="#33a02c") +#ef6548
      geom_sf(data=stopsVLT, size=1,  color="#33a02c", alpha=0.9) +
    
    # BRTs
      geom_sf(data=subset(brt, Nome %like% c("TransOl")), size=0.3, color="#984ea3", alpha=0.5) +
      geom_sf(data=subset(brt, Nome %like% c("TransCarioca")), size=0.3, color="#ff7f00", alpha=0.5) +
      geom_sf(data=subset(brt, Nome%like% "TransOeste"), size=0.3, color="#377eb8") +
      geom_sf(data=subset(brt, Nome %like% "TransBrasil"), size=0.3, color="#e41a1c", alpha=0.8) + # TRANSBRASIL
    
    # BRT stops
      geom_sf(data=subset(stopsbrt, Corredor %like% "TransOl"), size=1,  color="#984ea3", alpha=0.5) +
      geom_sf(data=subset(stopsbrt, Corredor %like% "TransCarioca"), size=1,  color="#ff7f00", alpha=0.5) +
      geom_sf(data=subset(stopsbrt, Corredor %like% "TransOeste"), size=1,  color="#377eb8", alpha=0.5) +
      geom_sf(data=subset(stopsbrt, Corredor %like% "TransBrasil"), size=1,  color="#e41a1c") +
    
    
    theme_opts +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          legend.key.size = unit(.75, "cm"),
          legend.position=c(0.13, 0.1) # horz vert
          ) +

      
    coord_sf(xlim = c(-43.45, -43.15),ylim = c(-22.95, -22.8)) # zoom



# save
  tempplot <- plot_grid(tempplot_partial, tempplot_full, labels = c("Partial", "Full"), nrow = 2, align = "hv", hjust = 0)
  
  cat("saving zoomed in ", i)
  ggsave(tempplot, file=paste0("./plots_4_transbra/map4_diff_jobsmatch_ratio_zoom_",i,"_Reds.png"), dpi = 400, 
          width = 16, height = 18, units = "cm")




    


    
####### SEM ZOOM

tempplot_partial <-
      
      basemap +
      geom_sf(data= subset(df_partial, test>0 ), aes(fill= test, label=ID), alpha=.9, color=NA, show.legend = F) +
      scale_fill_distiller(  limits=c(0, max_val),labels=percent, palette="Reds",  direction = 1, name="Accessibility variation") + 
      
      # streets
        geom_sf(data=streets, color="gray80", size=0.08, alpha=.3) +
      
      # METRO
        geom_sf(data=metro, size=0.3, color="gray20") +
        geom_sf(data=metrolinha4, size=0.3, color="#f781bf") +
        geom_sf(data=stopsMETRO, size=0.5,  color="gray20", alpha=0.9) +
        geom_sf(data=subset(stopsMETRO, Obs =="Nova Linha 4"), size=0.5,  color="#f781bf", alpha=0.9) +
  
  
      # TREM
      geom_sf(data=supervia, size=0.3, linetype = 5, color="gray50") +
      geom_sf(data=stopsTREM, size=0.5,  color="gray50", alpha=0.9) +
      
      # VLT
      geom_sf(data=vlt, size=0.3, color="#33a02c") +#ef6548
      geom_sf(data=stopsVLT, size=0.5,  color="#33a02c", alpha=0.9) +
      
      # BRTs
      geom_sf(data=subset(brt, Nome %like% c("TransOl")), size=0.3, color="#984ea3", alpha=0.5) +
      geom_sf(data=subset(brt, Nome %like% c("TransCarioca")), size=0.3, color="#ff7f00", alpha=0.5) +
      geom_sf(data=subset(brt, Nome%like% "TransOeste"), size=0.3, color="#377eb8") +
      geom_sf(data=subset(brt_partial, Nome %like% "TransBrasil"), size=0.3, color="#e41a1c", alpha=0.8) + # TRANSBRASIL
  
    
      # BRT stops
      geom_sf(data=subset(stopsbrt, Corredor %like% "TransOl"), size=0.5,  color="#984ea3", alpha=0.5) +
      geom_sf(data=subset(stopsbrt, Corredor %like% "TransCarioca"), size=0.5,  color="#ff7f00", alpha=0.5) +
      geom_sf(data=subset(stopsbrt, Corredor %like% "TransOeste"), size=0.5,  color="#377eb8", alpha=0.5) +
      geom_sf(data=subset(stopsbrt_partial, Corredor %like% "TransBrasil"), size=0.5,  color="#e41a1c") +
  
      theme_opts +
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            
            # Legends
            legend.position=c(0.75, 0.12), # horz vert
            legend.direction='horizontal',
            legend.box='horizontal',
            legend.title=element_text(size=8),
            legend.text=element_text(size=8)
            ) 
  #  facet_wrap(~year, ncol =1) 
    
    
    
tempplot_full <-
  
  basemap +
  geom_sf(data= subset(df_full, test>0 ), aes(fill= test, label=ID), alpha=.95, color=NA) +
  scale_fill_distiller(  limits=c(0, max_val),labels=percent, palette="Reds",  direction = 1, name="Accessibility variation") + 
  
  # streets
    geom_sf(data=streets, color="gray80", size=0.08, alpha=.3) +
  
  # METRO
  geom_sf(data=metro, size=0.3, color="gray20") +
  geom_sf(data=metrolinha4, size=0.3, color="#f781bf") +
  geom_sf(data=stopsMETRO, size=0.5,  color="gray20", alpha=0.9) +
  geom_sf(data=subset(stopsMETRO, Obs =="Nova Linha 4"), size=0.5,  color="#f781bf", alpha=0.9) +
  
 
  # TREM
  geom_sf(data=supervia, size=0.3, linetype = 5, color="gray50") +
  geom_sf(data=stopsTREM, size=0.5,  color="gray50", alpha=0.9) +
  
  # VLT
  geom_sf(data=vlt, size=0.3, color="#33a02c") +#ef6548
  geom_sf(data=stopsVLT, size=0.5,  color="#33a02c", alpha=0.9) +
  
  # BRTs
  geom_sf(data=subset(brt, Nome %like% c("TransOl")), size=0.3, color="#984ea3", alpha=0.5) +
  geom_sf(data=subset(brt, Nome %like% c("TransCarioca")), size=0.3, color="#ff7f00", alpha=0.5) +
  geom_sf(data=subset(brt, Nome%like% "TransOeste"), size=0.3, color="#377eb8") +
  geom_sf(data=subset(brt, Nome %like% "TransBrasil"), size=0.3, color="#e41a1c", alpha=0.8) + # TRANSBRASIL
  
  
  # BRT stops
  geom_sf(data=subset(stopsbrt, Corredor %like% "TransOl"), size=.5,  color="#984ea3", alpha=0.5) +
  geom_sf(data=subset(stopsbrt, Corredor %like% "TransCarioca"), size=.5,  color="#ff7f00", alpha=0.5) +
  geom_sf(data=subset(stopsbrt, Corredor %like% "TransOeste"), size=.5,  color="#377eb8", alpha=0.5) +
  geom_sf(data=subset(stopsbrt, Corredor %like% "TransBrasil"), size=.5,  color="#e41a1c") +
  
  theme_opts +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        
        # Legends
        legend.position=c(0.75, 0.12), # horz vert
        legend.direction='horizontal',
        legend.box='horizontal',
        legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        legend.key.size = unit(.75, "cm"))

#  facet_wrap(~year, ncol =1) 


# save
  tempplot <- plot_grid(tempplot_partial, tempplot_full, labels = c("Partial", "Full"), nrow = 2, align = "hv", hjust = 0)

  cat("saving zoomed out ", i)
  ggsave(tempplot, file=paste0("./plots_4_transbra/map3_diff_jobsmatch_ratio_zoomOUT_",i,"_Reds.png"), dpi = 400, 
       width = 16, height = 18, units = "cm")

}
  
  beep()
  
  
  
  
############### Map 7. Schools Variation --------------------------------
  
  
  




######## ////// CHARTS CHARTS CHARTS CHARTS CHARTS CHARTS CHARTS CHARTS CHARTS  ---------------------



  
### Box plot ------------------------
  # boxplot by different travel times  
  
  baseplot2 <- theme_minimal() +
    theme( 
      axis.text.y  = element_text(face="bold")
      ,axis.text.x  = element_text(face="bold")
      ,panel.grid.minor = element_blank()
      ,strip.text = element_text(size = 11, face ="bold")
      ,legend.text = element_text(size = 11)
    )
  

for (i in c(30,60,90,120)) {
    
  
  # subset scenarios 
    temp_df <- subset(long_map, year != "Baseline")
  
  # convert scale 
    tempvar <- noquote( paste0("dif_access_ratio_",i) )
    temp_df$test <- (temp_df[, paste(tempvar)][[1]] - 1)
    temp_df <- subset(temp_df, test != 0)
    max_val <-  max(temp_df$test, na.rm = T) %>% round(digits=1)# get max e min scale value
    summary(temp_df$test, by=decile)
    
    # get average by decile and scenario
    xxx <- copy(temp_df)
  
    # average results
    setDT(xxx)[dif_access_ratio_30 !=1, .(pop=sum(pop, na.rm = T), gain=weighted.mean(x=dif_access_ratio_30, y=pop)), by=.(year)] 
    setDT(xxx)[dif_access_ratio_60 !=1, .(pop=sum(pop, na.rm = T), gain=weighted.mean(x=dif_access_ratio_60, y=pop)), by=.(year)] 
    setDT(xxx)[dif_access_ratio_90 !=1, .(pop=sum(pop, na.rm = T), gain=weighted.mean(x=dif_access_ratio_90, y=pop)), by=.(year)] 
    setDT(xxx)[dif_access_ratio_120 !=1, .(pop=sum(pop, na.rm = T), gain=weighted.mean(x=dif_access_ratio_120, y=pop)), by=.(year)] 
    
    xxx <- setDT(xxx)[, .( gain=weighted.mean(x=test, y=pop)), by=.(year, decile)]
    
    

        
    
tempplot <- 
  
  ggplot() + # aes(weight=pop)
      geom_boxplot(data=subset(temp_df, pop >0), aes(x=year, y=test, weight=pop, color=factor(decile)),
                   outlier.colour=rgb(.5,.5,.5, alpha=0.1), position=position_dodge(2)) + 
  geom_point(data=xxx, aes(x=year, y= gain, color=factor(decile))) +
  
  #geom_violin() +
  #geom_jitter(alpha=0.4) +
    scale_colour_brewer(palette = "RdBu" ) +
       scale_y_continuous(name="Change in Accessibility", limits = c(0, .5), labels = percent_format()) + 
   scale_x_discrete(name="Scenario") +
#    geom_hline(yintercept = 0) +
    facet_grid(.~decile) +
  baseplot +
  guides(color = guide_legend(title="Income Decile", nrow = 1, title.position = "top", label.position = "bottom")) +
  theme(
        axis.text.x=element_text(angle=90, hjust=1),
    # Legends
    # legend.background = element_rect(colour = NA, fill = NA),
     legend.position=c(0.5, 0.9), # horz vert
    legend.direction='horizontal',
    legend.box='horizontal',
    
    strip.background = element_blank(),
    strip.text.x = element_blank(),
     legend.title=element_text(size=9),
     legend.text=element_text(size=8),
     legend.key.size = unit(1, "cm"),
     legend.text.align=0.5
    )

ggsave(tempplot, file=paste0("./plots_4_transbra/plot2_boxplot_jobsmatch_",i,".png"), dpi = 300,
         width = 25, height = 12.5, units = "cm") 
  
  
}

  
  

  
  

##### Boxplots all times ----------------------------  
 # remove baseline
  gc(reset = T)
  
  temp_df <- subset(output_oAccess_long, year != "baseline")
  setDT(temp_df)
  
  # subset columns
  
  temp_df <- temp_df[, .(ID, year, decile, pop, dif_access_ratio_15, dif_access_ratio_30, dif_access_ratio_60, dif_access_ratio_90, dif_access_ratio_120) ]
  
  
  
# melt data for all travel times
  
  temp_df <- melt.data.table( 
    data=temp_df, 
    id.vars = c('ID', 'year', 'decile', 'pop')
    #, measure.vars = grep("dif_access_ratio_", colnames(temp_df)) 
  )


  
  temp_df$year <- factor(temp_df$year, levels=c("partial", "full"),
                          labels = c("Partial", "Full"))  
  

    
  temp_df$decile <- factor(temp_df$decile, levels=c(1:10),
                            labels = c("1\nPoorest",2:9,"10\nRichest"))  
  
  
  # chage colnames
  temp_df[, variable := gsub("dif_access_ratio_", "", variable)]
  temp_df <- temp_df[ variable != 15 ,]
  temp_df[, variable := paste0(variable, " min.") ]
  
  temp_df$variable <- factor(temp_df$variable, levels=c('30 min.','60 min.', '90 min.', '120 min.'),
                             labels = c('30 min.','60 min.', '90 min.', '120 min.'))  
  
  
  
  # remove cells with no pop
  temp_df <- temp_df[ pop >0, ]

    
# convert scale 
  temp_df$test <- (temp_df[, .(value)][[1]] - 1)
  temp_df <- subset(temp_df, test > 0)
  max_val <-  max(temp_df$test, na.rm = T) %>% round(digits=1)# get max e min scale value
  summary(temp_df$test)
  
  
  # get average by decile and scenario
  xxx <- copy(temp_df)
  
  # average results \ tabela 1 \ table 1
  
  # average entire city
  setDT(xxx)[, .(pop=sum(pop, na.rm = T), gain=weighted.mean(x=test, y=pop)), by=.(year, variable)] 
  

  # copy to clip board and paste to Excel
  x <- setDT(xxx)[, .(pop=sum(pop, na.rm = T), gain=weighted.mean(x=test, y=pop)), by=.(year, variable)] 
  write.table(x, "clipboard", sep="\t", row.names=FALSE, col.names=T)
  
  # average b decile
  xxx <- setDT(xxx)[, .( gain=weighted.mean(x=test, y=pop)), by=.(year, decile, variable) ][order(year,variable,decile)]
  
  


output_oAccess_wide[ diff_ratio_oaccess_jobsmatch_full_90 !=1, .(sum(pop))]  
output_oAccess_wide[ diff_ratio_oaccess_jobsmatch_full_120 !=1, .(sum(pop))]


output_oAccess_wide[ diff_ratio_oaccess_jobsmatch_partial_90 !=1, .(sum(pop))]  
output_oAccess_wide[ diff_ratio_oaccess_jobsmatch_partial_120 !=1, .(sum(pop))]

# average houseold income per capita
  output_oAccess_wide[, .( mean_income=weighted.mean(x=income, y=pop, na.rm = T))]
     
  
# average access gain
  temp_df[, .(avg_gain=mean(test)), by=variable]
  
  
outlier_rich<- subset(temp_df, pop >0 & variable=="60 min." & decile >6)

outlier_rich <- outlier_rich[test > 1]
  
#### Box Plot  
  
  tempplot <- 
    
    ggplot() + # aes(weight=pop)
    geom_boxplot(data=subset(temp_df, pop >0), aes(x=year, y=test, weight=pop, color=factor(decile)),
                 outlier.colour=rgb(.5,.5,.5, alpha=0.1), position=position_dodge(2), size=.3) + 
    geom_point(data=xxx, aes(x=year, y= gain, color=factor(decile))) +
    
    #geom_violin() +
    #geom_jitter(alpha=0.4) +
    scale_colour_brewer(palette = "RdBu" ) +
    scale_y_continuous(name="Change in Accessibility", limits = c(0, .5), labels = percent_format()) + 
    scale_x_discrete(name="Scenario") +
    #    geom_hline(yintercept = 0) +
    facet_grid(variable~decile) +
    baseplot +
    guides(color = guide_legend(title="Income Decile", nrow = 1, title.position = "top", label.position = "bottom")) +
    theme(
      axis.text.x=element_text(angle=90, hjust=1),
      # Legends
      # legend.background = element_rect(colour = NA, fill = NA),
      #      legend.position=c(0.5, 0.9), # horz vert
      legend.position= "top", # horz vert
      legend.direction='horizontal',
      legend.box='horizontal',
      
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      legend.title=element_text(size=9),
      legend.text=element_text(size=8),
      legend.key.size = unit(1, "cm"),
      legend.text.align=0.5
    )
  
  ggsave(tempplot, file=paste0("./plots_4_transbra/plot2_boxplot_jobsmatch_alltimes2.png"), dpi = 300,
         width = 17, height = 19, units = "cm") 
  
  
  

  
#### Scatter plot ------------------------


baseplot1 <- theme_minimal() +
  theme( 
    axis.text.y  = element_text(face="bold", size=9)
    ,axis.text.x  = element_text(face="bold", size=9)
    ,panel.grid.minor = element_blank()
    ,strip.text = element_text(size = 11, face ="bold")
    ,legend.text = element_text(size = 11)
  )

  

  
for (i in c(30, 60,90,120)) {
  
  
# vars
  b <- noquote( paste0("prop_med_oaccess_jobsmatch_",i,"_baseline") ) 
  p <- noquote( paste0("prop_med_oaccess_jobsmatch_",i,"_partial") ) 
  f <- noquote( paste0("prop_med_oaccess_jobsmatch_",i,"_full") ) 
  
  df_partial <- subset(wide_map, get(b) != get(p))
  df_full <- subset(wide_map, get(b) != get(f))
  
  

      
  # population affected
    pop_partial <- sum(df_partial$pop, na.rm = T) /1000000
    pop_partial <- round(pop_partial, digits = 2)
    pop_partial_prop <- sum(df_partial$pop, na.rm = T) / sum(wide_map$pop, na.rm = T) *100
    pop_partial_prop <- round(pop_partial_prop, digits = 1)
  
  
    pop_full <- sum(df_full$pop, na.rm = T) /1000000
    pop_full <- round(pop_full, digits = 2)
    pop_full_prop <- sum(df_full$pop, na.rm = T) / sum(wide_map$pop, na.rm = T) *100
    pop_full_prop <- round(pop_full_prop, digits = 1)
    
  
  

    
# plot
  
tempplot_partial <- 
  
    ggplot(data=subset(df_partial, pop > 0  ), aes(x=get(b), y=get(p))) +
    geom_point(aes(colour = factor(decile), size=pop/1000), alpha = 0.4, show.legend = F) +
    scale_colour_brewer(palette = "RdBu") +
    labs(size="Population\nin thousands", colour="Income Decile") +
    #xlim(0, 60) + ylim(0, 60) + # axis limits
    geom_abline() +
    scale_x_continuous(name="Baseline", labels=percent) +  # limits=c(0,.70),
    scale_y_continuous(name="Partial Operation", labels=percent) + # limits=c(0,.70),
    baseplot1 +
    coord_fixed(ratio=0.9) +
    
    guides(color = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
    guides(size = guide_legend(nrow = 1,  label.position = "bottom")) +
    
    theme(
      # Legends
      legend.position=c(0.75, 0.14), # horz vert
      legend.direction='horizontal',
      legend.box='vertical',
      legend.title=element_text(size=8),
      legend.text=element_text(size=7),
      legend.key.size = unit(.4, "cm"),
      legend.text.align=0.5,
      # axis
      axis.ticks=element_blank()) + 
      ggtitle("Partial scenario") +

  annotate("text", x = -Inf, y = Inf, hjust = -.2, vjust = 3.5, size=3, color="gray30",
           label = paste0("Population affected:\n ", pop_partial_prop,"% (",pop_partial," mi.)"))

  
    


tempplot_full <- 
  
  ggplot(data=subset(df_full, pop > 0  ), aes(x=get(b), y=get(f))) +
  geom_point(aes(colour = factor(decile), size=pop/1000), alpha = 0.4) +
  scale_colour_brewer(palette = "RdBu") +
  labs(size="Population\nin thousands", colour="Income Decile") +
  #xlim(0, 60) + ylim(0, 60) + # axis limits
  geom_abline() +
  scale_x_continuous(name="Baseline", labels=percent) +  # limits=c(0,.70),
  scale_y_continuous(name="Full Operation", labels=percent) + # limits=c(0,.70),
  baseplot1 +
  coord_fixed(ratio=0.9) +
  
  guides(color = guide_legend(nrow = 1, title.position = "top", label.position = "bottom", override.aes = list(size=2.5))) +
  guides(size = guide_legend(nrow = 1,  label.position = "bottom")) +
  
  
  theme(
    # Legends
    legend.position=c(0.71, 0.18), # horz vert
    legend.direction='horizontal',
    legend.box='vertical',
    legend.title=element_text(size=8),
    legend.text=element_text(size=7),
    legend.key.size = unit(.4, "cm"),
    legend.text.align=0.5,
    # axis
    axis.ticks=element_blank()) +
    ggtitle("Full scenario") +
    annotate("text", x = -Inf, y = Inf, hjust = -.2, vjust = 3.5, size=3, color="gray30",
             label = paste0("Population affected:\n ", pop_full_prop,"% (",pop_full," mi.)"))



# save
  tempplot <- plot_grid(tempplot_partial, tempplot_full,  nrow = 1, align = "hv", hjust = 0)
  
  ggsave(tempplot, file=paste0("./plots_4_transbra/plot1_scatter",i,"222.png") , dpi = 300,
           width = 25, height = 12.5, units = "cm") 

}
  
  
  

  