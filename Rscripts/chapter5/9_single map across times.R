


##### MAP all times_ zoom OUT Full ----------------------------  

i = "Full"
  
# remove baseline
  temp_df <- subset(long_map, year == i )
  setDT(temp_df)

# subset columns
  temp_df <- temp_df[, c('ID', 'year', 'decile', 'pop', 'dif_access_ratio_15', 'dif_access_ratio_30', 'dif_access_ratio_60', 'dif_access_ratio_90', 'dif_access_ratio_120') ]



# melt data for all travel times
  temp_df_melt <- melt.data.table( data=temp_df, 
                                   id.vars = c('ID', 'year', 'decile', 'pop'), 
                                   measure.vars = grep("dif_access_ratio_", colnames(temp_df)) 
                                  )

# chage colnames
  temp_df_melt[, variable := gsub("dif_access_ratio_", "", variable)]
  temp_df_melt <- temp_df_melt[ variable != 15 ,]
  temp_df_melt[, variable := paste0(variable, " min.") ]
  
  temp_df_melt$variable <- factor(temp_df_melt$variable, levels=c('30 min.','60 min.', '90 min.', '120 min.'),
                             labels = c('30 min.','60 min.', '90 min.', '120 min.'))  
  



# convert scale 
  temp_df_melt$test <- (temp_df_melt[, .(value)][[1]] - 1)
  temp_df_melt <- subset(temp_df_melt, test != 0)
  max_val <-  max(temp_df_melt$test, na.rm = T) %>% round(digits=1)# get max e min scale value
  summary(temp_df_melt$test, by=decile)
  
  # TRUNCATE to 1
    summary(temp_df_melt$test)
    temp_df_melt$test <- ifelse(temp_df_melt$test > 1, 1, temp_df_melt$test)
    summary(temp_df_melt$test)
  
    
# Add Geometry
  temp_sf <- as.data.frame(temp_df_melt) # convert do data.frame
  temp_sf <- left_join(temp_sf,   long_map[, c("ID", "geometry")], by="ID") # merge to add geometry
  temp_sf <- unique(temp_sf)  
  
  temp_sf <- st_sf(temp_sf) # convert to sf
  class(temp_sf)
  

  



#### PLOT
  
# temp_sf <- subset(temp_sf, variable =="90 min.") # 666666666666666666666666666666666666666666666
  
tempplot_out_full <- 
    basemap +
  
  # streets
    geom_sf(data=streets, color="gray80", size=0.085, alpha=.3) +
  
  
   # ggplot()+ 
    geom_sf(data= subset(temp_sf, test>0 ), aes(fill= test, label=ID), alpha=0.5, color=NA, show.legend=T) +
    scale_fill_distiller(  limits=c(0, 1),labels=percent, palette="Reds",  direction = 1, name="Accessibility variation") + 
    facet_wrap(~variable, ncol = 1) +
    
  
  
  # METRO
  geom_sf(data=metro, size=0.3, color="gray20") +
  geom_sf(data=metrolinha4, size=0.3, color="#f781bf") +
#  geom_sf(data=stopsMETRO, size=1,  color="gray20", alpha=0.9) +
#  geom_sf(data=subset(stopsMETRO, Obs =="Nova Linha 4"), size=1,  color="#f781bf", alpha=0.9) +
  
  # TREM
  geom_sf(data=supervia, size=0.3, linetype = 5, color="gray50") +
#  geom_sf(data=stopsTREM, size=1,  color="gray50", alpha=0.9) +
  
  # VLT
  geom_sf(data=vlt, size=0.3, color="#33a02c") +#ef6548
#  geom_sf(data=stopsVLT, size=1,  color="#33a02c", alpha=0.9) +
  
  # BRTs
  geom_sf(data=subset(brt, Nome %like% c("TransOl")), size=0.3, color="#984ea3", alpha=0.5) +
  geom_sf(data=subset(brt, Nome %like% c("TransCarioca")), size=0.3, color="#ff7f00", alpha=0.5) +
  geom_sf(data=subset(brt, Nome%like% "TransOeste"), size=0.3, color="#377eb8") +
  geom_sf(data=subset(brt, Nome %like% "TransBrasil"), size=0.3, color="#e41a1c", alpha=0.8) + # TRANSBRASIL
  
# # BRT stops
# geom_sf(data=subset(stopsbrt, Corredor %like% "TransOl"), size=1,  color="#984ea3", alpha=0.5) +
# geom_sf(data=subset(stopsbrt, Corredor %like% "TransCarioca"), size=1,  color="#ff7f00", alpha=0.5) +
# geom_sf(data=subset(stopsbrt, Corredor %like% "TransOeste"), size=1,  color="#377eb8", alpha=0.5) +
# geom_sf(data=subset(stopsbrt, Corredor %like% "TransBrasil"), size=1,  color="#e41a1c") +
  
  
  theme_opts +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.key.width = unit(.7, "cm"),
        # legend.key.size = unit(.75, "cm"),
        legend.position= "bottom" # horz vert
        ) +
  
  coord_sf(xlim = c(-43.704, -43.181),ylim = c(-23.01, -22.795)) # zoom






##### MAP all times_ zoom OUT Partial ----------------------------  

i = "Partial" 
  
  # remove baseline
  temp_df <- subset(long_map, year == i )
  setDT(temp_df)
  
  # subset columns
  temp_df <- temp_df[, c('ID', 'year', 'decile', 'pop', 'dif_access_ratio_15', 'dif_access_ratio_30', 'dif_access_ratio_60', 'dif_access_ratio_90', 'dif_access_ratio_120') ]
  
  
  
  # melt data for all travel times
  temp_df_melt <- melt.data.table( data=temp_df, 
                                   id.vars = c('ID', 'year', 'decile', 'pop'), 
                                   measure.vars = grep("dif_access_ratio_", colnames(temp_df)) 
  )
  
  # chage colnames
  temp_df_melt[, variable := gsub("dif_access_ratio_", "", variable)]
  temp_df_melt <- temp_df_melt[ variable != 15 ,]
  temp_df_melt[, variable := paste0(variable, " min.") ]
  
  temp_df_melt$variable <- factor(temp_df_melt$variable, levels=c('30 min.','60 min.', '90 min.', '120 min.'),
                                  labels = c('30 min.','60 min.', '90 min.', '120 min.'))  
  
  
  
  
  # convert scale 
  temp_df_melt$test <- (temp_df_melt[, .(value)][[1]] - 1)
  temp_df_melt <- subset(temp_df_melt, test != 0)
  max_val <-  max(temp_df_melt$test, na.rm = T) %>% round(digits=1)# get max e min scale value
  summary(temp_df_melt$test, by=decile)
  
  # TRUNCATE to 1
  summary(temp_df_melt$test)
  temp_df_melt$test <- ifelse(temp_df_melt$test > 1, 1, temp_df_melt$test)
  summary(temp_df_melt$test)
  
  
  # Add Geometry
  temp_sf <- as.data.frame(temp_df_melt) # convert do data.frame
  temp_sf <- left_join(temp_sf,   long_map[, c("ID", "geometry")], by="ID") # merge to add geometry
  temp_sf <- unique(temp_sf)  
  
  temp_sf <- st_sf(temp_sf) # convert to sf
  class(temp_sf)
  
  
#### PLOT

#  temp_sf <- subset(temp_sf, variable =="90 min.") # 666666666666666666666666666666666666666666666
  
tempplot_out_partial <- 
    basemap +
  
  
   # streets
    geom_sf(data=streets, color="gray80", size=0.085, alpha=.3) +
  
  
    # ggplot()+ 
    geom_sf(data= subset(temp_sf, test>0 ), aes(fill= test, label=ID), alpha=0.5, color=NA, show.legend=T) +
    scale_fill_distiller(  limits=c(0, 1),labels=percent, palette="Reds",  direction = 1, name="Accessibility variation") + 
    facet_wrap(~variable, ncol = 1) +
    
    
    
    # METRO
    geom_sf(data=metro, size=0.3, color="gray20") +
    geom_sf(data=metrolinha4, size=0.3, color="#f781bf") +
    #  geom_sf(data=stopsMETRO, size=1,  color="gray20", alpha=0.9) +
    #  geom_sf(data=subset(stopsMETRO, Obs =="Nova Linha 4"), size=1,  color="#f781bf", alpha=0.9) +
    
    # TREM
    geom_sf(data=supervia, size=0.3, linetype = 5, color="gray50") +
    #  geom_sf(data=stopsTREM, size=1,  color="gray50", alpha=0.9) +
    
    # VLT
    geom_sf(data=vlt, size=0.3, color="#33a02c") +#ef6548
    #  geom_sf(data=stopsVLT, size=1,  color="#33a02c", alpha=0.9) +
    
    # BRTs
    geom_sf(data=subset(brt, Nome %like% c("TransOl")), size=0.3, color="#984ea3", alpha=0.5) +
    geom_sf(data=subset(brt, Nome %like% c("TransCarioca")), size=0.3, color="#ff7f00", alpha=0.5) +
    geom_sf(data=subset(brt, Nome%like% "TransOeste"), size=0.3, color="#377eb8") +
    geom_sf(data=subset(brt_partial, Nome %like% "TransBrasil"), size=0.3, color="#e41a1c", alpha=0.8) + # TRANSBRASIL
    
    # # BRT stops
    # geom_sf(data=subset(stopsbrt, Corredor %like% "TransOl"), size=1,  color="#984ea3", alpha=0.5) +
    # geom_sf(data=subset(stopsbrt, Corredor %like% "TransCarioca"), size=1,  color="#ff7f00", alpha=0.5) +
    # geom_sf(data=subset(stopsbrt, Corredor %like% "TransOeste"), size=1,  color="#377eb8", alpha=0.5) +
    # geom_sf(data=subset(stopsbrt, Corredor %like% "TransBrasil"), size=1,  color="#e41a1c") +
    
    
    theme_opts +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          legend.key.width = unit(.7, "cm"),
          # legend.key.size = unit(.75, "cm"),
          legend.position= "bottom" # horz vert
    ) +
    
    coord_sf(xlim = c(-43.704, -43.181),ylim = c(-23.01, -22.795)) # zoom

  






# save
  tempplot <- ggpubr::ggarrange(tempplot_out_full, tempplot_out_partial, ncol=2, nrow=1, labels = c("(A)", "(B)"), common.legend = TRUE, legend="bottom")
#  beep()

  
cat("saving zoomed both Full and Partial")
ggsave(tempplot, file= "./plots_4_transbra/map33_diff_jobsmatch_ratio_zoom_out-long.png", dpi = 400, 
       width = 17, height = 20, units = "cm") ;  beep()






#################################
# tabela 1
    


  
  setDT(temp_df_melt)[, .(pop=sum(pop, na.rm = T), gain=weighted.mean(x=test, y=pop)), by=.(year, variable)] 
  
  

  
  
##### MAP all times_ zoom IN Partial ----------------------------  
  
i = "Partial"
    
    # remove baseline
    temp_df <- subset(long_map, year == i )
    setDT(temp_df)
    
    # subset columns
    temp_df <- temp_df[, c('ID', 'year', 'decile', 'pop', 'dif_access_ratio_15', 'dif_access_ratio_30', 'dif_access_ratio_60', 'dif_access_ratio_90', 'dif_access_ratio_120') ]
    
    
    
    # melt data for all travel times
    temp_df_melt <- melt.data.table( data=temp_df, 
                                     id.vars = c('ID', 'year', 'decile', 'pop'), 
                                     measure.vars = grep("dif_access_ratio_", colnames(temp_df)) 
    )
    
    # chage colnames
    temp_df_melt[, variable := gsub("dif_access_ratio_", "", variable)]
    temp_df_melt <- temp_df_melt[ variable != 15 ,]
    temp_df_melt[, variable := paste0(variable, " min.") ]
    
    temp_df_melt$variable <- factor(temp_df_melt$variable, levels=c('30 min.','60 min.', '90 min.', '120 min.'),
                                    labels = c('30 min.','60 min.', '90 min.', '120 min.'))  
    
    
    
    
    # convert scale 
    temp_df_melt$test <- (temp_df_melt[, .(value)][[1]] - 1)
    temp_df_melt <- subset(temp_df_melt, test != 0)
    max_val <-  max(temp_df_melt$test, na.rm = T) %>% round(digits=1)# get max e min scale value
    summary(temp_df_melt$test, by=decile)
    
    # TRUNCATE to 1
    summary(temp_df_melt$test)
    temp_df_melt$test <- ifelse(temp_df_melt$test > 1, 1, temp_df_melt$test)
    summary(temp_df_melt$test)
    
    
    # Add Geometry
    temp_sf <- as.data.frame(temp_df_melt) # convert do data.frame
    temp_sf <- left_join(temp_sf,   long_map[, c("ID", "geometry")], by="ID") # merge to add geometry
    temp_sf <- unique(temp_sf)  
    
    temp_sf <- st_sf(temp_sf) # convert to sf
    class(temp_sf)
    
    
#### PLOT

tempplot_in_partial <-  
      basemap +
  
  # streets
      geom_sf(data=streets, color="gray80", size=0.09, alpha=.3) +
  
      # ggplot()+ 
      geom_sf(data= subset(temp_sf, test>0 ), aes(fill= test, label=ID), alpha=0.75, color=NA, show.legend=T) +
      scale_fill_distiller(  limits=c(0, 1),labels=percent, palette="Reds",  direction = 1, name="Accessibility variation") + 
      facet_wrap(~variable, ncol = 4) +
      
      
      # METRO
      geom_sf(data=metro, size=0.3, color="gray20") +
      geom_sf(data=metrolinha4, size=0.3, color="#f781bf") +
      #  geom_sf(data=stopsMETRO, size=1,  color="gray20", alpha=0.9) +
      #  geom_sf(data=subset(stopsMETRO, Obs =="Nova Linha 4"), size=1,  color="#f781bf", alpha=0.9) +
      
      # TREM
      geom_sf(data=supervia, size=0.3, linetype = 5, color="gray50") +
      #  geom_sf(data=stopsTREM, size=1,  color="gray50", alpha=0.9) +
      
      # VLT
      geom_sf(data=vlt, size=0.3, color="#33a02c") +#ef6548
      #  geom_sf(data=stopsVLT, size=1,  color="#33a02c", alpha=0.9) +
      
      # BRTs
      geom_sf(data=subset(brt, Nome %like% c("TransOl")), size=0.3, color="#984ea3", alpha=0.5) +
      geom_sf(data=subset(brt, Nome %like% c("TransCarioca")), size=0.3, color="#ff7f00", alpha=0.5) +
      geom_sf(data=subset(brt, Nome%like% "TransOeste"), size=0.3, color="#377eb8") +
      geom_sf(data=subset(brt_partial, Nome %like% "TransBrasil"), size=0.3, color="#e41a1c", alpha=0.8) + # TRANSBRASIL
      
      # # BRT stops
      # geom_sf(data=subset(stopsbrt, Corredor %like% "TransOl"), size=1,  color="#984ea3", alpha=0.5) +
      # geom_sf(data=subset(stopsbrt, Corredor %like% "TransCarioca"), size=1,  color="#ff7f00", alpha=0.5) +
      # geom_sf(data=subset(stopsbrt, Corredor %like% "TransOeste"), size=1,  color="#377eb8", alpha=0.5) +
      # geom_sf(data=subset(stopsbrt, Corredor %like% "TransBrasil"), size=1,  color="#e41a1c") +
      
      
      theme_opts +
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            legend.key.width = unit(.7, "cm"),
            # legend.key.size = unit(.75, "cm"),
            legend.position= "bottom" # horz vert
      ) +
      
   #   coord_sf(xlim = c(-43.71, -43.181),ylim = c(-23.01, -22.795)) # zoom
    
    coord_sf(xlim = c(-43.45, -43.181),ylim = c(-23.01, -22.8))  # zoom
    
    
    

  
  
  
  
  
  
##### MAP all times_ zoom IN Full ----------------------------  
  
i = "Full"
    
    # remove baseline
    temp_df <- subset(long_map, year == i )
    setDT(temp_df)
    
    # subset columns
    temp_df <- temp_df[, c('ID', 'year', 'decile', 'pop', 'dif_access_ratio_15', 'dif_access_ratio_30', 'dif_access_ratio_60', 'dif_access_ratio_90', 'dif_access_ratio_120') ]
    
    
    
    # melt data for all travel times
    temp_df_melt <- melt.data.table( data=temp_df, 
                                     id.vars = c('ID', 'year', 'decile', 'pop'), 
                                     measure.vars = grep("dif_access_ratio_", colnames(temp_df)) 
    )
    
    # chage colnames
    temp_df_melt[, variable := gsub("dif_access_ratio_", "", variable)]
    temp_df_melt <- temp_df_melt[ variable != 15 ,]
    temp_df_melt[, variable := paste0(variable, " min.") ]
    
    temp_df_melt$variable <- factor(temp_df_melt$variable, levels=c('30 min.','60 min.', '90 min.', '120 min.'),
                                    labels = c('30 min.','60 min.', '90 min.', '120 min.'))  
    
    
    
    
    # convert scale 
    temp_df_melt$test <- (temp_df_melt[, .(value)][[1]] - 1)
    temp_df_melt <- subset(temp_df_melt, test != 0)
    max_val <-  max(temp_df_melt$test, na.rm = T) %>% round(digits=1)# get max e min scale value
    summary(temp_df_melt$test, by=decile)
    
    # TRUNCATE to 1
    summary(temp_df_melt$test)
    temp_df_melt$test <- ifelse(temp_df_melt$test > 1, 1, temp_df_melt$test)
    summary(temp_df_melt$test)
    
    
    # Add Geometry
    temp_sf <- as.data.frame(temp_df_melt) # convert do data.frame
    temp_sf <- left_join(temp_sf,   long_map[, c("ID", "geometry")], by="ID") # merge to add geometry
    temp_sf <- unique(temp_sf)  
    
    temp_sf <- st_sf(temp_sf) # convert to sf
    class(temp_sf)
    
    
#### PLOT
    
tempplot_in_full <- 
      basemap +
  
  # streets
    geom_sf(data=streets, color="gray80", size=0.09, alpha=.3) +
  
  # ggplot()+ 
      geom_sf(data= subset(temp_sf, test>0 ), aes(fill= test, label=ID), alpha=0.5, color=NA, show.legend=F) +
      scale_fill_distiller(  limits=c(0, 1),labels=percent, palette="Reds",  direction = 1, name="Accessibility variation") + 
      facet_wrap(~variable, ncol = 4) +
      
      
      
      # METRO
      geom_sf(data=metro, size=0.3, color="gray20") +
      geom_sf(data=metrolinha4, size=0.3, color="#f781bf") +
      #  geom_sf(data=stopsMETRO, size=1,  color="gray20", alpha=0.9) +
      #  geom_sf(data=subset(stopsMETRO, Obs =="Nova Linha 4"), size=1,  color="#f781bf", alpha=0.9) +
      
      # TREM
      geom_sf(data=supervia, size=0.3, linetype = 5, color="gray50") +
      #  geom_sf(data=stopsTREM, size=1,  color="gray50", alpha=0.9) +
      
      # VLT
      geom_sf(data=vlt, size=0.3, color="#33a02c") +#ef6548
      #  geom_sf(data=stopsVLT, size=1,  color="#33a02c", alpha=0.9) +
      
      # BRTs
      geom_sf(data=subset(brt, Nome %like% c("TransOl")), size=0.3, color="#984ea3", alpha=0.5) +
      geom_sf(data=subset(brt, Nome %like% c("TransCarioca")), size=0.3, color="#ff7f00", alpha=0.5) +
      geom_sf(data=subset(brt, Nome%like% "TransOeste"), size=0.3, color="#377eb8") +
      geom_sf(data=subset(brt, Nome %like% "TransBrasil"), size=0.3, color="#e41a1c", alpha=0.8) + # TRANSBRASIL
      
      # # BRT stops
      # geom_sf(data=subset(stopsbrt, Corredor %like% "TransOl"), size=1,  color="#984ea3", alpha=0.5) +
      # geom_sf(data=subset(stopsbrt, Corredor %like% "TransCarioca"), size=1,  color="#ff7f00", alpha=0.5) +
      # geom_sf(data=subset(stopsbrt, Corredor %like% "TransOeste"), size=1,  color="#377eb8", alpha=0.5) +
      # geom_sf(data=subset(stopsbrt, Corredor %like% "TransBrasil"), size=1,  color="#e41a1c") +
      
      
      theme_opts +
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            legend.key.width = unit(.7, "cm"),
            # legend.key.size = unit(.75, "cm"),
            legend.position= "bottom" # horz vert
      ) +
      
      #   coord_sf(xlim = c(-43.71, -43.181),ylim = c(-23.01, -22.795)) # zoom
      
      coord_sf(xlim = c(-43.45, -43.181),ylim = c(-23.01, -22.8))  # zoom
    
    
    

    
    
    
# save
tempplot <- plot_grid(tempplot_in_full, tempplot_in_partial, labels = c("(A)", "(B)"), nrow = 2, align = "vh", hjust = 0)

cat("saving zoomed both Full and Partial")
ggsave(tempplot, file= "./plots_4_transbra/map33_diff_jobsmatch_ratio_zoom_in.png", dpi = 400, 
       width = 23, height = 14, units = "cm") ;  beep()


  
  



