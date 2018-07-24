


#### a SINGLE Scatter plot for all travel times ------------------------


baseplot1 <- theme_minimal() +
  theme( 
    axis.text.y  = element_text(face="bold", size=9)
    ,axis.text.x  = element_text(face="bold", size=9)
    ,panel.grid.minor = element_blank()
    ,strip.text = element_text(size = 11, face ="bold")
    ,legend.text = element_text(size = 11)
  )







# get data.table
wide_dt <- copy(wide_map) %>% setDT()
  

# separate scenarios
    df_baseline <- wide_dt[, .(ID, pop, decile, prop_med_oaccess_jobsmatch_30_baseline, prop_med_oaccess_jobsmatch_60_baseline, prop_med_oaccess_jobsmatch_90_baseline, prop_med_oaccess_jobsmatch_120_baseline)]
    df_partial <- wide_dt[, .(ID, pop, decile, prop_med_oaccess_jobsmatch_30_partial, prop_med_oaccess_jobsmatch_60_partial, prop_med_oaccess_jobsmatch_90_partial, prop_med_oaccess_jobsmatch_120_partial)]
    df_full <- wide_dt[, .(ID, pop, decile, prop_med_oaccess_jobsmatch_30_full, prop_med_oaccess_jobsmatch_60_full, prop_med_oaccess_jobsmatch_90_full, prop_med_oaccess_jobsmatch_120_full)]
    
    df_baseline[, scenario := "Baseline"]
     df_partial[, scenario := "Partial operation"]
        df_full[, scenario := "Full Operation"]

            
# melt scenario travel times    
  df_baseline_melted <- melt.data.table(data= df_baseline, id.vars = c("ID", "pop","decile", "scenario"))
  df_partial_melted <- melt.data.table(data= df_partial, id.vars = c("ID", "pop","decile", "scenario"))
  df_full_melted <- melt.data.table(data= df_full, id.vars = c("ID", "pop","decile", "scenario"))
    
# Create time column with minutes
  df_baseline_melted[, time := ifelse(variable %like% "120", 120, 
                                      ifelse(variable %like% "90", 90,
                                             ifelse(variable %like% "60", 60,
                                                    ifelse(variable %like% "30", 30, NA))))]
  
  df_partial_melted[, time := ifelse(variable %like% "120", 120, 
                                      ifelse(variable %like% "90", 90,
                                             ifelse(variable %like% "60", 60,
                                                    ifelse(variable %like% "30", 30, NA))))]
  
  df_full_melted[, time := ifelse(variable %like% "120", 120, 
                                      ifelse(variable %like% "90", 90,
                                             ifelse(variable %like% "60", 60,
                                                    ifelse(variable %like% "30", 30, NA))))]
        
        
  
  df_baseline_melted[, variable := NULL][, scenario := NULL]
  df_partial_melted[, variable := NULL]
  df_full_melted[, variable := NULL]
  
  
# pile up partial and full scenarios
  df_pile <- rbind(df_partial_melted, df_full_melted)
  setnames(df_pile, 'value', 'value_y')
  head(df_pile)

# add Baseline scenario in a wide format  
  # a <- df_pile[df_baseline_melted, on=c("ID", "pop", "decile", "time"), ]
  df_pile <- left_join(df_pile, df_baseline_melted, by = c("ID", "pop", "decile", "time")) %>% setDT()
  setnames(df_pile, 'value', 'value_x')
  head(df_pile)
  
  
# change time labels
  df_pile$time <- factor(df_pile$time, levels=c(30, 60, 90, 120),
                            labels = c("30 min", "60 min", "90 min", "120 min"))  
  


  
  
tempplot_full <-  
    
    ggplot(data=subset(df_pile, pop > 0 & scenario %like% "Full" ), aes(x=value_x, y=value_y)) +
    geom_point(aes(colour = factor(decile), size=pop/1000), alpha = 0.4, show.legend=T) +
    scale_colour_brewer(palette = "RdBu") +
    labs(size="Population in thousands", colour="Income Decile") +
    baseplot1 +
    geom_abline() +
    facet_wrap(~time, ncol=4) +
    scale_x_continuous(name="Baseline", labels=percent, limits=c(0,1)) + 
    scale_y_continuous(name="Full Scenario", labels=percent, limits=c(0,1)) + 
    
    coord_fixed(ratio=.9) + # ratio=0.9
    
    guides(color = guide_legend(nrow = 1, title.position = "top", label.position = "bottom", override.aes = list(size=5))) +
    guides(size = guide_legend(nrow = 1,  label.position = "bottom")) +
    
    theme(
      # Legends
      legend.position="bottom", # horz vert
      legend.direction='horizontal',
      legend.box='vertical',
      legend.title=element_text(size=8),
      legend.text=element_text(size=7),
      legend.key.size = unit(.2, "cm"),
      legend.text.align=0.5,
      # axis
      axis.ticks=element_blank())
  
  
tempplot_partial <-  
  
  ggplot(data=subset(df_pile, pop > 0 & scenario %like% "Partial" ), aes(x=value_x, y=value_y)) +
  geom_point(aes(colour = factor(decile), size=pop/1000), alpha = 0.4, show.legend=F) +
  scale_colour_brewer(palette = "RdBu") +
  labs(size="Population in thousands", colour="Income Decile") +
  baseplot1 +
  geom_abline() +
  facet_wrap(~time, ncol=4) +
  scale_x_continuous(name="Baseline", labels=percent, limits=c(0,1)) + 
  scale_y_continuous(name="Partial Scenario", labels=percent, limits=c(0,1)) + 
  
  coord_fixed(ratio=.9) + # 
  
  guides(color = guide_legend(nrow = 1, title.position = "top", label.position = "bottom", override.aes = list(size=5))) +
  guides(size = guide_legend(nrow = 1,  label.position = "bottom")) +
  
  theme(
    # Legends
    legend.position="bottom", # horz vert
    legend.direction='horizontal',
    legend.box='vertical',
    legend.title=element_text(size=8),
    legend.text=element_text(size=7),
    legend.key.size = unit(.2, "cm"),
    legend.text.align=0.5,
    # axis
    axis.ticks=element_blank())




# save
tempplot <- ggpubr::ggarrange(tempplot_full, tempplot_partial,   nrow=2, common.legend = TRUE, align="hv", legend="bottom")


cat("saving zoomed both Full and Partial")
ggsave(tempplot, file= "./plots_4_transbra/plot_scatter_partialtttttttttttttt.png.png", dpi = 400, 
       width = 23, height = 15, units = "cm") ;  beep()





