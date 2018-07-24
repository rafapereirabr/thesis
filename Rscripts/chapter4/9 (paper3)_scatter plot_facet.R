


##### A. Read Spatial Grids ----------

# read maps
hex_0500 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_0500')
hex_1000 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_1000')
hex_2000 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_2000')
hex_4000 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_4000')
map_trzn <- st_read(dsn = './shapefiles_OD_rio_2012', layer ='map_grid_trzn')




##### B. Mix data ----------

for (i in c('0500', '1000', '2000', '4000', 'trzn')){
  
  # Read accesibility data of each grid
  oaccess_wide_mix <- read_csv(paste0("./accessibility/output_oAccess_wide_",i,"_201404_2017mix.csv"))
  oaccess_wide_counter <- read_csv(paste0("./accessibility/output_oAccess_wide_",i,"_201404_2017counterfactual.csv"))
  
  # add grid info
  oaccess_wide_mix$grid <- NULL
  oaccess_wide_counter$grid <- NULL
  
  # assign corresponding names
  assign(paste0("oaccess_wide_mix_",i), oaccess_wide_mix)
  assign(paste0("oaccess_wide_counter_",i), oaccess_wide_counter)
  rm(oaccess_wide_mix, oaccess_wide_counter)
}

# Merge data WIDE maps
hex_0500_wide_counter <- left_join( hex_0500, oaccess_wide_counter_0500, by="ID")
hex_1000_wide_counter <- left_join( hex_1000, oaccess_wide_counter_1000, by="ID")
hex_2000_wide_counter <- left_join( hex_2000, oaccess_wide_counter_2000, by="ID")
hex_4000_wide_counter <- left_join( hex_4000, oaccess_wide_counter_4000, by="ID")
map_trzn_wide_counter <- left_join( map_trzn, oaccess_wide_counter_trzn, by="ID")


# Merge data WIDE maps
hex_0500_wide_mix <- left_join( hex_0500, oaccess_wide_mix_0500, by="ID")
hex_1000_wide_mix <- left_join( hex_1000, oaccess_wide_mix_1000, by="ID")
hex_2000_wide_mix <- left_join( hex_2000, oaccess_wide_mix_2000, by="ID")
hex_4000_wide_mix <- left_join( hex_4000, oaccess_wide_mix_4000, by="ID")
map_trzn_wide_mix <- left_join( map_trzn, oaccess_wide_mix_trzn, by="ID")

# Rbind maps
wide_map_counter <- rbind(hex_0500_wide_counter, hex_1000_wide_counter, hex_2000_wide_counter, hex_4000_wide_counter, map_trzn_wide_counter)
wide_map_mix <- rbind(hex_0500_wide_mix, hex_1000_wide_mix, hex_2000_wide_mix, hex_4000_wide_mix, map_trzn_wide_mix)


# merge
  wide_map_counter$scenario <- 'counter'
  wide_map_mix$scenario <- 'mix'
  
  wide_map <- rbind(wide_map_counter,wide_map_mix)
  table(wide_map$scenario)
  rm(list=setdiff(ls(), c("wide_map")))
  
  gc(reset = T)

#### Change labels  --------------------------------

# Year Labels


# Grid Labels
wide_map$grid <-  if_else(wide_map$grid=='grid_0500', '0.5 Km',
                          if_else(wide_map$grid=='grid_1000', '1 Km',
                                  if_else(wide_map$grid=='grid_2000', '2 Km',
                                          if_else(wide_map$grid=='grid_4000', '4 Km',
                                                  if_else(wide_map$grid=='grid_trzn', 'Traffic zones', 'ERROR')))))





# scenario
wide_map$scenario <- factor(wide_map$scenario, levels=c('mix', 'counter'),
                          labels = c("Implemented", 'Counterfactual'))  




# incomde lables
wide_map$decile <- factor(wide_map$decile, levels=c(1:10),
                          labels = c("1 Poorest",2:9,"10 Richest"))  



# CHART


# base plot

baseplot <- theme_minimal() +
  theme( 
    #axis.text.y  = element_text(face="bold")
    #,axis.text.x  = element_text(face="bold")
    #,
    panel.grid.minor = element_blank()
    ,strip.text.x = element_text(size = 9) #, face ="bold"
    ,legend.text = element_text(size = 9)
    , axis.text = element_text(size=7)
    , axis.title = element_text(size=9)
  )







############### Chart oAccess Schools Scatter plot -------------------------------



#for (i in c(15,30,60,90,120)) {
  
  i=60
 
 # SCHOOLS
  # Average accessibility 
  mean(wide_map$prop_med_oaccess_schools_60_2014, na.rm=T) # 15.2%
  mean(wide_map$prop_med_oaccess_schools_60_2017, na.rm=T) # 12.8%
  
  # Plot
  tempvar_before <- noquote( paste0("prop_med_oaccess_schools_",i,"_2014") ) 
  tempvar_after <- noquote( paste0("prop_med_oaccess_schools_",i,"_2017") ) 
  
  tempplot <- 
   
     ggplot(data=subset(wide_map, pop > 0), aes(x=get(tempvar_before), y=get(tempvar_after))) +
      geom_point(aes(colour = factor(decile), size=pop/1000), alpha = 0.4) +
      scale_colour_brewer(palette = "RdBu") +
      labs(size="Population\nin thousands", colour="Income Decile") +
      #xlim(0, 60) + ylim(0, 60) + # axis limits
      geom_abline() +
      scale_x_continuous(name="2014 Med. Accessibiliy", labels=percent) +  # limits=c(0,.70),
      scale_y_continuous(name="2017 Med. Accessibiliy", labels=percent) + # limits=c(0,.70),
      
  #    facet_wrap( grid~scenario, ncol=2) +# vertical
      facet_grid( scenario~grid) +# horz
      
      
      baseplot +
      coord_fixed(ratio=1) +
      
      guides(color = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
      guides(size = guide_legend(nrow = 1,  label.position = "bottom")) +
      
      theme(
        # Legends
  #      legend.position=c(0.75, 0.14), # horz vert
        legend.position="bottom", # horz vert
        
        legend.direction='horizontal',
        legend.box='horizontal',
        legend.title=element_text(size=8),
        legend.text=element_text(size=7),
        legend.key.size = unit(.4, "cm"),
        legend.text.align=0.5,
        # axis
        axis.ticks=element_blank()
      )
  
  # vertical
  # ggsave(tempplot, file=paste0("./plots_orig3/charts/chart_diff_schools_scenarios_",i,".png") , dpi = 300,
  #          width = 16.5, height = 20.5, units="cm")
  
  
# horizontal
ggsave(tempplot, file=paste0("./plots_orig3/charts/chart_diff_schools_scenarios_",i,".png") , dpi = 300,
       width = 23, height = 13, units="cm")

  



############## JOBS 
# Average accessibility 
mean(wide_map$prop_med_oaccess_jobsmatch_60_2014, na.rm=T) # 14.8%
mean(wide_map$prop_med_oaccess_jobsmatch_60_2017, na.rm=T) # 13.4%

# Plot
tempvar_before <- noquote( paste0("prop_med_oaccess_jobsmatch_",i,"_2014") ) 
tempvar_after <- noquote( paste0("prop_med_oaccess_jobsmatch_",i,"_2017") ) 


tempplot <- 
  
  ggplot(data=subset(wide_map, pop > 0), aes(x=get(tempvar_before), y=get(tempvar_after))) +
  geom_point(aes(colour = factor(decile), size=pop/1000), alpha = 0.4) +
  scale_colour_brewer(palette = "RdBu") +
  labs(size="Population\nin thousands", colour="Income Decile") +
  #xlim(0, 60) + ylim(0, 60) + # axis limits
  geom_abline() +
  scale_x_continuous(name="2014 Med. Accessibiliy", labels=percent) +  # limits=c(0,.70),
  scale_y_continuous(name="2017 Med. Accessibiliy", labels=percent) + # limits=c(0,.70),
  
  #    facet_wrap( grid~scenario, ncol=2) +# vertical
  facet_grid( scenario~grid) +# horz
  
  
  baseplot +
  coord_fixed(ratio=1) +
  
  guides(color = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
  guides(size = guide_legend(nrow = 1,  label.position = "bottom")) +
  
  theme(
    # Legends
    #      legend.position=c(0.75, 0.14), # horz vert
    legend.position="bottom", # horz vert
    
    legend.direction='horizontal',
    legend.box='horizontal',
    legend.title=element_text(size=8),
    legend.text=element_text(size=7),
    legend.key.size = unit(.4, "cm"),
    legend.text.align=0.5,
    # axis
    axis.ticks=element_blank()
  )

# horizontal
ggsave(tempplot, file=paste0("./plots_orig3/charts/chart_diff_jobsmatch_scenarios_",i,".png") , dpi = 300,
       width = 23, height = 13, units="cm")






######################### How much did inequality change ??????


dt <- copy(wide_map) %>% setDT()

table(dt$scenario)
table(dt$grid)

# Ineq in 2014
(
dt[scenario=="Counterfactual" & grid=="0.5 Km" & decile >7, weighted.mean(x=prop_med_oaccess_jobs_60_2014, w=pop, na.rm=T)] /
dt[scenario=="Counterfactual" & grid=="0.5 Km" & decile <3, weighted.mean(x=prop_med_oaccess_jobs_60_2014, w=pop, na.rm=T)]
  -1)  * 100


# Ineq in 2017
(
dt[scenario=="Counterfactual" & grid=="0.5 Km" & decile >7, weighted.mean(x=prop_med_oaccess_jobs_60_2017, w=pop, na.rm=T)] / 
dt[scenario=="Counterfactual" & grid=="0.5 Km" & decile <3, weighted.mean(x=prop_med_oaccess_jobs_60_2017, w=pop, na.rm=T)]
  -1)  * 100



# thesis text:
# While in 2014, the level of access to jobs were 111% larger for the richest 20% than for the poorest 20%, this difference rose to 180% in 2017. 



# quanto cresceu cada uma
dt[decile >7, weighted.mean(x = diff_ratio_oaccess_jobsmatch_60, w=pop, na.rm=T)] /
dt[decile <3, weighted.mean(x = diff_ratio_oaccess_jobsmatch_60, w=pop, na.rm=T)]




