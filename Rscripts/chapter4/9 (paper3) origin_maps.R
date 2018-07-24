

# set working Directory
  setwd("R:/Dropbox/Dout/Data Dout")
  
  
##################### Load packages -------------------------------------------------------
  
  source("./R scripts/00_LoadPackages.R")
  
  

  
  
######## ////// MAPS MAPS MAPS MAPS MAPS MAPS MAPS MAPS MAPS  ---------------------
  
  # Collor palettes
  # http://colorbrewer2.org
  # http://www.colorhexa.com/
  

  
##### A. Read oAccess data ----------
  
for (i in c('0500', '1000', '2000', '4000', 'trzn')){
  
        # Read accesibility data of each grid
        # oaccess_wide <- read_csv(paste0("./accessibility/output_oAccess_wide_",i,"_201404_2017counterfactual.csv"))
        # oaccess_long <- read_csv(paste0("./accessibility/output_oAccess_long_",i,"_201404_2017counterfactual.csv"))
        
        oaccess_wide <- read_csv(paste0("./accessibility/output_oAccess_wide_",i,"_201404_2017mix.csv"))
        oaccess_long <- read_csv(paste0("./accessibility/output_oAccess_long_",i,"_201404_2017mix.csv"))
        
        # add grid info
       oaccess_wide$grid <- NULL
        oaccess_long$grid <- NULL
        
        # assign corresponding names
        assign(paste0("oaccess_wide_",i), oaccess_wide)
        assign(paste0("oaccess_long_",i), oaccess_long)
        rm(oaccess_wide, oaccess_long)
        }
  

  
##### B. Read Spatial Grids ----------
  
# read maps
  hex_0500 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_0500')
  hex_1000 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_1000')
  hex_2000 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_2000')
  hex_4000 <- st_read(dsn = './Spatial Grid', layer ='hex_grid_4000')
  map_trzn <- st_read(dsn = './shapefiles_OD_rio_2012', layer ='map_grid_trzn')

# Merge data Long maps
  hex_0500_long <- left_join( hex_0500, oaccess_long_0500, by="ID")
  hex_1000_long <- left_join( hex_1000, oaccess_long_1000, by="ID")
  hex_2000_long <- left_join( hex_2000, oaccess_long_2000, by="ID")
  hex_4000_long <- left_join( hex_4000, oaccess_long_4000, by="ID")
  map_trzn_long <- left_join( map_trzn, oaccess_long_trzn, by="ID")
  
  # Rbind maps
  long_map <- rbind(hex_0500_long, hex_1000_long, hex_2000_long, hex_4000_long, map_trzn_long)
  
  
# Merge data WIDE maps
  hex_0500_wide <- left_join( hex_0500, oaccess_wide_0500, by="ID")
  hex_1000_wide <- left_join( hex_1000, oaccess_wide_1000, by="ID")
  hex_2000_wide <- left_join( hex_2000, oaccess_wide_2000, by="ID")
  hex_4000_wide <- left_join( hex_4000, oaccess_wide_4000, by="ID")
  map_trzn_wide <- left_join( map_trzn, oaccess_wide_trzn, by="ID")
  
  # Rbind maps
  wide_map <- rbind(hex_0500_wide, hex_1000_wide, hex_2000_wide, hex_4000_wide, map_trzn_wide)
  
  

# keep only long and wide maps
  rm(list=setdiff(ls(), c("wide_map", "long_map")))
  gc(reset = T)
  
  
  
# test
  # plot long
    ggplot() + geom_sf(data= subset(long_map, !is.na(med_oaccess_edubas_60)), aes(fill=med_oaccess_edubas_60), color=NA) + facet_wrap(~grid)
    
  # plot wide  
    ggplot() + geom_sf(data= subset(wide_map, !is.na(diff_minus_oaccess_jobsmatch_60)), aes(fill=diff_minus_oaccess_jobsmatch_60), color=NA) + facet_wrap(~grid)
    



    
    
    
###### Explore results #--------------------------------------------- 
    
df500 <- copy(wide_map) %>% setDT()
df500 <- df500[grid=="grid_0500",] 
df500 <- df500[pop >0 ]
df500 <- subset(df500, diff_ratio_oaccess_schools_60 != Inf)
df500 <- subset(df500, diff_ratio_oaccess_schools_60 != -Inf)


df4000 <- copy(wide_map) %>% setDT()
df4000 <- df4000[grid=="grid_4000",] 





# mudanca media de access to jobs in 60 min # AVERAGE RESULTS #  #--------------------------------------------- 
(weighted.mean(df500$diff_ratio_oaccess_jobsmatch_60, df500$pop, na.rm=T)-1 )* 100
(weighted.mean(df500$diff_ratio_oaccess_schools_60, df500$pop, na.rm=T)-1 )* 100




### Affordability table -----------------

# by decile
df500[, .(income = weighted.mean(x=RDPC, w=pop)), by=decile][order(decile)]

# total
weighted.mean(x=df500$RDPC, w=df500$pop)

# average change in aCcess in percentage points
  #weighted.mean(df500$diff_minus_oaccess_jobsmatch_60, df500$pop, na.rm=T) 
  #weighted.mean(df500$diff_minus_oaccess_schools_60, df500$pop, na.rm=T)   

  ( weighted.mean(df500$diff_ratio_oaccess_jobsmatch_60, w = df500$pop, na.rm=T) -1 )*100 
  ( weighted.mean(df500$diff_ratio_oaccess_schools_60, w = df500$pop, na.rm=T) -1 )*100     
  

# get total population
totalpop <- sum(df500$pop, na.rm=T)

# check distribution
summary(df500$diff_minus_oaccess_jobsmatch_60)
density(df500$diff_minus_oaccess_jobsmatch_60, na.rm=T) %>% plot(col="green")

#summary(df500$diff_minus_oaccess_schools_60)
summary(df500$diff_minus_oaccess_schools_60)
density(df500$diff_minus_oaccess_schools_60, na.rm=T) %>% lines(col="red")
abline(v=0, col="black")


# What propotion of the pop got any improvement 
  df500[ diff_minus_oaccess_jobsmatch_60 >5 , sum(pop, na.rm=T)] / totalpop *100
  df500[ diff_minus_oaccess_schools_60 >5 , sum(pop, na.rm=T)] / totalpop *100

# What propotion of the pop got any loss 
  df500[ diff_minus_oaccess_jobsmatch_60 < -5 , sum(pop, na.rm=T)] / totalpop *100
  df500[ diff_minus_oaccess_schools_60 < -5 , sum(pop, na.rm=T)] / totalpop *100

  
  df4000[ diff_minus_oaccess_jobsmatch_60 < -5 , sum(pop, na.rm=T)] / totalpop *100
  df4000[ diff_minus_oaccess_schools_60 < -5 , sum(pop, na.rm=T)] / totalpop *100
  
  
# What propotion of the pop got same same
  df500[ findInterval(diff_minus_oaccess_jobsmatch_60, c(-5,5)) , sum(pop, na.rm=T)] / totalpop *100
  df500[ findInterval(diff_minus_oaccess_schools_60, c(-5,5)) , sum(pop, na.rm=T)] / totalpop *100
    
    
  df500[ findInterval(diff_ratio_oaccess_jobsmatch_60,c(.7,1.3)) , sum(pop, na.rm=T)] / totalpop *100
  df500[ findInterval(diff_ratio_oaccess_schools_60,  c(.7,1.3)) , sum(pop, na.rm=T)] / totalpop *100
  

summary(df500$diff_ratio_oaccess_jobsmatch_60)
sd(df500$diff_ratio_oaccess_jobsmatch_60, na.rm=T)

# Access
  density(df500$diff_minus_oaccess_schools_60, na.rm=T) %>% plot(col="gray")
  density(df500$diff_minus_oaccess_jobsmatch_60[which(df500$decile > 8)], na.rm=T) %>% lines(col="blue")
  density(df500$diff_minus_oaccess_jobsmatch_60[which(df500$decile < 3)], na.rm=T) %>% lines(col="red")
  abline(v=0, col="black")
  
  
  
###### box plot #---------------------------
# Schools 500
  ggplot(df500, aes(x=factor(decile), y=diff_ratio_oaccess_schools_60, weight=pop, color=factor(decile))) + # aes(weight=pop)
    geom_boxplot( outlier.colour=rgb(.5,.5,.5, alpha=0.05)) + 
    scale_colour_brewer(palette = "RdBu") +
    scale_y_continuous(name="Avg. Accessibility (%)") + 
    scale_x_discrete(name="Income Decile") +
    geom_hline(yintercept = 1)  
  
  # jobs 500
  ggplot(df500, aes(x=factor(decile), y=diff_ratio_oaccess_jobsmatch_60, weight=pop, color=factor(decile))) + # aes(weight=pop)
    geom_boxplot( outlier.colour=rgb(.5,.5,.5, alpha=0.05)) + 
    scale_colour_brewer(palette = "RdBu") +
    scale_y_continuous(name="Avg. Accessibility (%)") + 
    scale_x_discrete(name="Income Decile") +
    geom_hline(yintercept = 1) 
  
  ggplot(df500, aes(x=factor(decile), y=med_oaccess_edubas_60_2017, weight=pop, color=factor(decile))) + # aes(weight=pop)
    geom_boxplot( outlier.colour=rgb(.5,.5,.5, alpha=0.05)) + 
    scale_colour_brewer(palette = "RdBu") +
    scale_y_continuous(name="Avg. Accessibility (%)") + 
    scale_x_discrete(name="Income Decile") +
    geom_hline(yintercept = mean(df500$med_oaccess_edubas_60_2017)) 
  
  
# jobs all scales  
  ggplot(wide_map, aes(x=factor(decile), y=diff_minus_oaccess_jobsmatch_60, weight=pop,color=factor(decile))) + # aes(weight=pop)
    geom_boxplot( outlier.colour=rgb(.5,.5,.5, alpha=0.05)) + 
    scale_colour_brewer(palette = "RdBu") +
    scale_y_continuous(name="Variation in median accessibility (%..)") + 
    scale_x_discrete(name="Income Decile") +
 #   baseplot +
    geom_hline(yintercept = 0) +
    facet_wrap(~grid, ncol = 2) +
    scale_y_continuous(limits=c(-10,10))
    
  
  ggplot(wide_map, aes(x=factor(decile), y=diff_ratio_oaccess_jobsmatch_60, weight=pop,color=factor(decile))) + # aes(weight=pop)
    geom_boxplot( outlier.colour=rgb(.5,.5,.5, alpha=0.05)) + 
    scale_colour_brewer(palette = "RdBu") +
    scale_y_continuous(name="Variation in median accessibility (%..)") + 
    scale_x_discrete(name="Income Decile") +
    #   baseplot +
    geom_hline(yintercept = 1) +
    facet_wrap(~grid, ncol = 2) +
    scale_y_continuous(limits=c(0,2))
  
  
  
  
# boxplot by different travel times  
  
  baseplot2 <- theme_minimal() +
    theme( 
      axis.text.y  = element_text(face="bold")
      ,axis.text.x  = element_text(face="bold")
      ,panel.grid.minor = element_blank()
      ,strip.text = element_text(size = 11, face ="bold")
      ,legend.text = element_text(size = 11)
    )
  
  
  # subset columns
  setDT(df500)
  cols_to_keep <- names(df500)[(names(df500) %like% "diff_ratio_oaccess_jobsmatch_.*")]
  df500_melted <- df500[, c("ID", "pop", "decile", cols_to_keep), with=F]
  
  # mudanca media de access to jobs in 60 min # AVERAGE RESULTS
  (weighted.mean(df500_melted$diff_ratio_oaccess_jobsmatch_60, df500_melted$pop, na.rm=T)-1 )* 100
  
  # melt data
  df500_melted <- data.table::melt(df500_melted, id=1:3)
  
  # rescale values to percent
  df500_melted[, value := value -1 ]
  
  # change names/labels
  df500_melted[, variable := stringr::str_sub( variable ,-3,-1) ]
  df500_melted[, variable :=   as.numeric(sub("_", "", variable)) ]
  df500_melted <- subset(df500_melted, variable != 15)
  df500_melted[, variable :=   paste0(variable, " min.")]
  
  df500_melted$decile <- factor(df500_melted$decile, levels=c(1:10),
                                labels = c("1\nPoorest",2:9,"10\nRichest"))  
  
  # order of facets
  df500_melted$variable <- factor(df500_melted$variable, levels = c('30 min.',  '60 min.',  '90 min.',  '120 min.'))
  
  
  
  tempplot <- 
    
    ggplot(df500_melted, aes(x=factor(decile), y=value, weight=pop, color=factor(decile))) + # aes(weight=pop)
    geom_boxplot( outlier.colour=rgb(.5,.5,.5, alpha=0.05)) + 
    scale_colour_brewer(palette = "RdBu" ) +
    scale_y_continuous(name="Change in Accessibility", limits = c(-.4, .4), labels = percent_format()) + 
    scale_x_discrete(name="Income Decile") +
    geom_hline(yintercept = 1) +
    facet_wrap(~variable) +
    geom_hline(yintercept= 0) +
    baseplot2
  
  ggsave(tempplot, file=paste0("./plots_orig3/charts/boxplot_jobsmatch_counter.png"), dpi = 300,
         width = 25, height = 12.5, units = "cm") 
  
  
  ggsave(tempplot, file=paste0("./plots_orig3/charts/boxplot_jobsmatch_mix.png"), dpi = 300,
         width = 25, height = 12.5, units = "cm") 
  
  
  
  
  
##### C. Create Base Map --------------------------------
    
  source("./R scripts/00_BaseMap_sf.R")
  
  # test
  basemap + infra_antes + infra_atual + infra_depois + venues + theme_opts
  basemap + theme_opts
  
  
  
  
#### Change labels  --------------------------------
  
  # Year Labels

  long_map$year <- if_else(long_map$year=="201404", 2014, 2017)
  table(long_map$year)
  
# Grid Labels
  long_map$grid <-  if_else(long_map$grid=='grid_0500', '0.5 Km',
                    if_else(long_map$grid=='grid_1000', '1 Km',
                    if_else(long_map$grid=='grid_2000', '2 Km',
                    if_else(long_map$grid=='grid_4000', '4 Km',
                    if_else(long_map$grid=='grid_trzn', 'Traffic zones', 'ERROR')))))
  
  
  wide_map$grid <-  if_else(wide_map$grid=='grid_0500', '0.5 Km',
                      if_else(wide_map$grid=='grid_1000', '1 Km',
                      if_else(wide_map$grid=='grid_2000', '2 Km',
                      if_else(wide_map$grid=='grid_4000', '4 Km',
                      if_else(wide_map$grid=='grid_trzn', 'Traffic zones', 'ERROR')))))
  
  
# incomde lables
  wide_map$decile <- factor(wide_map$decile, levels=c(1:10),
                            labels = c("1 Poorest",2:9,"10 Richest"))  

  long_map$decile <- factor(long_map$decile, levels=c(1:10),
                            labels = c("1 Poorest",2:9,"10 Richest"))  
  
  
  
  
  

#### Map 1 Population  --------------------------------

# all scales
tempplot <- 
           basemap +
            geom_sf(data= subset(wide_map, pop > 0), aes(fill=pop/1000/area), color=NA) +
            scale_fill_distiller( palette="Oranges", guide = "colorbar", direction = 1, name= bquote('Residents, thousands per'~Km^2) ) +
            facet_wrap(~grid, ncol = 2) +
            theme_opts +
  
            theme(
              # Legends
                legend.position=c(0.65, 0.07), # horz vert
                legend.direction='horizontal',
                legend.box='horizontal',
                legend.title=element_text(size=8),
                legend.text=element_text(size=8),
              # axis
                axis.title=element_blank(),
                axis.text=element_blank(),
                axis.ticks=element_blank()) 
    
  
ggsave(tempplot, file=paste0("./plots_orig3/map1_pop.png"), dpi = 300,
       width = 25, height = 12.5, units = "cm") 


# 500 scale
  tempplot <- 
    basemap +
    geom_sf(data= subset(wide_map, pop > 0 & grid=="0.5 Km"), aes(fill=pop/1000/area), color=NA) +
    scale_fill_distiller( palette="Oranges", guide = "colorbar", direction = 1, name= bquote('Residents, thousands per'~Km^2) ) +
    infra_antes + infra_atual + infra_depois + venues +
    theme_opts +
    theme(
      # Legends
      legend.position=c(0.01, 0.02), # horz vert
      legend.direction='horizontal',
      legend.box='horizontal',
      legend.title=element_text(size=7),
      legend.text=element_text(size=7),
      
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
      )



ggsave(tempplot, file=paste0("./plots_orig3/map1_pop_grid_0500_axis.png"), dpi = 300,
       width = 16.5, height = 9, units = "cm") 







#### FAZER - Map 4.  INCOME  --------------------------------


tempplot <- 
  basemap +
  geom_sf(data= subset(wide_map, pop > 0), aes(fill=factor(decile)), color=NA, alpha=.9) +
  scale_fill_brewer( palette="RdBu", guide = "legend",  name="Income decile") +
  
  #scale_fill_distiller( palette="Oranges", guide = "colorbar", direction = 1, name="Population density") +
  facet_wrap(~grid, ncol = 2) +
  theme_opts +
  guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
  theme(
    # Legends
    legend.position=c(0.52, 0.07), # horz vert
    legend.direction='horizontal',
    legend.box='horizontal',
    legend.title=element_text(size=9),
    legend.text=element_text(size=8),
    legend.key.size = unit(.5, "cm"),
    legend.text.align=0.5,
    # axis
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())



ggsave(tempplot, file=paste0("./plots_orig3/map_income.png"), dpi = 300,
       width = 16.5, height = 14, units = "cm") 









############### Map 4. oAccess jobsmatch 2014 vs 2017 -------------------------------


# All years and grids in one single plot


# ### Gini inequality of accessibility, ponderado pela pop
#     
#     temp <- oaccess_wide[, .(pop, prop_med_oaccess_jobs_60_2014, prop_med_oaccess_jobs_60_2017)]
#     temp <- na.omit(temp)
#     
#     reldist::gini(x = temp$prop_med_oaccess_jobs_60_2014, weights = temp$pop )
#     reldist::gini(x = temp$prop_med_oaccess_jobs_60_2017, weights = temp$pop )



for (i in c(15,30,60,90,120)) {
  
  
  # JOBS match
  tempvar <- noquote( paste0("prop_med_oaccess_jobsmatch_", i) )
  temp_df <- subset(long_map, year==2017)
  
  
tempplot <- 
  basemap +
  geom_sf(data= subset(temp_df, pop > 0), aes(fill=get(tempvar)), alpha=.85, color=NA) +
  scale_fill_viridis( option="A", name="Proportion of jobs accessible", labels = percent_format())+#, limits=c(0, .6), breaks= seq(0,.6,.1) ) +
  facet_wrap(~grid, ncol = 2) +
  infra_antes + infra_atual +
  theme_opts +
#  theme(legend.position="top") +
  
        theme(
          # Legends
          legend.position=c(0.55, 0.07), # horz vert
          legend.direction='horizontal',
          legend.box='horizontal',
          legend.title=element_text(size=8),
          legend.text=element_text(size=8),
          # axis
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())

ggsave(tempplot, file=paste0("./plots_orig3/map4_oAcess_jobsmatch_", i,".png"), dpi = 300,
       width = 16, height = 14, units = "cm")


## schools
  tempvar <- noquote( paste0("prop_med_oaccess_schools_", i) )

  
tempplot <- 
  basemap +
  geom_sf(data= subset(temp_df, pop > 0), aes(fill=get(tempvar)), alpha=.85, color=NA) +
  scale_fill_viridis( option="A", name="Propotion of public schools accessible", labels = percent_format())+#, limits=c(0, .6), breaks= seq(0,.6,.1) ) +
  facet_wrap(~grid, ncol = 2) +
  infra_antes + infra_atual +
  theme_opts +
  #  theme(legend.position="top") +
  
  theme(
    # Legends
    legend.position=c(0.55, 0.07), # horz vert
    legend.direction='horizontal',
    legend.box='horizontal',
    legend.title=element_text(size=8),
    legend.text=element_text(size=8),
    # axis
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())

ggsave(tempplot, file=paste0("./plots_orig3/map4_oAcess_schools_", i,".png"), dpi = 300,
       width = 16, height = 14, units = "cm")

}






# CHART
############### Chart oAccess Schools Scatter plot -------------------------------



for (i in c(15,30,60,90,120)) {
  
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
    facet_wrap( ~grid, ncol=2) +# vertical
    baseplot +
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
      axis.ticks=element_blank()
    )
  
  ggsave(tempplot, file=paste0("./plots_orig3/charts/chart_diff_schools_counter_",i,".png") , dpi = 300,
         width = 16.5, height = 20.5, units="cm")
  
  
# JOBS
  # Average accessibility 
  mean(wide_map$prop_med_oaccess_jobsmatch_60_2014, na.rm=T) # 14.8%
  mean(wide_map$prop_med_oaccess_jobsmatch_60_2017, na.rm=T) # 13.4%
  
  # plot
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
    facet_wrap( ~grid, ncol=2) +# vertical
    baseplot +
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
      axis.ticks=element_blank()
    )
  
  ggsave(tempplot, file=paste0("./plots_orig3/charts/chart_diff_jobsmatch_counter_",i,".png") , dpi = 300,
         width = 16.5, height = 20.5, units="cm")
}








############### Map 6. Jobsmatch Variation --------------------------------

# # color scheme 
  # Uber (divergent)
  uber_scale_diver <- c(scale_fill_gradient2(low = "#108188", mid = "#eee7e5", high = "#c22e00", midpoint = 0, guide = "colourbar"))
  uber_scale_diver <- c(scale_fill_gradient2(low = "#c22e00", mid = "#eee7e5", high = "#108188", midpoint = 0, guide = "colourbar"))
# Rainbow color Palette
    myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")

    summary(wide_map$diff_minus_oaccess_jobsmatch_30)
    summary(wide_map$diff_minus_oaccess_jobsmatch_60)
    summary(wide_map$diff_ratio_oaccess_jobsmatch_60)
    


for (i in c(30,60)) {
    

  tempvar <- noquote( paste0("diff_minus_oaccess_jobsmatch_",i) ) 
  
  scaleval <- wide_map[, paste(tempvar)][[1]] %>% abs() %>% max(na.rm=T) # get max e min scale value
  
  summary(wide_map$diff_ratio_oaccess_jobsmatch_60)
  density(wide_map$diff_ratio_oaccess_jobsmatch_60, na.rm=T) %>% plot()
  
  tempplot <- 
    basemap +
    geom_sf(data= subset(wide_map, pop > 0 ), aes(fill= get(tempvar)), color=NA) +
    #scale_fill_viridis( name="Accessibility variation\nin percentage points", option="inferno")+#, limits=c(0, .6), breaks= seq(0,.6,.1) ) +
    scale_fill_distiller( limits=c(-scaleval, scaleval), palette="RdBu",  direction = 1, name="Accessibility variation\nin percentage points") + 
    #scale_fill_gradientn(limits=c(-scaleval, scaleval), colours = rev(myPalette(100)), name="Accessibility variation\nin percentage points" ) + # , limits=c(-27, 27)) + #  
    #uber_scale_diver +
    facet_wrap(~grid, ncol = 2) +

    infra_antes + infra_atual +
    theme_opts +
    theme(
      # Legends
      legend.position=c(0.65, 0.07), # horz vert
      legend.direction='horizontal',
      legend.box='horizontal',
      legend.title=element_text(size=8),
      legend.text=element_text(size=8),
      # axis
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank()) 
  
  ggsave(tempplot, file=paste0("./plots_orig3/diff_jobsmatch_minus_",i,"RdBu_counter.png"), dpi = 300,
         width = 16.5, height = 15, units = "cm") 
  
  
##### RATIO map
  
  
  
  
  tempvar <- noquote( paste0("diff_ratio_oaccess_jobsmatch_",i) )
  
# convert scale 
  wide_map$test <- (wide_map[, paste(tempvar)][[1]] - 1)
  wide_map$test <- ifelse(wide_map$test >1, 1, wide_map$test)
  wide_map$test <- ifelse(wide_map$test < -1, -1, wide_map$test)
  summary(wide_map$test)
  
  
tempplot <- 
  basemap +
  geom_sf(data= subset(wide_map, !is.na(test) ), aes(fill= test, label=ID), alpha=.85, color=NA) +
  scale_fill_distiller(  limits=c(-1, 1),labels=percent, palette="RdBu",  direction = 1, name="Accessibility variation") + 
  # scale_fill_gradientn(limits=c(-100, 100), colours = rev(myPalette(100)), name="Accessibility variation\nin percentage points" ) + # , limits=c(-27, 27)) + #  
  # scale_fill_gradient2( low = "red3", mid = "white", high = "navyblue", midpoint = 1, space = "Lab", na.value = "grey50", guide = "colourbar") +
  # scale_fill_gradient2( low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 1, space = "Lab", na.value = "grey50", guide = "colourbar") +
  theme_map() +
  facet_wrap(~grid, ncol = 2) +
  
  infra_antes + infra_atual +
  theme_opts +
  theme(
    # Legends
    legend.position=c(0.65, 0.07), # horz vert
    legend.direction='horizontal',
    legend.box='horizontal',
    legend.title=element_text(size=8),
    legend.text=element_text(size=8),
    # axis
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank()) 
  
  ggsave(tempplot, file=paste0("./plots_orig3/diff_jobsmatch_ratio_",i,"RdBu_counter.png"), dpi = 300,
         width = 16.5, height = 15, units = "cm") 
  
}
beep()




############### Map 7. Schools Variation --------------------------------

summary(wide_map$diff_minus_oaccess_schools_60)
summary(wide_map$diff_ratio_oaccess_schools_60)




for (i in c(30,60)) {
  
  
  
  tempvar <- noquote( paste0("diff_minus_oaccess_schools_",i) )
  
  scaleval <- wide_map[, paste(tempvar)][[1]] %>% abs() %>% max(na.rm=T) # get max e min scale value
  
  summary(wide_map$diff_ratio_oaccess_schools_60)
  density(wide_map$diff_ratio_oaccess_schools_60, na.rm=T) %>% plot()
  
  tempplot <- 
    basemap +
    geom_sf(data= subset(wide_map, pop > 0 ), aes(fill= get(tempvar)), color=NA) +
    scale_fill_distiller( limits=c(-scaleval, scaleval), palette="RdBu",  direction = 1, name="Accessibility variation\nin percentage points") + 
    facet_wrap(~grid, ncol = 2) +
    
    infra_antes + infra_atual +
    theme_opts +
    theme(
      # Legends
      legend.position=c(0.65, 0.07), # horz vert
      legend.direction='horizontal',
      legend.box='horizontal',
      legend.title=element_text(size=8),
      legend.text=element_text(size=8),
      # axis
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank()) 
  
  ggsave(tempplot, file=paste0("./plots_orig3/diff_schools_minus_",i,"RdBu_counter.png"), dpi = 300,
         width = 16.5, height = 15, units = "cm") 
  
  
  ##### RATIO map
  
  
  
  
  tempvar <- noquote( paste0("diff_ratio_oaccess_schools_",i) )
  
  # convert scale 
  wide_map$test <- (wide_map[, paste(tempvar)][[1]] - 1)
  wide_map$test <- ifelse(wide_map$test >1, 1, wide_map$test)
  wide_map$test <- ifelse(wide_map$test < -1, -1, wide_map$test)
  summary(wide_map$test)
  
  
  tempplot <- 
    basemap +
    geom_sf(data= subset(wide_map, !is.na(test) ), aes(fill= test, label=ID), alpha=.85, color=NA) +
    scale_fill_distiller(  limits=c(-1, 1),labels=percent, palette="RdBu",  direction = 1, name="Accessibility variation") + 
    theme_map() +
    facet_wrap(~grid, ncol = 2) +
    
    infra_antes + infra_atual +
    theme_opts +
    theme(
      # Legends
      legend.position=c(0.65, 0.07), # horz vert
      legend.direction='horizontal',
      legend.box='horizontal',
      legend.title=element_text(size=8),
      legend.text=element_text(size=8),
      # axis
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank()) 
  
  ggsave(tempplot, file=paste0("./plots_orig3/diff_schools_ratio_",i,"RdBu_counter.png"), dpi = 300,
         width = 16.5, height = 15, units = "cm") 
  
}
beep()








######### Using ggmap ----------------------------------------

so_ggmap <- base_ggmap
passado <-   base_ggmap + infra_antes
presente <- base_ggmap + infra_antes + infra_atual + venues
futuro <-   base_ggmap + infra_antes + infra_atual + infra_depois + venues

ggsave(so_ggmap, file=paste0("./plots_dest/", "a_so_ggmap.png"), dpi = 300, width = 19, height = 10, units = "cm") 
ggsave(passado , file=paste0("./plots_dest/", "a_passado.png"), dpi = 300, width = 19, height = 10, units = "cm") 
ggsave(presente, file=paste0("./plots_dest/", "a_presente.png"), dpi = 300, width = 19, height = 10, units = "cm") 
ggsave(futuro  , file=paste0("./plots_dest/", "a_futuro.png"), dpi = 300, width = 19, height = 10, units = "cm") 


ggmap(google_map) + 
  geom_polygon(data= subset(hexriopoly500_df_stacked, !is.na(avg_propaccessJobsmatch)), 
               aes(long, lat, group = group, fill = avg_propaccessJobsmatch), alpha=0.8) +
  scale_fill_gradient( low = "#fef0d9",   high = "#bd0026", na.value = "white",
                       guide = guide_legend(title = "Population")) + # pop RED
  facet_grid(. ~ year) +
  theme_opts +
  theme(legend.position="bottom")


ggmap(google_map) +
  geom_polygon(data= subset(hexriopoly500_df_stacked, !is.na(med_catchm_pop90)), 
               aes(long, lat, group = group, fill = med_catchm_pop90), alpha=0.8) +
  scale_fill_distiller( palette="OrRd", guide = "colorbar", direction = 1, name="Avg. Accessibility (%)") +
  facet_grid(year~.) +
  theme(legend.justification = c(0, 0), legend.position = c(0, 0)) + 
  theme_opts


###### Infra

a <-  ggmap(google_map) +# infra_antes + infra_atual + infra_depois +
  scale_x_continuous(limits = c(-43.79653853, -43.15022410), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-23.07665345 , -22.77554842), expand = c(0, 0)) 

ggmap(google_map) + theme_opts
geom_polygon(data= subset(hexriopoly500_df, pop > 0 ) , aes(long, lat, group = group, fill = pop/1000 )) +
  scale_fill_distiller( palette="Oranges", guide = "colorbar", direction = 1, name="Population in thousands") +
  scale_x_continuous(limits = c(-43.79653853, -43.15022410), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-23.07665345 , -22.77554842), expand = c(0, 0)) 



ggsave(a, file=paste0("./plots_dest/", "mapxx_population_future_ggmap.png"), dpi = 300,
       width = 19, height = 10, units = "cm") 












###### scatter plot soh de 500 metros ---------------------------



temp_df <- subset(wide_map, grid == "grid_0500")

# Plot
i=60
tempvar_before <- noquote( paste0("prop_med_oaccess_jobsmatch_",i,"_2014") ) 
tempvar_after <- noquote( paste0("prop_med_oaccess_jobsmatch_",i,"_2017") ) 


tempplot <- 
  ggplot(data=subset(temp_df, pop > 0), aes(x=get(tempvar_before), y=get(tempvar_after))) +
  geom_point(aes(colour = factor(decile), size=pop/1000), alpha = 0.4) +
  scale_colour_brewer(palette = "RdBu") +
  labs(size="Population\nin thousands", colour="Income Decile") +
  #xlim(0, 60) + ylim(0, 60) + # axis limits
  geom_abline() +
  scale_x_continuous(name="2014 Med. Accessibiliy", labels=percent) +  # limits=c(0,.70),
  scale_y_continuous(name="2017 Med. Accessibiliy", labels=percent) + # limits=c(0,.70),
  #  facet_wrap( ~grid, ncol=2) +# vertical
  baseplot +
  coord_fixed(ratio=0.9) +
  
  guides(color = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
  guides(size = guide_legend(nrow = 1,  label.position = "bottom")) +
  
  theme(legend.position="none", axis.ticks=element_blank()) +
# theme(
#   # Legends
#   legend.position=c(0.75, 0.2), # horz vert
#   legend.direction='horizontal',
#   legend.box='vertical',
#   legend.title=element_text(size=8),
#   legend.text=element_text(size=7),
#   legend.key.size = unit(.4, "cm"),
#   legend.text.align=0.5,
#   # axis
#   axis.ticks=element_blank()
# ) +
ggtitle("Implemented")

ggsave(tempplot, file=paste0("./plots_orig3/charts/chart_diff_jobsmatch_60_mix11.png") , dpi = 300,
       width = 11, height = 11, units="cm")

