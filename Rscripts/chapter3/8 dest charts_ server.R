
  
# set working Directory
  setwd("R:/Dropbox/Dout/Data Dout")



##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")







######## 0 read Access Data ----------------------

#################################################
# There are 4 datasets:                         # 
# # avg data of each grid cell                  #  MAPS  
#   stacked     - access_stacked_grid500_avg    #  output_catchment_size_500_long
#   not stacked - access_grid500_avg            #  output_gridDATA500hex_wide
#                                               #
# # composition of catchment area               #  CHARTS
#   stacked     - catchment_stacked500          #  output_catchment_composition_500_long
#   not stacked - catchment_500                 #  output_catchment_composition_500_wide
#################################################




# composition of catchment area -STACKED long  ---------------------
  catchment_stacked500_mix <- fread("./accessibility/output_catchment_composition_500_long.csv") # old 2011-2014/catchment_stacked500.csv")
  catchment_stacked500_counter <- fread("./accessibility/output_catchment_composition_500_long_counter.csv") # old 2011-2014/catchment_stacked500.csv")
  catchment_stacked500_mix$scenario <- "Implemented"
  catchment_stacked500_counter$scenario <- "Counterfactual"
  
  catchment_stacked500 <- rbind(catchment_stacked500_mix, catchment_stacked500_counter)
  names(catchment_stacked500)
  table(catchment_stacked500$scenario)


# composition of catchment area - NOT stacked ---------------------
  catchment_500_mix <- fread("./accessibility/output_catchment_composition_500_wide.csv") # old 2011-2014/catchment_500.csv")
  catchment_500_counter <- fread("./accessibility/output_catchment_composition_500_wide_counter.csv") # old 2011-2014/catchment_500_counter.csv")
  catchment_500_mix$scenario <- "Implemented"
  catchment_500_counter$scenario <- "Counterfactual"
  
  catchment_500 <- rbind(catchment_500_mix, catchment_500_counter)
  names(catchment_500)
  table(catchment_500$scenario)
  

# clean memory
  rm( list=setdiff(ls(), c('catchment_stacked500', 'catchment_500')))
  gc(reset = T)


# Labels, order of Implemented and Counterfactual
  catchment_stacked500$scenario <- factor(catchment_stacked500$scenario, levels=c("Implemented", "Counterfactual"), labels = c("Implemented", "Counterfactual"))
  catchment_500$scenario <- factor(catchment_500$scenario, levels=c("Implemented", "Counterfactual"), labels = c("Implemented", "Counterfactual"))
  



######## BASE PLOT ----------------------


baseplot2 <- theme_minimal() +
  theme( 
    axis.text.y  = element_text(face="bold")
    ,axis.text.x  = element_text(face="bold")
    ,panel.grid.minor = element_blank()
    ,strip.text = element_text(size = 11, face ="bold")
    ,legend.text = element_text(size = 11)
    )
  
  
# color palette http://colorbrewer2.org/
  getPalette <-  colorRampPalette(brewer.pal(9, "YlGnBu")) # YlGnBu YlOrBr

  getPalette <-  colorRampPalette(brewer.pal(9, "RdBu")) # YlGnBu YlOrBr
  

  
######### 1. SPORTS SPORTS SPORTS SPORTS SPORTS ---------------------------
######### 1. SPORTS SPORTS SPORTS SPORTS SPORTS ---------------------------
  
# Location of sports venues
# https://www.rio2016.com/locais-de-competicao
# https://pt.wikipedia.org/wiki/Locais_de_competi%C3%A7%C3%A3o_dos_Jogos_Ol%C3%ADmpicos_de_Ver%C3%A3o_de_2016
sports_venues <- c(  1327 # Olympic Park
                    ,1109 # Rio Centro --------------------------------------------- ideally 1099 ok
                    , 569 # Golf course                                   
                    ,3170 # Maracana
                    ,3797 # Olympic Stadium
                    #,antigo 4680 # Olympic shooting center  --------------------------------------------- 4679 \ 4680 ok
                    ,4733 # Deodoro Stadium    --------------------------------------------- ideally 4599 ok
                    ,3177 # Sambodromo - atlhetics 
                    ,2924 # Marina da Gloria
                    
                    #,1472 # Beach Volleyball - Copacabana
                    ,1149 # Lagoa Stadium
                    #, 216 # Pontal
                    )
  

  

  
######### 1.A SIZE of Catchment Area ----------------------------

  # Keep only sports venues
  df_sports <- catchment_stacked500[destination %in% sports_venues , ]
  table(df_sports$destination)

  
  # Sort Data by decile - this rearranges the plot
  df_sports <- df_sports[i=order(-decile, med_catchm_decile60, incomecomposit60),] # avg_daccess_pop
  # cru <- arrange(cru, desc(decile))
  
  # add labels to destinations 
  df_sports[, destination := factor(destination, levels=c("569", "1109", "1327", "1149", 
                                                          "2924", "4733", "3177", "3797", "3170"),
                              labels = c("Golf course", "Rio Centro", "Olympic\nPark", "Lagoa\nStadium",
                                         "Marina\nda Gloria","Deodoro\nStadium", "Sambodromo","Olympic\nStadium", "Maracana\nStadium"))]
  
  table(df_sports$destination)
  
# add labels to decile and year
  df_sports[, decile := factor(decile, levels=c(1:10), labels = c("1 Poorest",2:9,"10 Richest"))]
  df_sports$year <- if_else(df_sports$year=="201404", 2014, 2017)
  
  
# Pop que consegue chegar no maracana em 60 min
df_sports[destination=="Maracana\nStadium"][,.(sum(med_catchm_decile60)), by=.(year, scenario)]
 

     
# Plot 1A - Loop
#  facet_grid( year ~scenario) +                     # vertical

for (i in c(30,60,90)) {
    
          tempvar <- noquote( paste0("med_catchm_decile", i) )
            
          tempplot <- 
                    ggplot(df_sports ,aes(x = factor(year),  y = get(tempvar) , fill = factor(decile))) + 
                            geom_bar( stat = "identity", alpha=0.9) + 
                            scale_fill_manual(values = getPalette(10), guide_legend(title = "Income Decile")) +
                            scale_y_continuous(labels = comma) +
                            facet_grid( scenario ~ destination) +
                            baseplot2 +
                            labs(x=NULL, y="Size of Catachment Area")
          
          ggsave(tempplot, file=paste0("./plots_dest2/", "plot_1A_size_catch",i,"_both.png"), dpi = 300, width = 30, height = 20, units="cm")
          
          }
    

  
  
  


######### 1.B Composition of Catchment Area -------------------

# # Keep only sports venues
# df_sports <- catchment_stacked500[destination %in% sports_venues , ]
# 
# 
# # Sort Data by decile - this rearranges the plot
# df_sports <- df_sports[i=order(-decile,incomecomposit),] # avg_daccess_pop
# # cru <- arrange(cru, desc(decile))
# 
# # # add labels to destinations
# # df_sports[, destination := factor(destination, levels=c("216", "553", "1316", "1138", "1472", "2784", "3158", "3165", "3774"),
# #                                   labels = c("Pontal", "Golf course", "Olympic Park", "Lagoa Stadium", "Beach Volleyball at Copacabana",
# #                                              "Marina da Gloria","Maracana Stadium","Sambodromo","Olympic Stadium"))]
# 
# # add labels to decile
# df_sports[, decile :=    factor(decile, levels=c(1:10), labels = c("1 Poorest",2:9,"10 Richest"))]
# 

# plot_1B_compo_catch <- 
#   ggplot(df_sports ,aes(x = reorder(destination, incomecomposit, max, order=TRUE),  y = incomecomposit, fill = factor(decile))) + 
#     geom_bar( stat = "identity", alpha=0.9) + 
#     scale_fill_manual(values = getPalette(10), guide_legend(title = "Income Decile")) +
#     scale_y_continuous(labels = percent_format()) +
#     facet_grid(. ~ year) +
#     baseplot2 +
#     labs(x=NULL, y="Income Composition") +
#     theme(axis.text.x = element_text(angle = 40, hjust = 1))
#   
# ggsave(plot_1B_compo_catch, file="./plots_dest2/plot_1B_compo_catch.png" , dpi = 300,
#        width = 30, height = 20, units="cm")  
#   
#   


# # Plot 1B - Loop
#   
# for (i in c(30,60,90)) {
#   
#                   tempvar <- noquote( paste0("incomecomposit", i) )
#                   
#                   tempplot <- 
#                             ggplot(df_sports ,aes(x = factor(year),  y = get(tempvar), fill = factor(decile))) +
#                                       geom_bar( stat = "identity", alpha=0.9) + 
#                                       scale_fill_manual(values = getPalette(10), guide_legend(title = "Income Decile")) +
#                                       scale_y_continuous(labels = percent_format()) +
#                                       facet_grid( scenario ~ destination) +
#                                       baseplot2 +
#                                       labs(x=NULL, y="Income Composition") +
#                                       theme(axis.text.x = element_text(angle = 40, hjust = 1))
#                   
#                   ggsave(tempplot, file=paste0("./plots_dest2/", "plot_1B_compo_catch",i,".png"), dpi = 300, width = 30, height = 15.4, units="cm")
#                   
#                   }
#   
  



# plot_1B_compo_catch <- 
#   ggplot(df_sports ,aes(x = factor(year),  y = incomecomposit60, fill = factor(decile))) + 
#   geom_bar( stat = "identity", alpha=0.9) + 
#   scale_fill_manual(values = getPalette(10), guide_legend(title = "Income Decile")) +
#   scale_y_continuous(labels = percent_format()) +
#   facet_grid(. ~ destination) +
#   baseplot2 +
#   labs(x=NULL, y="Income Composition") +
#   theme(axis.text.x = element_text(angle = 40, hjust = 1))
# 
# ggsave(plot_1B_compo_catch, file="./plots_dest2/plot_1B_compo_catch2222222.png" , dpi = 300,
#        width = 35, height = 18, units="cm")  












  
######### 1.C variation in Composition of Catchment Area  ------------------
  
# Keep only sports venues
df_sports2 <- catchment_500[destination %in% sports_venues , ]

# add labels to destinations
df_sports2[, destination := factor(destination, levels=c("569", "1109", "1327", "1149", 
                                                        "2924", "4733", "3177", "3797", "3170"),
                                  labels = c("Golf course", "Rio Centro", "Olympic\nPark", "Lagoa\nStadium",
                                             "Marina\nda Gloria","Deodoro\nStadium", "Sambodromo","Olympic\nStadium", "Maracana\nStadium"))]


# add labels to decile
df_sports2[, decile :=    factor(decile, levels=c(1:10), labels = c("1 Poorest",2:9,"10 Richest"))]



  

# Plot 1C Composition - Loop RELATIVE

for (i in c(30,60,90)) {
  
  tempvar <- noquote( paste0("diff_incomecomposit", i,"_2017_2014") )
  

  tempplot <- 
              ggplot() + 
                  geom_bar(data = df_sports2, aes(x=factor(destination), y= get(tempvar), fill=factor(decile)),stat = "identity", alpha=0.9) +
                  scale_fill_manual(values = getPalette(10), guide_legend(title = "Income Decile")) +
                  baseplot2 +
                 # ylim(-10, 10) +
                  labs(x=NULL, y="Variation in Percentage Points") +
                  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
                  scale_x_discrete(position = "top") +
                  geom_hline(yintercept=0, color="red") +
                  facet_grid( scenario ~.) 
    
  
    
    
  ggsave(tempplot, file=paste0("./plots_dest2/", "plot_1C_compo_catch",i,"_both.png"), dpi = 300, width = 25, height = 20, units="cm")
  
  }




# Plot 1D Composition - Loop ABSOLUTE   ------------------

for (i in c(30,60,90)) {
  
  tempvar <- noquote( paste0("diff_catchm_decile", i,"_2017_2014") )
  
  
  tempplot <- 
              ggplot() + 
              geom_bar(data = df_sports2, aes(x=factor(destination), y= get(tempvar), fill=factor(decile)),stat = "identity", alpha=0.9) +
              scale_fill_manual(values = getPalette(10), guide_legend(title = "Income Decile")) +
              baseplot2 +
             # ylim(-10, 10) +
              scale_y_continuous(labels = comma) +
              labs(x=NULL, y="Variation in Total Population") +
              theme(axis.text.x = element_text(angle = 0, hjust = .5)) +
              scale_x_discrete(position = "top") +
              geom_hline(yintercept=0, color="red") +
              facet_grid( scenario ~.) 
  
  
  ggsave(tempplot, file=paste0("./plots_dest2/", "plot_1D_catchm_decile",i,"_both.png"), dpi = 300, width = 24, height = 20, units="cm")

  }













######### 2. HOSPITALS HOSPITALS HOSPITALS HOSPITALS HOSPITALS ---------------------------


#  get total population of Rio
  gridDATA500hex <- fread("./Spatial Grid/gridDaTA_0500.csv") # grid data
  riopop <- sum(gridDATA500hex$pop, na.rm = T)


# read output data of Hospitals
# long
  dest_access_grid500_avg_mix <- fread("./accessibility/output_catchment_Hospitals_long.csv")
  dest_access_grid500_avg_counter <- fread("./accessibility/output_catchment_Hospitals_long_counter.csv")
  dest_access_grid500_avg_mix$scenario <- "Implemented"
  dest_access_grid500_avg_counter$scenario <- "Counterfactual"
  
  dest_access_grid500_avg <- rbind(dest_access_grid500_avg_mix, dest_access_grid500_avg_counter)

# wide
  catchment_500_mix <- fread("./accessibility/output_catchment_Hospitals_wide.csv")
  catchment_500_counter <- fread("./accessibility/output_catchment_Hospitals_wide_counter.csv")
  catchment_500_mix$scenario <- "Implemented"
  catchment_500_counter$scenario <- "Counterfactual"
  
  catchment_500 <- rbind(catchment_500_mix, catchment_500_counter)


  
# clean memory
  rm( list=setdiff(ls(), c('riopop', 'dest_access_grid500_avg', 'catchment_500', 'baseplot2', 'getPalette')))
  gc(reset = T)


# Labels, order of Implemented and Counterfactual
  dest_access_grid500_avg$scenario <- factor(dest_access_grid500_avg$scenario, levels=c("Implemented", "Counterfactual"), labels = c("Implemented", "Counterfactual"))
  catchment_500$scenario <- factor(catchment_500$scenario, levels=c("Implemented", "Counterfactual"), labels = c("Implemented", "Counterfactual"))
  
# FACTOR HOSPITALS hierarq
  dest_access_grid500_avg[, hierarq := factor(hierarq, levels=c("Low","Medium","High"))] # Sort Data by decile - this rearranges the plot
  catchment_500[, hierarq := factor(hierarq, levels=c("Low","Medium","High"))] # Sort Data by decile - this rearranges the plot
  

# add labels to decile
  dest_access_grid500_avg[, decile := factor(decile, levels=c(1:10), labels = c("1 Poorest",2:9,"10 Richest"))]
  catchment_500[, decile := factor(decile, levels=c(1:10), labels = c("1 Poorest",2:9,"10 Richest"))]

# add labels to decile and year
  dest_access_grid500_avg$year <- if_else(dest_access_grid500_avg$year=="201404", "2014", "2017")
  table(dest_access_grid500_avg$year)
  
  

df_hosp <- copy(dest_access_grid500_avg)
df_hosp2 <- copy(catchment_500)




# Sort Data by decile - this rearranges the plot
df_hosp <- df_hosp[i=order(-decile, med_catchm_decile30, incomecomposit30),] # avg_daccess_pop
df_hosp2 <- df_hosp2[i=order(-decile, med_catchm_decile30_2017, incomecomposit30_2014),] # avg_daccess_pop




# Pop que consegue chegar nos hospitais em 30 min
catchment_500[,.((med_catchm_decile60_prop_2017)), by=.(decile, hierarq, scenario)]

df_hosp[,.(sum(med_catchm_decile30)), by=.(year,hierarq, scenario)]
df_hosp[,.(sum(med_catchm_decile30)/ riopop*100), by=.(year,hierarq, scenario)] 

 



# por decile
df_hosp[,.(med_catchm_decile30_prop*100), by=.(year,hierarq, decile, scenario)]

df_hosp$med_catchm_decile30_prop




# number of facilities in each complexity level
# High 51
# Low 158
# Low + High 2
# Low + Medium 31
# Low + Medium + High 33
# Medium 23
# Medium + High 6 
# 
# High = 51 + 2 + 33 + 6 (92)
# Medium = 31 + 33 + 23 + 6 (93)
# Low = 158 + 2 + 31 + 33 (224)

######### 2.A SIZE of Catchment Area ----------------------------

# Plot 2A - Loop

for (i in c(15, 30,60)) {
  
  tempvar <- noquote( paste0("med_catchm_decile", i) )
  
  tempplot <- 
    ggplot(df_hosp ,aes(x = factor(year),  y = get(tempvar), fill = factor(decile))) + #  /riopop
    geom_bar( stat = "identity", position = "stack", alpha=0.9) + 
    scale_fill_manual(values = getPalette(10), guide_legend(title = "Income Decile")) +
    scale_y_continuous(labels = comma) +
    facet_grid(scenario ~ hierarq) +
    baseplot2 +
    labs(x=NULL, y="Size of Catachment Area")  

  
  ggsave(tempplot, file=paste0("./plots_dest2/", "plot_2A_size_catch",i,"_both.png"), dpi = 300, width = 25, height = 20, units="cm")
}









# ######### 2.B Composition of Catchment Area -------------------
# 
# # Plot 2B - Loop
# 
# for (i in c(30,60)) {
#   
#   tempvar <- noquote( paste0("incomecomposit", i) )
#   
#   tempplot <- 
#     ggplot(df_hosp ,aes(x = factor(year),  y = get(tempvar), fill = factor(decile))) +
#     geom_bar( stat = "identity", alpha=0.9) + 
#     scale_fill_manual(values = getPalette(10), guide_legend(title = "Income Decile")) +
#     scale_y_continuous(labels = percent_format()) +
#     facet_grid(. ~ hierarq) +
#     baseplot2 +
#     labs(x=NULL, y="Income Composition") +
#     theme(axis.text.x = element_text(angle = 0, hjust = .5))
#   
#   ggsave(tempplot, file=paste0("./plots_dest2/", "plot_2B_compo_catch",i,".png"), dpi = 300,
#          width = 25, height = 15, units="cm")
# }





# ######### 2.C variation in Composition of Catchment Area  ------------------
# 
# Plot 2C Composition - Loop RELATIVE
summary(df_hosp2$diff_incomecomposit60_2017_2014)


for (i in c(15, 30,60)) {

  tempvar <- noquote( paste0("diff_incomecomposit", i,"_2017_2014") )

  df_hosp2 <- df_hosp2[i=order(-decile, -get(tempvar)),] # sort data

  tempplot <-
    ggplot() +
    geom_bar(data = subset(df_hosp2, scenario!="Iamplemented"), aes(x=factor(hierarq), y= get(tempvar), fill=factor(decile)),stat = "identity", alpha=0.9) +
    scale_fill_manual(values = getPalette(10), guide_legend(title = "Income Decile")) +
    baseplot2 +
    #ylim(-10, 10) +
    labs(x=NULL, y="Variation in Percentage Points") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    geom_hline(yintercept=0, color="red") +

  facet_grid(scenario ~ .) 
    
    
  ggsave(tempplot, file=paste0("./plots_dest2/", "plot_2C_compo_catch",i,".png"), dpi = 300,
         width = 20, height = 15, units="cm")
}




# Plot 2D Composition - Loop ABSOLUTE ---------------



df_hosp[ year=="2014" & scenario=="Implemented", .(loss=sum(med_catchm_decile30)), by=.(year, hierarq, scenario)]

df_hosp[ year=="2017" & scenario=="Implemented", .(loss=sum(med_catchm_decile30)), by=.(year, hierarq, scenario)]

df_hosp2[, .(loss=sum(diff_catchm_decile30_2017_2014)), by=.(hierarq, scenario)]

# Numeric results 
# Low
((sum(df_hosp$med_catchm_decile30[which(df_hosp$hierarq=="Low"& df_hosp$year=="2017" & df_hosp$scenario=="Implemented")]) / sum(df_hosp$med_catchm_decile30[which(df_hosp$hierarq=="Low"& df_hosp$year=="2014" & df_hosp$scenario=="Implemented")])) - 1) *100


# Medium
((sum(df_hosp$med_catchm_decile30[which(df_hosp$hierarq=="Medium"& df_hosp$year=="2017" & df_hosp$scenario=="Implemented")]) / sum(df_hosp$med_catchm_decile30[which(df_hosp$hierarq=="Medium"& df_hosp$year=="2014" & df_hosp$scenario=="Implemented")])) - 1) *100

# high
((sum(df_hosp$med_catchm_decile30[which(df_hosp$hierarq=="High"& df_hosp$year=="2017" & df_hosp$scenario=="Implemented")]) / sum(df_hosp$med_catchm_decile30[which(df_hosp$hierarq=="High"& df_hosp$year=="2014" & df_hosp$scenario=="Implemented")]))- 1) *100





for (i in c(15, 30,60)) {
  
  tempvar <- noquote( paste0("diff_catchm_decile", i,"_2017_2014") )
  
  df_hosp2 <- df_hosp2[i=order(-decile, -get(tempvar)),] # sort data
  
  tempplot <-
            ggplot() + 
            geom_bar(data = df_hosp2, aes(x=factor(hierarq), y= get(tempvar), fill=factor(decile)),stat = "identity", alpha=0.9) +
          #  geom_bar(data = catchment_500neg, aes(x=factor(hierarq), y= get(tempvar), fill=factor(decile)),stat = "identity", alpha=0.9) +
            scale_fill_manual(values = getPalette(10), guide_legend(title = "Income Decile")) +
            baseplot2 +
            # ylim(-10, 10) +
            scale_y_continuous(labels = comma) +
            labs(x=NULL, y="Variation in Total Population") +
            theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
            geom_hline(yintercept=0, color="red") +
            facet_grid(scenario ~ .)
    
  ggsave(tempplot, file=paste0("./plots_dest2/", "plot_2D_catchm_decile",i,"_both.png"), dpi = 300, width = 20, height = 15, units="cm")
}




######### 2.EE % of Pop with access to hospitals by income and hierarchy  ------------------

# % of the population by income decile with access to hospitals by hierarchy
summary(df_hosp$med_catchm_decile120_prop)


for (i in c(15,30,60,90,120)) {
  
  tempvar <- noquote( paste0("med_catchm_decile", i,"_prop") )
  
  tempplot <- 
    ggplot(df_hosp ,aes(x = factor(year) ,  y = get(tempvar), fill = factor(decile))) + 
          geom_bar( stat = "identity", alpha=0.9) + 
          scale_fill_manual(values = getPalette(10), guide_legend(title = "Income Decile")) +
          scale_y_continuous(labels = percent_format()) +
          facet_grid (hierarq ~ decile) +
          baseplot2 +
          labs(x=NULL, y="Proportion of the population with access to a facility ")
  
  ggsave(tempplot, file=paste0("./plots_dest2/", "plot_2E_Pro_pop_with_access_",i,".png"), dpi = 300,
         width = 28, height = 15, units="cm")
}


  
