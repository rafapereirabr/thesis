

# Libraries
library(maps)
library(geosphere)
library(dplyr)
library(ggplot2)
library(rworldmap)
library(plyr)
library(data.table)
library(ggthemes)


# rio location
rio <- data.frame( lat= c( -22.910461), long = c( -43.163133))





# World map
  worldMap <- getMap(resolution = "low")
  world.points <- fortify(worldMap)
  world.points$region <- world.points$id
  world.df <- world.points[,c("long","lat","group", "region")]


# Brazil map
  brazil_map <- subset(worldMap, NAME == "Brazil")
  brazil_map_fort <- fortify(brazil_map)
  brazil_map_fort$region <- brazil_map_fort$id


world_map <- 
      
    ggplot() + 
        geom_polygon(data = world.df, aes(x = long, y = lat, group = group), fill="gray80", color="gray99") +
        geom_polygon(data = brazil_map_fort, aes(x = long, y = lat, group = group), fill="#306844") +
        geom_point(data=rio, aes(x=long, y=lat), color="#cc0000", size=4) +  # 10 para powrpoints
        scale_y_continuous(breaks = (-2:2) * 30) +
        scale_x_continuous(breaks = (-4:4) * 45) +
        coord_map("ortho", orientation=c(-24, 300, -1)) +
        theme_map()  +  
      theme(plot.background = element_rect( colour = "gray60") ,
            panel.grid = element_line(colour = "gray70", size=.2))

