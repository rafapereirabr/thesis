
# options
  options(digits=10)   # number of digits to show
  options(scipen=999) # disable scientific notation


# install packages
list.of.packages <- c("sp", "rgeos", "ggplot2", "data.table", "rgdal", "dplyr", 
                      "magrittr", "spatialEco", "geosphere", "readr", "read.dbc", "sf", # "emoGG",
                      "foreign", "ggmap", "maptools", "scales", "fasttime", "grid", "broom",
                      "plyr", "bit64", "survey", "parallel", "doSNOW", "RColorBrewer", "ggsn",
                      "ineq", "pbapply", "beepr", "gridExtra", "cowplot", "ggthemes", "ggspatial",
                      "readxl", "xlsx", "plotly", "spdep", "reshape2", "reshape", "mapview",
                      "igraph", "ggnetwork", "ggraph", "intergraph", "poweRlaw",
                       "viridis", "Grid2Polygons", "raster", "leaflet", "tidyr",
                      "Hmisc", "ggimage", "BAMMtools", "stringr", "stringi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# Load packages

library(dplyr)      # to manipulate data frames
library(tidyr)      # to manipulate data frames
library(magrittr)   # Pipe operations
library(data.table) # to manipulate data frames (read.csv is ultrafast for reading CSV files)
library(ggplot2)    # to make charts and maps



library(sp)
library(rgeos)      # to get centroid of polygons using gCentroid
library(ggmap)      # to use google map tiles
library(ggsn)       # add scale bar and north arrow on ggplot2
library(maptools)   # to readShapeSpatial
library(Grid2Polygons)  # Convert spatial objects from class SpatialGridDataFrame to SpatialPolygonsDataFrame
#library(raster)         # 'Intersect' function, to intersect spatial polygons
library(sf)          # Simple (spatial) features
library(ggspatial)  # make simple maps with ggplot
library(Hmisc)        # to compute weighted decile using wtd.quantile
library(foreign)    # to export DF to other formats
library(scales)     # to get color scales
library(spatialEco) # Overlay points and polygons point.in.poly()
#library(fasttime)   # Fast version of as.POSIXct.character for GMT fixed format
library(pbapply)    # to include progress bar in apply
library(beepr)      # Beeps at the end of the command
library(gridExtra)  # Arrange Grid of ggplots
library(ggthemes)
#library(readxl)     # read excel docs
#library(xlsx)       # read and write excel files
library(cowplot)    # arrange ggplots
library(grid)
library(rgdal)
#library(parallel) # parallel processing
#library(doSNOW)   # parallel processing
library(plotly)   # interactive plots
library(plyr)
library(bit64)
#library(survey)
library(readr)
library(RColorBrewer) # nice color palettes
library(viridis)      # nice color palettes


library(mapview) # interactive view of spatial objects  >> mapview(spatial_object)
#library(mapedit) 


library(ineq)
#library(read.dbc) # read .dbc files - datasus data
library(leaflet)        # Interactive map
#library(emoGG)      # plot airport emoji in ggplot ||| https://github.com/dill/emoGG
library(ggimage) # plot airport icon in ggplot  ||| https://cran.r-project.org/web/packages/ggimage/vignettes/ggimage.html
library(spdep) # Spatial analysis
library(reshape2) # plot dcast - transformar matrix de long para wide
library(reshape) # criar combinacao de pares
library(BAMMtools) # fast calculation of jenks natural breaks

library(broom) # make it simpler to return results of regressions with glance()

library(stringr) # string editing
library(stringi) # string editing




# library(bigvis) # ggplot para big data
# gstat #geostatistical model
# spatstat #spatial point patter analysis


rm(new.packages, list.of.packages)
