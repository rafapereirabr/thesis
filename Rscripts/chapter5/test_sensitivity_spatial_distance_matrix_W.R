



###### Distance Decay with i distant cap --------------



myseq <- seq(20, 50, 10) 

n=length(myseq); res_decay <- data.frame(k=myseq,I=rep(NA,n)) %>% setDT()

for(i in myseq){  
  cat(i," ")
  
  # convert to matrix format
  ttmatrix3 <- acast(all_to_all, origin~destination, value.var="travel_time") %>% as.matrix()
  ttmatrix3 <- as.matrix(ttmatrix3)
  
  ttmatrix3[ttmatrix3 > i ] <-0 # cap to trips longer than i
  
  
# get an inverse matrix of distances, make sure diagonal=0
  W <- 1/ttmatrix3
  diag(W) <- 0
  
  
# replace NA values with 0, so that areas are not neighbors
  W[!is.finite(W)] <- NA # convert inf into NA
  sum(is.na(W))
  W[is.na(W)] <- 0
  sum(is.na(W))
  
# # row-normalized Matrix > fica mais rapido, mas a opcao style="W" de mat2listw ja faz a normalizacao
#   rtot <- rowSums(W, na.rm=TRUE)
#   W <- W / rtot
#   
#   # Find IDs with no neighbours
#     IDs_with_no_neighbours <- rownames(W)[ rowSums(W, na.rm=TRUE) !=1 ]
#     
#   # Remove IDs_with_no_neighbours from matrix W rows and columns
#     W <- W[!(rownames(W) %in% IDs_with_no_neighbours), !(colnames(W) %in% IDs_with_no_neighbours)]
#     nrow(W)

  # create weight matrix
    weights_ttime <- mat2listw(W, row.names = row.names(W), style="W" ) # W > row-standardised weights

  # update regression
    temp_data <- subset(map, map@data$ID %in% rownames(W))
    temp_f1_ols <- lm( log(ratio) ~ log(RDPC)+log(popdens)+log(jobdens)+log(elevation), weight=pop,data=temp_data@data)
  
  # test moran
    temp_value <- lm.morantest(temp_f1_ols, listw=weights_ttime,zero.policy=T)$estimate[1]
    
  # keep result
    res_decay[, I := ifelse(k==i, temp_value, I) ]
}

plot(res_decay,type="b",main="Moran's I by distance cap",pch=20,cex=1.5)


# 9min 0.6364088243






###### Near distance --------------

# Neighbourhood contiguity by distance
# Use "dis.neigh" function from here https://stackoverflow.com/questions/43557993/how-to-input-dissimilarity-matrix-in-spatial-analysis-in-spdep-r
# Isso eh uma versao adaptada de dnearneigh

myseq <- seq(20, 60, 10) 

n=length(myseq); res_near <- data.frame(k=myseq,I=rep(NA,n)) %>% setDT()

for(i in myseq){  
  cat(i," ")
  
  # convert to matrix format
  ttmatrix3 <- acast(all_to_all, origin~destination, value.var="travel_time") %>% as.matrix()

  diag(ttmatrix3) <- 0
  
  # replace NA values with 0, so that areas are not neighbors
#  ttmatrix3[!is.finite(ttmatrix3)] <- NA # convert inf into NA
  sum(is.na(ttmatrix3))
  ttmatrix3[is.na(ttmatrix3)] <- 0
  sum(is.na(ttmatrix3))
  
  
  nb_list <- dis.neigh(ttmatrix3, d1=0, d2=  i)
  temp_value <- lm.morantest(f1_ols, listw=nb_list, zero.policy=T)$estimate[1]
  res_near[, I := ifelse(k==i, temp_value, I) ]
}




plot(res_near,type="b",main="Moran's I by Max distance",pch=20,cex=1.5)








dis.neigh<-function(x, d1 = 0, d2=50){
  #x must be a symmetrical distance matrix
  #create empty list
  style = "M" #for style unknown
  neighbours<-list()
  weights<-list()
  #set attributes of neighbours list
  attr(neighbours, "class")<-"nb"
  attr(neighbours, "distances")<-c(d1,d2)
  attr(neighbours, "region.id")<-colnames(x)
  
  #check each row for neighbors that satisfy distance threshold
  neighbour<-c()
  weight<-c()
  i<-1
  for(row in c(1:nrow(x))){
    j<-1
    for(col in c(1:ncol(x))){
      if(x[row,col]>d1 && x[row,col]<d2){
        neighbour[j]<-col
        weight[j]<-1/x[row,col] #inverse distance (dissimilarity)
        j<-1+j
      }
    }
    neighbours[i]<-list(neighbour)
    weights[i]<-list(weight)
    i<-1+i
  }
  
  #create neighbour and weight list
  res <- list(style = style, neighbours = neighbours, weights = weights)
  class(res) <- c("listw", "nb")
  attr(res, "region.id") <- attr(neighbours, "region.id")
  attr(res, "call") <- match.call()
  
  return(res)
}

















