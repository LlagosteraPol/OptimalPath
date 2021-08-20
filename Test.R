rm(list=ls())

library(sp)
library(spatstat)
library(rgeos)
library(maptools)


#-----------------------------------------------------Yenpaty library---------------------------------------------
#To use the yen's algorithm to get the k-shortest paths, install the following library:
#library(remotes)
#install_github("ecohealthalliance/yenpathy", build_vignettes = TRUE)

library(yenpathy)

small_graph <- data.frame(
  start = c(1, 4, 5, 1, 1, 8, 1, 2, 7, 3),
  end = c(4, 5, 6, 6, 8, 6, 2, 7, 3, 6),
  weight = c(1, 1, 1.5, 5, 1.5, 2.5, 1.5, 0.5, 0.5, 0.5)
)

k_shortest_paths(small_graph, from = 1, to = 6)

#-----------------------------------------------------------------------------------------------------------------

# get.edge.ids(g, c(1,2, 4,5)) # to know the id of edges given its vertices
# as_ids(E(g)[33 %--% 35]) # same as before
# E(g)[33 %--% 35]$distance <- 999 # set weight
# adjacent_vertices(g, c(1, 34))

#-----------------------------Color palette functions------------------------------
# rainbow(n), heat.colors(n), terrain.colors(n), topo.colors(n), and cm.colors(n)
#----------------------------------------------------------------------------------

#has de carregar aquest fitxer
load(file="DB/RData/Linear_pixel_final_18_Juny_2021_ultima_copia.RData")

#Ara aquí tens:

#----------------------------------PLOT HEATMAP------------------------------------
#ww es el mapa de densitat (pixel a pixel) que tens de intensitat d'accidents
pdf("Images/Heatmap.pdf",height=6,width=13.5)
plot(ww, main = "")
dev.off()
#----------------------------------------------------------------------------------

points(LN_pp,pch=19,col="red", cex=0.5) #LN_pp és el patró puntual projectat
#LNnew és el mapa carreteres (simplificat, però realista) on no surt 
# aquella conneccio (aquesta hauria d'anar a la Figura1)

plot(LNnew)

#L'objected lpp (network més point pattern) és l'objecte LN_pp
#Objected linnet amb el patró puntual i LNnew, és LN_pp
plot(LN_pp,axes=TRUE)

#El vector amb els pesos de les intensitat per ara 436 arestes es    seg_m # 

#El vector amb les distancies es  length_seg_entre_cross
ls(LNnew)

#---------------------------------------------------PLOT------------------------------------------------------
source("functions.R")
image(ww,col=topo.colors(100)) ##per mostrar els pesos per pixel, ww
x11()
plot.lpp.lines(LNnew,seg_m) ##per ensenyar els pesos per lines

#-------------------------------Check ID of element in array by value----------------------------------------
a <- c(3, 2, -7, -3, 5, 2)
c <- which(a==-7) # this will give you numerical value

b <- list(list(3,4,5))
d <- which(unlist(b)==4)

#---------------------------------------------------Create graph from DataFrame EXAMPLE---------------------------------------------------
## A simple example with a couple of actors
## The typical case is that these tables are read in from files....
actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)

## The opposite operation
as_data_frame(g, what="vertices")
as_data_frame(g, what="edges")

#------------------------------------------------------------Get shortest paths-----------------------------------------------------------
source("functions.R")
source("IgraphModel.R")

#filtered_paths <- filter_paths(graph = g, from = alcarras, to = albages, edge_param = "distance", filter = 10000)

juneda = 63
soses = 161
menarguens = 197
belloc = 209
#(graph = g, from = soses, to = belloc)
short1 <- get.shortest.paths(g,soses,belloc)
short2 <- shortest_paths(g,soses,belloc)

short_dst <- get.shortest.paths(g,soses,belloc, weight=E(g)$distance)

top_paths <- get_k_shortest_paths(g, from = soses, to = belloc, weight='distance', k=10)


#-----------------------------------------------------------------------------------------------------------------------------------------
library(sf)
library(maptools)
library(raster)
load(url("http://github.com/mgimond/Spatial/raw/master/Data/ppa.RData"))

# Load a pop_sqmile.img population density raster layer
img  <- raster("pop_sqmile.img")
pop  <- as.im(img)
library(spatstat)
marks(starbucks)  <- NULL
Window(starbucks) <- ma
Q <- quadratcount(starbucks, nx= 6, ny=3)
Q.d <- intensity(Q)

# Plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(starbucks, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

#----------------------------------------------------------------------------------------------------------------------------------------

paths = list()
for (path_info in all_shortest_paths11){
  print("--------------------------------------------------------------------------------------------------------------------------------------")
  print(path_info$path)
  #append(paths, path_info$path)
}

#-------------------------------------------------------Rating path test---------------------------------------------------------------------------------

vertices_data <- data.frame(ID=c(1,2,3,4,5,6,7,8,9,10))

edges_data <- cbind(c(1,  1,  1,  2,  2,  2,  2,  3,  3,  4,  4,  5,  5,  6,  7,  7,  8,  9), 
                    c(2,  4,  6,  3,  4,  5,  7,  5,  8,  6,  7,  7,  8,  9,  9,  10, 10, 10))

intensity_data <-   c(3,  7,  2,  8,  6,  10, 5,  10, 3,  4,  8,  6,  2,  1,  9,  8,  7,  1)
distance_data <-    c(12, 31, 38, 15, 28, 29, 32, 40, 22, 19, 24, 31, 19, 33, 39, 24, 31, 17)

segments_info <- cbind(edges_data, intensity_data) 
segments_info <- cbind(segments_info, distance_data) 
colnames(segments_info) <- c("from","to","intensity","distance")

edges_df = data.frame(from = segments_info[, 'from'],
                      to = segments_info[, 'to'],
                      intensity = segments_info[, 'intensity'],
                      distance = segments_info[, 'distance'])


g_t = graph_from_data_frame(edges_df, directed=FALSE, vertices=vertices_data)


f = 1
t = 10

all_paths_t <- all_simple_paths(g_t, from=f, to=t)
ipaths_t <- list()
for (path in all_paths_t){
  
  intensity_sum <- sum(E(g_t, path = unlist(path))$intensity)
  
  distance_sum <- sum(E(g_t, path = unlist(path))$distance)
  
  ipaths_t[[length(ipaths_t)+1]] <- list(from = f, to=t, path = as.numeric(unlist(as_ids(path))), 
                                     intensity = intensity_sum, 
                                     distance = distance_sum)
}

#intensity order
ipaths_t <- ipaths_t[order(sapply(ipaths_t,'[[',4))]
for(i in 1:length(ipaths_t)){
  ipaths_t[[i]] <- c(ipaths_t[[i]], n_intensity=i)
}

# distance order
ipaths_t <- ipaths_t[order(sapply(ipaths_t,'[[',5))]
for(i in 1:length(ipaths_t)){
  ipaths_t[[i]] <- c(ipaths_t[[i]], n_distance=i)
}

intensity_ordered <- ipaths_t[order(sapply(ipaths_t,'[[',4))]
distance_ordered <- ipaths_t[order(sapply(ipaths_t,'[[',5))]

#------------------------------------------------------------Rating with 2 variables---------------------------------------------------------------------------------
load("C:/Users/usuari/RProjects/OptimalPath/DB/RData/Rating_0505_28_july_2021.RData")

safest_jun_men <- list()


for (path in jun_men_weight_ordered){
  if(jun_men_weight_ordered[[1]][5] == path[[5]]){
    if(length(safest_jun_men) == 0){
      safest_jun_men <- list(path)
    }else{
      safest_jun_men <- rbind(safest_jun_men, list(path))
    }
    
  }else{
    break
  }
}
safest_jun_men_ordered <- safest_jun_men[order(sapply(safest_jun_men,'[[',4))]
for (i in 1:length(safest_jun_men_ordered)){print(safest_jun_men_ordered[[i]][["distance"]])}

safest_sos_bell <- list()
bk = FALSE
for (path in sos_bell_weight_ordered){
  if(sos_bell_weight_ordered[[1]][5] == path[[5]]){
    if(length(safest_sos_bell) == 0){
      safest_sos_bell <- list(path)
    }else{
      safest_sos_bell <- rbind(safest_sos_bell, list(path))
    }
    bk = TRUE
  }else if(bk){
    break
  }
}
safest_sos_bell_ordered <- safest_sos_bell[order(sapply(safest_sos_bell,'[[',4))]
for (i in 1:length(safest_sos_bell_ordered)){print(safest_sos_bell_ordered[[i]][["distance"]])}

require(igraph)
t1 <- E(g, path = safest_jun_men_ordered[[1]]$path)$weight
t2 <- E(g, path = safest_jun_men_ordered[[2]]$path)$weight

#------------------------------------------------------Get sets of pats with same weight---------------------------------------------------------------------------
load(file="DB/RData/Rating_0505_28_july_2021.RData")

safest_jun_lst<- list()

tmp <- jun_men_weight_ordered[[1]][5]$weight
ctr <- 1
i <- 1
for (path in jun_men_weight_ordered){
  if(jun_men_weight_ordered[[i]][5]$weight == tmp){
    ctr <- ctr + 1
  }else{
    tmp <- jun_men_weight_ordered[[i]][5]$weight
    if(length(safest_jun_lst) == 0){
      safest_jun_lst <- list(list(amount = ctr, weight = tmp))
    }else{
      safest_jun_lst <- rbind(safest_jun_lst, list(list(amount = ctr, weight = tmp)))
    }
    ctr <- 1
  }
  i <- i+1
}

safest_sos_lst<- list()

tmp <- sos_bell_weight_ordered[[1]][5]$weight
ctr <- 1
i <- 1
for (path in sos_bell_weight_ordered){
  if(sos_bell_weight_ordered[[i]][5]$weight == tmp){
    ctr <- ctr + 1
  }else{
    tmp <- sos_bell_weight_ordered[[i]][5]$weight
    if(length(safest_sos_lst) == 0){
      safest_sos_lst <- list(list(amount = ctr, weight = tmp))
    }else{
      safest_sos_lst <- rbind(safest_sos_lst, list(list(amount = ctr, weight = tmp)))
    }
    ctr <- 1
  }
  i <- i+1
}


