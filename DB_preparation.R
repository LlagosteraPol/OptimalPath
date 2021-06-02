
rm(list = ls())

source("functions.R")

library(dils)
require(igraph)
library(roxygen2)
library(spatstat)


#Importa dades
Dades_vertex<-read.table("DB/Data/Dades_vertex.data",header=FALSE)
Dades_segments<-read.table("DB/Data/Dades_segments.data",header=FALSE)
Dades_pesos<-read.table("DB/Data/Dades_pesos.data",header=FALSE)
Dades_distancies<-read.table("DB/Data/Dades_Dist_creua.data",header=FALSE)
ACC<-read.table("DB/Data/Projected_Accidents_Cat.data", header=F) ## Lectura dels accidents


##finestre d'observació
minx<-278486
maxx<-minx+40000
miny<-4587647
maxy<-miny+40000
wind=owin(c(minx,maxx),c(miny,maxy))

#Extract point pattern
ACC_win<-ppp(ACC$V1,ACC$V2, window=wind)

#crida vertex
xvt<-Dades_vertex$V1
yvt<-Dades_vertex$V2

##crea un point pattern dels vertexs
ppp_vertex<-ppp(xvt,yvt,window=wind)

#crida els segments (arestes)
from1<-Dades_segments$V1 #son els from (index del vertex de sortida del segment)
to1<-Dades_segments$V2 #son els to (index del vertex de arribada del segment)
edgs<-cbind(from1,to1)


#crea el linnet
LN_vertex<- linnet(ppp_vertex, edges=edgs)
plot(LN_vertex,main="")
points(ACC_win$x,ACC_win$y,pch=20,col=rgb(0,0,0,alpha=0.4),cex=1.2) # Accidents
#points(ppp_vertex,pch=19,col="black")

##Passar de class linnet to class psp
pv.map.psp<-as.psp(LN_vertex)
is.psp(pv.map.psp)
Acc_LN<-project2segment(ACC_win, pv.map.psp)

plot(Acc_LN$Xproj)

##The output ppp object is
Acc_LN$Xproj

##Ara ja pots generar el objecte que contingui el point pattern i les carreteres
LN_pp<-lpp(Acc_LN$Xproj,LN_vertex)
plot(LN_pp)




#si necessites la matriu d'adjacencies només et cal fer
Matriu_adja<-LN_vertex$m

#crida el pesos de cada segment/aresta
Pesos<-Dades_pesos$V1

#per exemple, Pesos[1] és el pes de l'aresta 1 (amb el mateix ordre que estan en l'objecte LN_vertex)


# Add weights to the edges matrix
weighted_segments <- cbind(Dades_segments, Dades_pesos) 
weighted_segments <- cbind(weighted_segments, Dades_distancies)

# TODO: Potser hi ha problema en l'arrodoniment
Dades_totes <- matrix(Dades_pesos$V1 + Dades_distancies$V1) 

weighted_segments <- cbind(weighted_segments, Dades_totes)

colnames(weighted_segments) <- c("from","to","weight","distance", "all")

segments_df <- data.frame(from = weighted_segments$from,
                          to = weighted_segments$to,
                          weight= weighted_segments$weight,
                          distance = weighted_segments$distance,
                          all = weighted_segments$all)

# g <- graph_from_data_frame(segments_df, directed=FALSE) # Not working properly


#----------------------------------------CREATE GRAPH----------------------------------------

g <- graph_from_adjacency_matrix(LN_vertex$m, mode = "undirected")
# Add vertices coordinates
g <- g %>% set_vertex_attr(name = "V1", value = Dades_vertex$V1) %>% 
           set_vertex_attr(name = "V2", value = Dades_vertex$V2)
# Add edge weights
g <- g %>% set_edge_attr(name = "weight", value = weighted_segments$weight) %>% 
           set_edge_attr(name = "distance", value = weighted_segments$distance) %>% 
           set_edge_attr(name = "all", value = weighted_segments$all)


#-----------------------------------------TEST-------------------------------------------------

E(g)$weight # Check weight
E(g)$distance # Check distance

g_df = as_data_frame(g)

#plot(g, vertex.label=NA, vertex.size=2, window=TRUE, axes=TRUE, edge.label = edge_attr(g)$weight, edge.label.cex = 0.5)
plot(g, vertex.label=NA, vertex.size=2, edge.label = edge_attr(g)$weight, edge.label.cex = 0.5) # Plot with weights
plot(g, vertex.size=2) # Plot node labels


# Get a matrix with the weights of the shortest paths
shortest_weight   <-  shortest.paths(g, v=V(g), weights=E(g)$weight)
shortest_distance <-  shortest.paths(g, v=V(g), weights=E(g)$distance)


# ---------------------PLOT GRAPH--------------------------

mm = matrix(cbind(vertex_attr(g)$V1, vertex_attr(g)$V2), ncol=2)
plot(g, layout = mm, vertex.size=2, window=TRUE, axes=TRUE, 
     xlim=c(270000, 320000),ylim=c(4590000,4620000), rescale=F)

# -----------------Village node index----------------------
seros = "2"
torres = "16"
albatarrec = "22"
cogul = "31"
artesa = "32"
castelldans = "34"
albages = "36"
juneda = "44"
alamus = "52"
belloc = "54"
vilanova = "60"
menarguens = "66"
benavent = "68"
almacelles = "77"
lleida = "88"
#----------------------------------------------------------

all_shortest_paths <- best_paths(graph = g, from = almacelles, to = albages, weight = "distance")
top_shortest_paths <- all_shortest_paths[1:10]

all_safest_paths <- best_paths(graph = g, from = almacelles, to = albages, weight = "weight")
top_safest_paths <- all_safest_paths[1:10]

best_paths <- best_paths(graph = g, from = "1", to = "15", weight = "all")
top_paths <- best_paths[1:10]

print_path(ppp_vertex, top_shortest_paths[[1]]$path)

infopaths <- paths_info(graph = g, from = almacelles, to = albages)
rated <- rate_paths(graph = g, from = almacelles, to = albages)
rated <- rated[order(sapply(rated,'[[',7))]



