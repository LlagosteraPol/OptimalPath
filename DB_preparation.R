
rm(list = ls())

library(dils)
require(igraph)
library(roxygen2)
library(spatstat)


# Set working directory
#setwd("R")

get_all_paths <- function(g){
  direct <- get.adjacency(g)
  indirect <- direct
  max <- vcount(g)-1
  for(i in 1:max){
    for(j in 1:max){
      indirect[i,j] <- length(get.all.shortest.paths(g, from=i, to=j))
    }
  }
  return(indirect)
}

#Importa dades
Dades_vertex<-read.table("Data/Dades_vertex.data",header=FALSE)
Dades_segments<-read.table("Data/Dades_segments.data",header=FALSE)
Dades_pesos<-read.table("Data/Dades_pesos.data",header=FALSE)
Dades_distancies<-read.table("Data/Dades_Dist_creua.data",header=FALSE)


##finestre d'observació
minx<-278486
maxx<-minx+40000
miny<-4587647
maxy<-miny+40000
wind=owin(c(minx,maxx),c(miny,maxy))

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
plot(LN_vertex)
points(ppp_vertex,pch=19,col="red")


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

g <- graph_from_data_frame(segments_df, directed=FALSE)
E(g)$weight # Check weight
E(g)$distance # Check distance

g_df = as_data_frame(g)

#plot(g, vertex.label=NA, vertex.size=2, window=TRUE, axes=TRUE, edge.label = edge_attr(g)$weight, edge.label.cex = 0.5)
plot(g, vertex.label=NA, vertex.size=2, edge.label = edge_attr(g)$weight, edge.label.cex = 0.5) # Plot with weights

# Get a matrix with the weights of the shortest paths
shortest_weight   <-  shortest.paths(g, v=V(g), weights=E(g)$weight)
shortest_distance <-  shortest.paths(g, v=V(g), weights=E(g)$distance)


#' Get the shortest n paths between two nodes (depending on weight).
#' 
#' @name best_n_paths
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @param n Number of bests paths to show
#' @param weight The weight to calculate the shortests paths, can be 'weight' or 'distance' (default = weight)
#' @return list of lists containing the top best n paths and its tota weight.
#' 
best_n_paths <- function(graph, from, to, n, weight='weight'){
  all_paths <- all_simple_paths(g, from=from, to=to)

  top_n <- list()
  
  for (path in all_paths){
    if(weight == "distance"){
      weight_sum <- sum(E(g, path = unlist(path))$distance)
    }
    else if (weight == "weight"){
      weight_sum <- sum(E(g, path = unlist(path))$weight)
    }
    else{
      weight_sum <- sum(E(g, path = unlist(path))$all)
    }
    
    if(length(top_n) < n){
      top_n[[length(top_n)+1]] <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
    }
    else{
      top_n <- top_n[order(sapply(top_n,'[[',1))]
      
      if(weight_sum < top_n[[1]]$weight){
        top_n[[1]] <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
      }
    }
  }
  return(top_n)
}


#' Print the map of the given ppp object with the given path as green nodes, it also print the other nodes as red
#' 
#' @name print_path
#' @param ppp_obj Point pattern object
#' @param path Path to be plotted
#' @return a plot of the map with the given path
#' 
print_path <- function(ppp_obj, path){
  ln_vertices<- linnet(ppp_obj, edges=edgs)
  plot(ln_vertices)
  points(ppp_obj,pch=19,col="red")
  points(ppp_obj[path],pch=19,col="green")
}



top_shortest_paths <- best_n_paths(g, "1", "15", 10, "distance")

top_safer_paths <- best_n_paths(g, "1", "15", 10)

top_paths <- best_n_paths(g, "1", "15", 10, "all")

print_path(ppp_vertex, top_shortest_paths[[1]]$path)





