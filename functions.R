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


#' Get all the paths between two nodes and gives information about its weight (accident intensity),
#' distance, transformed weight and transformed distance (weight and distance
#' divided by its respective maximums in the network)
#' 
#' @name paths_info
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @return list of lists containing the all the paths between the two given nodes with information 
#' related to them.
#' 
paths_info <- function(graph, from, to){
  all_paths <- all_simple_paths(g, from=from, to=to)
  
  ipaths <- list()
  
  for (path in all_paths){
    
    distance_sum <- sum(E(g, path = unlist(path))$distance)

    weight_sum <- sum(E(g, path = unlist(path))$weight)
    
    t_weight_sum <- sum(E(g, path = unlist(path))$t_weight)
    
    t_distance_sum <- sum(E(g, path = unlist(path))$t_distance)
      
    ipaths[[length(ipaths)+1]] <- list(from = from, to=to, path = as.numeric(unlist(as_ids(path))), 
                                       distance = distance_sum, 
                                       weight = weight_sum,
                                       t_distance = t_distance_sum,
                                       t_weight = t_weight_sum)
  }
  return(ipaths)
}


#' Get all the paths between two nodes and gives in which position are each of its weights (best to worst)
#' 
#' @name rate_paths
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @return list of lists containing the all the paths with their rated by position.
#' 
rate_paths <- function(graph, from, to){
  ipaths <- paths_info(graph = graph, from = from, to = to)
  
  # distance order
  ipaths <- ipaths[order(sapply(ipaths,'[[',4))]
  for(i in 1:length(ipaths)){
    ipaths[[i]] <- append(ipaths[[i]], list(n_distance=i))
  }
  
  #accident intensity order
  ipaths <- ipaths[order(sapply(ipaths,'[[',5))]
  for(i in 1:length(ipaths)){
    ipaths[[i]] <- append(ipaths[[i]], list(n_weight=i))
  }
  
  #accident intensity order
  ipaths <- ipaths[order(sapply(ipaths,'[[',6))]
  for(i in 1:length(ipaths)){
    ipaths[[i]] <- append(ipaths[[i]], list(n_t_distance=i))
  }
  
  #accident intensity order
  ipaths <- ipaths[order(sapply(ipaths,'[[',7))]
  for(i in 1:length(ipaths)){
    ipaths[[i]] <- append(ipaths[[i]], list(n_t_weight=i))
  }
  
  return(ipaths)
}

#' Get all the paths between two nodes ordered from less to more weight.
#' 
#' @name ordered_paths
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @param edge_param The weight to calculate the shortests paths, can be 'weight' or 'distance'
#' @return list of lists containing the all the paths ordered by weight.
#' 
ordered_paths <- function(graph, from, to, edge_param){
  all_paths <- all_simple_paths(g, from=from, to=to)
  
  paths_ordered <- list()
  
  for (path in all_paths){
    if(edge_param == "distance"){
      weight_sum <- sum(E(g, path = unlist(path))$distance)
    }
    else if (edge_param == "weight"){
      weight_sum <- sum(E(g, path = unlist(path))$weight)
    }
    else{
      weight_sum <- sum(E(g, path = unlist(path))$all)
    }
    paths_ordered[[length(paths_ordered)+1]] <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
  }
  return(paths_ordered[order(sapply(paths_ordered,'[[',1))])
}


#' Get all the paths between two nodes ordered from less to more weight.
#' It saves in another list, alls the paths that contains edges with 
#' weight superior to the specified in the function.
#' 
#' @name filter_paths
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @param edge_param The weight to calculate the shortests paths, can be 'weight' or 'distance'
#' @param filter Limit weight that an edge of a path can contain
#' @return list of lists containing the all the paths ordered by weight
#' and another identical list but with the paths with edges that has 
#' the sepecified limit weight or greater.
#' 
filter_paths <- function(graph, from, to, edge_param, filter){
  all_paths <- all_simple_paths(g, from=from, to=to)
  
  paths_ordered <- list()
  black_list <- list()
  is_forbiden <- FALSE
  
  for (path in all_paths){
    weight_sum <- 0
    is_forbiden <- FALSE
    for (edge in path){
      if(edge_attr(g, edge_param)[edge] >= filter){
        is_forbiden <- TRUE
      }
      weight_sum <- weight_sum + edge_attr(g, edge_param)[edge]
    }
    if (is_forbiden){
      black_list[[length(black_list)+1]] <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
    }
    
    else{
      paths_ordered[[length(paths_ordered)+1]] <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
    }
    
  }
  return(list(paths = paths_ordered[order(sapply(paths_ordered,'[[',1))], 
              black_list = black_list[order(sapply(black_list,'[[',1))]))
}

filter_graph <- function(graph, edge_param, filter){
  # delete_edges(graph, edges)
}

#' Print the map of the given ppp object with the given path as green nodes, it also print the other nodes as red
#' 
#' @name print_path_ppp
#' @param ppp_obj Point pattern object
#' @param path Path to be plotted
#' @return a plot of the map with the given path
#' 
print_path_ppp <- function(ppp_obj, path){
  ln_vertices<- linnet(ppp_obj, edges=edgs)
  plot(ln_vertices,col="blue")
  points(ppp_obj,pch=19,col="red")
  points(ppp_obj[path],pch=19,col="green")
}

#' Print the map of the given a 'igraph' object with the given path as green nodes
#' as well as the edges
#' 
#' @name print_path_graph
#' @param g <igraph> Graph object
#' @param path Path to be plotted
#' @param color <string> Color of the path 
#' @return a plot of the map with the given path
#' 
print_path_graph <- function(g, path, color){
  vcol <- rep("black", vcount(g))
  vcol[path] <- color
  
  ecol <- rep("black", ecount(g))
  ecol[E(g, path=path)] <- color

  mtx = matrix(cbind(vertex_attr(g)$V1, vertex_attr(g)$V2), ncol=2)
  plot(g, layout = mtx, 
       vertex.size=3, vertex.color=vcol, vertex.label="",
       edge.color=ecol,
       window=FALSE, axes=FALSE)
  box()
}

##funció plot.lpp.lines()
plot.lpp.lines<- function(LNnew,seg_m,width_line=0.1){
  
  x0<-c();y0<-c();x1<-c();y1<-c()
  for(i in 1:LNnew$lines$n){
    x0[i]<-LNnew$lines$ends$x0[i]
    y0[i]<-LNnew$lines$ends$y0[i]
    x1[i]<-LNnew$lines$ends$x1[i]
    y1[i]<-LNnew$lines$ends$y1[i]
  }
  
  #generar punts
  j1<-0;x<-c();y<-c();mk<-c()
  for(i in 1:LNnew$lines$n){
    m<-(y1[i]-y0[i])/(x1[i]-x0[i])
    a<-y0[i]-m*x0[i]
    for(j in 1:100){
      j1<-j1+1
      x[j1]<-runif(1,min(x0[i],x1[i]),max(x0[i],x1[i]))
      y[j1]<-m*x[j1]+a
      mk[j1]<-seg_m[i]
    }
  }
  pppp<-ppp(x,y,marks=mk,window=LNnew$window)
  plot(LNnew, main="")
  plot(pppp, pch=19,cex=width_line,cols=topo.colors(100),add=TRUE)
  rect(LNnew$window$xrange[1],LNnew$window$yrange[1],LNnew$window$xrange[2],LNnew$window$yrange[2],
       border="black",lwd=1)
}