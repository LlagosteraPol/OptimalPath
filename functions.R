library(dils)
require(igraph)
library(roxygen2)
library(spatstat)
library(yenpathy)
library(parallel)


#' Calculates the wheighted intensities and distances based on formula W(l_i)
#' 
#' @name w_li
#' @param intensities Dataframe containing all the accident intensities
#' @param distances Dataframe containing all the distances
#' @param a weighting for intensities (a+b=1)
#' @param b weighting for distances (a+b=1)
#' @return list with weighted intensities and distances
#' 
weighted_data <- function(intensities, distances, a, b){
  
  transformed_accIntensities <- data.frame(V1=mapply(FUN = `-`, intensities, 
                                                     min(intensities), SIMPLIFY = FALSE))
  
  transformed_accIntensities <- data.frame(V1=mapply(FUN = `/`, transformed_accIntensities, 
                                                     (max(intensities)-min(intensities)), SIMPLIFY = FALSE))
  
  transformed_accIntensities <- data.frame(V1=mapply(FUN = `*`, transformed_accIntensities, 
                                                     a, SIMPLIFY = FALSE))
  
  transformed_distances <- data.frame(V1=mapply(FUN = `-`, distances, 
                                                min(distances), SIMPLIFY = FALSE))
  
  transformed_distances <- data.frame(V1=mapply(FUN = `/`, transformed_distances, 
                                                (max(distances)-min(distances)), SIMPLIFY = FALSE))
  
  transformed_distances <- data.frame(V1=mapply(FUN = `*`, transformed_distances, 
                                                b, SIMPLIFY = FALSE))
  
  return(list(a_intensities = transformed_accIntensities, b_distances = transformed_distances))
}

#' Gives all the shortest paths lenght between each pair of nodes of the given graph (network)
#' 
#' @name get_all_paths
#' @param graph The graph on which calculates the paths
#' @return matrix with all the shortest path lenghts
#' 
get_all_paths <- function(graph){
  direct <- get.adjacency(graph)
  indirect <- direct
  max <- vcount(graph)-1
  for(i in 1:max){
    for(j in 1:max){
      indirect[i,j] <- length(get.all.shortest.paths(graph, from=i, to=j))
    }
  }
  return(indirect)
}

#' Function that returns the crossings of the network
#' 
#' @name getCrossings
#' @param graph The graph on which calculates the paths
#' @return crossings - array with an array of nodes with degree > 2
#' 
getCrossings = function(graph){
  crossings <- c()
  for(node in V(graph)){
    if(degree(graph, node)>2)
    {
      crossings <- c(crossings, node)
    }
  }
  crossings
}

#' Get the top k shortest paths between two nodes. It uses the library 'yenpathy', To install the library do the following:
#' library(remotes)
#' install_github("ecohealthalliance/yenpathy", build_vignettes = TRUE)
#' 
#' @name get_k_shortest_paths
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @param weight The weight to calculate the shortests paths, can be 'weight' or 'distance'
#' @param k number of shortest paths to be returned
#' @return list of lists containing the top k-shortest paths where each path is represented by the vertices constituting it
#' 
get_k_shortest_paths <- function(graph, from, to, weight='weight', k){
  # Get k shortest paths using 'yenpathy' library
  tmp_df <- as_data_frame(as.directed(graph, mode = "mutual")) # the graph must be converted to directed
  
  #Construct the dataframe that will be used to pass to the k_shortest_path 'yenpathy' function
  g_df <- data.frame(
    start = as.numeric(unlist(tmp_df$from)), # The ID's are characters (Ex: '1') must be converted to numbers
    end = as.numeric(unlist(tmp_df$to)),
    weight = as.numeric(unlist(if (weight=='distance') tmp_df$distance else tmp_df$weight))
  )
  
  return(k_shortest_paths(g_df, from = from, to = to, k=k))
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
  all_paths <- all_simple_paths(graph, from=from, to=to)
  
  ipaths <- list()
  
  for (path in all_paths){
    
    distance_sum <- sum(E(graph, path = unlist(path))$distance)

    weight_sum <- sum(E(graph, path = unlist(path))$weight)
    
    t_weight_sum <- sum(E(graph, path = unlist(path))$t_weight)
    
    t_distance_sum <- sum(E(graph, path = unlist(path))$t_distance)
    
    all_sum <- sum(E(graph, path = unlist(path))$all)
      
    ipaths[[length(ipaths)+1]] <- list(from = from, to=to, path = as.numeric(unlist(as_ids(path))), 
                                       distance = distance_sum, 
                                       weight = weight_sum,
                                       t_distance = t_distance_sum,
                                       t_weight = t_weight_sum,
                                       all = all_sum) 
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
  
  if (length(ipaths) == 0){
    return(ipaths)
  }
  
  # distance order
  ipaths <- ipaths[order(sapply(ipaths,'[[',4))]
  for(i in 1:length(ipaths)){
    #ipaths[[i]] <- append(ipaths[[i]], list(n_distance=i))
    ipaths[[i]] <- c(ipaths[[i]], n_distance=i)
  }
  
  #accident intensity order
  ipaths <- ipaths[order(sapply(ipaths,'[[',5))]
  for(i in 1:length(ipaths)){
    ipaths[[i]] <- c(ipaths[[i]], n_weight=i)
  }
  
  #transformed distance order
  ipaths <- ipaths[order(sapply(ipaths,'[[',6))]
  for(i in 1:length(ipaths)){
    ipaths[[i]] <- c(ipaths[[i]], n_t_distance=i)
  }
  
  #transformed accident intensity order
  ipaths <- ipaths[order(sapply(ipaths,'[[',7))]
  for(i in 1:length(ipaths)){
    ipaths[[i]] <- c(ipaths[[i]], n_t_weight=i)
  }
  
  #both transformed accident + distance with ponderation order
  ipaths <- ipaths[order(sapply(ipaths,'[[',8))]
  for(i in 1:length(ipaths)){
    ipaths[[i]] <- c(ipaths[[i]], n_t_all=i)
  }
  
  return(ipaths)
}


#' Get all the paths between two nodes ordered from less to more weight.
#' 
#' @name ordered_paths
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @param weight The weight to calculate the shortests paths, can be 'weight' or 'distance'
#' @return list of lists containing the all the paths ordered by weight.
#' 
ordered_paths <- function(graph, from, to, weight){
  all_paths <- all_simple_paths(graph, from=from, to=to)
  
  paths_ordered <- list()
  
  for (path in all_paths){
    if(weight == "distance"){
      weight_sum <- sum(E(graph, path = unlist(path))$distance)
    }
    else if (weight == "weight"){
      weight_sum <- sum(E(graph, path = unlist(path))$weight)
    }
    else{
      weight_sum <- sum(E(graph, path = unlist(path))$all)
    }
    paths_ordered[[length(paths_ordered)+1]] <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
  }
  return(paths_ordered[order(sapply(paths_ordered,'[[',1))])
}



#' Get all the paths between two nodes ordered from less to more weight.
#' All paths that contains edges with weight greater to the specified 
#' in the function are saved in another list.
#' 
#' @name filter_paths_old
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @param weight The weight to calculate the shortests paths, can be 'weight' or 'distance'
#' @param filter <array> Weight limits that an edge of a path can contain
#' @param paths The paths that want to be filtered, if it's empty, will calculate all paths
#' @return list of lists containing the all the paths ordered by weight
#' and another identical list but with the paths with edges that has 
#' the sepecified limit weight or greater.
#' 
filter_paths_old <- function(graph, from, to, weight, filter, paths = NULL){
  if (is.null(paths)){
    paths <- all_simple_paths(graph, from=from, to=to)
  }
  
  
  paths_ordered <- list()
  black_list <- list()
  is_forbiden <- FALSE
  
  for (path in paths){
    weight_sum <- 0
    is_forbiden <- FALSE
    for (edge in path){
      if(edge_attr(g, weight)[edge] >= filter){
        is_forbiden <- TRUE
        break
      }
      weight_sum <- weight_sum + edge_attr(graph, weight)[edge]
    }
    
    if(!is_forbiden){
      if(length(paths_ordered)==0){
        paths_ordered <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
      }else{
        paths_ordered <- list(paths_ordered, list(weight = weight_sum, path = as.numeric(unlist(as_ids(path)))))
      }
      
    }
    # if (is_forbiden){
    #   black_list[[length(black_list)+1]] <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
    # }
    # 
    # else{
    #   paths_ordered[[length(paths_ordered)+1]] <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
    # }
    
  }
  return(list(paths = paths_ordered[order(sapply(paths_ordered,'[[',1))]))
  # return(list(paths = paths_ordered[order(sapply(paths_ordered,'[[',1))], 
  #             black_list = black_list[order(sapply(black_list,'[[',1))]))
}


#' Get all the paths between two nodes ordered from less to more weight.
#' All paths that contains edges with weight greater to the specified 
#' in the function are saved in another list.
#' 
#' @name filter_paths
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @param weight The weight to calculate the shortests paths, can be 'weight' or 'distance'
#' @param filters <array> Weight limits that an edge of a path can contain
#' @param paths The paths that want to be filtered, if it's empty, will calculate all paths
#' @return list of lists containing the all the paths ordered by weight
#' and another identical list but with the paths with edges that has 
#' the sepecified limit weight or greater.
#' 
filter_paths <- function(graph, from, to, weight, filters, paths = NULL){
  paths_ordered <- vector(mode="list", length=length(filters))
  names(paths_ordered) <- filters
  filters <- as.numeric(unlist(filters)) # transform strings to integuers
  is_forbiden <- FALSE
  
  if(weight == 'distance'){
    max_parameter  = max(E(graph)$distance)
  }else{
    max_parameter = max(E(graph)$weight)
  }
  
  if (is.null(paths)){
    paths <- all_simple_paths(graph, from=from, to=to)
  }
  
  pb = txtProgressBar(min = 0, max = length(paths), initial = 0) 
  counter <- 0
  cat("Filtering paths...\n")
  for(path in paths){
    setTxtProgressBar(pb,counter)
    weight_sum <- 0
    is_forbiden <- TRUE
    filters_idx <- 1
    
    for(percent in filters){
      if(weight == 'distance'){
        
        if(max(E(graph, path = unlist(path))$distance) < (max_parameter*(percent/100))){
          weight_sum <- sum(E(graph, path = unlist(path))$distance)
          is_forbiden <- FALSE
          break
        }
      }else{
        if(max(E(graph, path = unlist(path))$weight) < (max_parameter*(percent/100))){
          weight_sum <- sum(E(graph, path = unlist(path))$weight)
          is_forbiden <- FALSE
          break
        }
      }
      if(is_forbiden){
        filters_idx <- filters_idx+1
      }
    }
    
    if(!is_forbiden){
      paths_ordered[[toString(filters[filters_idx])]][[length(paths_ordered[[toString(filters[filters_idx])]])+1]] <- 
        list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
    }
    counter <- counter + 1
  }
  close(pb)
  
  #return(list(paths = paths_ordered[order(sapply(paths_ordered,'[[',1))]))
  return(paths_ordered)
}

#' Delete all the edges with weight equal to or greater than the specified amount
#'
#' @name filter_graph
#' @param graph The graph on which calculates the paths
#' @param filter Maximum weight that the edges can contain
#' @param weight Weight type, can be 'distance' or 'weight'
#' @return graph without the edges with weight equal or greater than the given filter
#' 
filter_graph <- function(graph, filter, weight){
  if(weight == 'distance'){
    max_distance  = max(E(graph)$distance)
    return(delete.edges(graph, which(E(graph)$distance>=(max_distance*(filter/100)))))  
  }else{
    max_weight  = max(E(graph)$weight)
    return(delete.edges(graph, which(E(graph)$weight>=(max_weight*(filter/100)))))    
  }
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
#' @param graph <igraph> Graph object
#' @param path Path to be plotted
#' @param color <string> Color of the path 
#' @return a plot of the map with the given path
#' 
print_path_graph <- function(graph, path, color){
  vcol <- rep("black", vcount(graph))
  vcol[path] <- color
  vcol[path[1]] <- "sandybrown" # Path first node color
  vcol[path[length(path)]] <- "red" # Path last node color
  
  ecol <- rep("black", ecount(graph))
  ecol[E(graph, path=path)] <- color

  E(graph)$width <- 1
  E(graph, path=path)$width <- 3
  
  mtx = matrix(cbind(vertex_attr(graph)$V1, vertex_attr(graph)$V2), ncol=2)
  plot(graph, layout = mtx, 
       vertex.size=3, 
       vertex.color=vcol, 
       #vertex.frame.color=vcol,
       mark.border=NA,
       vertex.label="",
       edge.color=ecol,
       window=FALSE, axes=FALSE)
  rect(-1,-1,1,1,border="black",lwd=2)
}


#' Plot linnet object
#' 
#' @name plot.lpp.lines
#' @param LNnew Linnet object
#' @param width_line Witdth of the plotted lines
#' 
plot.lpp.lines <- function(LNnew,seg_m,width_line=0.1){
  
  x0<-c();y0<-c();x1<-c();y1<-c()
  for(i in 1:LNnew$lines$n){
    x0[i]<-LNnew$lines$ends$x0[i]
    y0[i]<-LNnew$lines$ends$y0[i]
    x1[i]<-LNnew$lines$ends$x1[i]
    y1[i]<-LNnew$lines$ends$y1[i]
  }
  
  #Generate points
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
  plot(pppp, pch=19,cex=width_line,cols=topo.colors(100),add=TRUE, main="", image=TRUE)
  rect(LNnew$window$xrange[1],LNnew$window$yrange[1],LNnew$window$xrange[2],LNnew$window$yrange[2],
       border="black",lwd=1)
}


#' Combine two sets of weights into one and using a proportion.
#' 
#' @name combine_weights
#' @param data1 <dataframe> With weights
#' @param data2 <dataframe> With weights
#' @param prop Proportion used in data1 (data2 will use 1-proportion), must be less or equal than 1
#' @return <dataframe> with the weights combined
#' 
combine_weights <- function(data1, data2, prop = 0.5){
  if (prop > 1){
    print("Weight must equal or less than 1. Continuing without proportion.")
    prop <- 1
  }
  
  
  transformed_data1 <- data.frame(V1=mapply(FUN = `*`, data1, prop, SIMPLIFY = FALSE))
  transformed_data2 <- data.frame(V1=mapply(FUN = `*`, data2, (1-prop), SIMPLIFY = FALSE))
  return (data.frame(V1 = transformed_data1+transformed_data2))
}