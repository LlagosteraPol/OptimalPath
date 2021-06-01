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

paths_info <- function(graph, from, to){
  all_paths <- all_simple_paths(g, from=from, to=to)
  
  ipaths <- list()
  
  for (path in all_paths){
    
    distance_sum <- sum(E(g, path = unlist(path))$distance)

    weight_sum <- sum(E(g, path = unlist(path))$weight)

    ipaths[[length(ipaths)+1]] <- list(from = from, to=to, path = as.numeric(unlist(as_ids(path))), distance = distance_sum, weight = weight_sum)
  }
  return(ipaths)
}

weight_order <- function(graph, from, to, type){
  ipaths <- paths_info(graph = graph, from = from, to=to)
  if(type == "distance"){
    return(ipaths[order(sapply(ipaths,'[[',4))])
  }
  else if (type == "weight"){
    return(ipaths[order(sapply(ipaths,'[[',5))])
  }

  return(ipaths)
}

rate_paths <- function(graph, from, to){
  ipaths <- paths_info(graph = graph, from = from, to=to)
  #distance_order <- weight_order(graph = graph, from = from, to=to, type="distance")
  #weight_order <-  weight_order(graph = graph, from = from, to=to, type="weight")
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
  
  return(ipaths)
}

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
best_paths <- function(graph, from, to, weight='weight'){
  all_paths <- all_simple_paths(g, from=from, to=to)
  
  ordered_paths <- list()
  
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
    ordered_paths[[length(ordered_paths)+1]] <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
    #if(length(top_n) < n){
    #  top_n[[length(top_n)+1]] <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
    #}
    #else{
    #  top_n <- top_n[order(sapply(top_n,'[[',1))]
      
    #  if(weight_sum < top_n[[1]]$weight){
    #    top_n[[1]] <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
    #  }
    #}
  }
  return(ordered_paths[order(sapply(ordered_paths,'[[',1))])
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