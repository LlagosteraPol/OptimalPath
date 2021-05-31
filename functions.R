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