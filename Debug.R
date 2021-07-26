
source("functions.R")

#load(file="DB/RData/JunMen_allPaths_18_Jul_2021.RData")

juneda = 63
soses = 161
menarguens = 197
belloc = 209


max_distance  = max(E(g)$distance)
max_accidents = max(E(g)$weight)
#sos_bell_paths <- all_simple_paths(g, from=soses, to=belloc)


filter_paths_test <- function(graph, from, to, weight, filters, paths = NULL){
  paths_ordered <- vector(mode="list", length=length(filters))
  names(paths_ordered) <- filters
  filters <- as.numeric(unlist(filters)) # transform strings to integuers
  is_forbiden <- FALSE
  
  if(weight == 'distance'){
    max_parameter  = max(E(g)$distance)
  }else{
    max_parameter = max(E(g)$weight)
  }
  
  if (is.null(paths)){
    paths <- all_simple_paths(g, from=from, to=to)
  }
  
  
  for(path in paths){
    weight_sum <- 0
    is_forbiden <- TRUE
    filters_idx <- 1

    for(percent in filters){
      if(weight == 'distance'){
        if(max(E(g, path = unlist(path))$distance) < (max_parameter*(percent/100))){
          weight_sum <- sum(E(g, path = unlist(path))$distance)
          is_forbiden <- FALSE
          break
        }
      }else{
        if(max(E(g, path = unlist(path))$weight) < (max_parameter*(percent/100))){
          weight_sum <- sum(E(g, path = unlist(path))$weight)
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
  }
  #return(list(paths = paths_ordered[order(sapply(paths_ordered,'[[',1))]))
  return(paths_ordered)
}

filtered_paths <- filter_paths_test(graph=g, from=soses, to=menarguens, weight='distance', filters=c(10,25,50,75), paths = jun_men_paths)