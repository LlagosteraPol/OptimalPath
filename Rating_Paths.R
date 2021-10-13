#----------------------------------------------------------Get info about the paths-------------------------------------------------------

#all_shortest_paths <- best_paths(graph = g, from = vilanova, to = seros, weight = "distance")
#top_shortest_paths <- all_shortest_paths[1:10]

#all_safest_paths <- best_paths(graph = g, from = vilanova, to = seros, weight = "weight")
#top_safest_paths <- all_safest_paths[1:10]

#best<- best_paths(graph = g, from = "1", to = "15", weight = "all")
#top_paths <- best[1:10]
  
  source("functions.R")
  source("IgraphModel.R")
  #filtered_paths <- filter_paths(graph = g, from = alcarras, to = albages, edge_param = "distance", filter = 10000)
  
  
  juneda = 63
  soses = 161
  menarguens = 197
  belloc = 209
  
  #---------------------------------------------------------------------------------
  #infopaths <- paths_info(graph = g, from = soses, to = belloc)
  sos_bell <- rate_paths(graph = g, from = soses, to = belloc)
  sos_bell_distance_ordered <- sos_bell[order(sapply(sos_bell,'[[',6))]
  sos_bell_weight_ordered <- sos_bell[order(sapply(sos_bell,'[[',7))]
  sos_bell_all_ordered <- sos_bell[order(sapply(sos_bell,'[[',8))]
  
  jun_men <- rate_paths(graph = g, from = juneda, to = menarguens)
  jun_men_distance_ordered <- jun_men[order(sapply(jun_men,'[[',6))]
  jun_men_weight_ordered <- jun_men[order(sapply(jun_men,'[[',7))]
  jun_men_all_ordered <- jun_men[order(sapply(jun_men,'[[',8))]
  #-----------------------------------------------------------------------------------------------------------------------------------------