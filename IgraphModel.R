
rm(list = ls())

source("DB_preparation.R")

#----------------------------------------CREATE GRAPH-----------------------------------------

# Add weights to the edges matrix
weighted_segments <- cbind(Dades_segments, Dades_pesos) 
weighted_segments <- cbind(weighted_segments, Dades_distancies)

# TODO: Potser hi ha problema en l'arrodoniment
Dades_totes <- matrix(Dades_pesos$V1 + Dades_distancies$V1) 

weighted_segments <- cbind(weighted_segments, Dades_totes)

colnames(weighted_segments) <- c("from","to","weight","distance", "all")

vertices_df <- data.frame(names=seq(1, length(Dades_vertex[[1]]), 1) , V1 = Dades_vertex$V1, V2 = Dades_vertex$V2)

segments_df <- data.frame(from = weighted_segments$from,
                          to = weighted_segments$to,
                          weight= weighted_segments$weight,
                          distance = weighted_segments$distance,
                          all = weighted_segments$all)

g = graph_from_data_frame(segments_df, directed=FALSE, vertices=vertices_df)


#-----------------------------------------TEST-------------------------------------------------
# E(g)$weight # Check weight
# E(g)$distance # Check distance
# 
# g_df = as_data_frame(g)

#plot(g, vertex.label=NA, vertex.size=2, window=TRUE, axes=TRUE, edge.label = edge_attr(g)$weight, edge.label.cex = 0.5)
#plot(g, vertex.label=NA, vertex.size=2, edge.label = edge_attr(g)$weight, edge.label.cex = 0.5) # Plot with weights
#plot(g, vertex.size=2) # Plot node labels


# Get a matrix with the weights of the shortest paths
# shortest_weight   <-  shortest.paths(g, v=V(g), weights=E(g)$weight)
# shortest_distance <-  shortest.paths(g, v=V(g), weights=E(g)$distance)
#---------------------------------------------------------------------------------------------