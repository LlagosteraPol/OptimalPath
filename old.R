
source("DB_preparation.R")

# Add weights to the edges matrix
weighted_segments <- cbind(Dades_segments, Dades_pesos) 
weighted_segments <- cbind(weighted_segments, Dades_distancies)
weighted_segments <- cbind(weighted_segments, transformed_accIntensities) 
weighted_segments <- cbind(weighted_segments, transformed_distances)
weighted_segments <- cbind(weighted_segments, all_data)

colnames(weighted_segments) <- c("from","to","weight","distance", "t_weight", "t_distance", "all")


vertices_df <- data.frame(names = seq(1, length(Dades_vertex[[1]]), 1), 
                          xcoord = Dades_vertex$V1, 
                          ycoord = Dades_vertex$V2)

segments_df <- data.frame(from = weighted_segments$from,
                          to = weighted_segments$to,
                          weight = weighted_segments$weight,
                          imd = imd,
                          distance = weighted_segments$distance,
                          t_weight = weighted_segments$t_weight,
                          t_distance = weighted_segments$t_distance,
                          all = weighted_segments$all)



g = graph_from_data_frame(segments_df, directed=FALSE, vertices=vertices_df)

checker <- function(g, i){
  PlotNetwork(g, net_vertices = c(igraph::ends(g, igraph::E(g)[i])), net_edges = igraph::E(g)[i])
}


#---------------------------------------------------------------------------------------------------------------------------------