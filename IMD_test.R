#devtools::install_github("LlagosteraPol/intensitynet")

rm(list = ls())

library(igraph)

# accidents <- read.csv(file = 'DB/punt_coor_accid_lle.txt', sep = ';')
# data_roads <- read.csv(file = 'DB/punts_id_transectes_lleida_2_pv9.txt', sep=';')
# net_vertices <- read.csv(file = 'DB/punts_coord_id_v9.txt', sep = ';')

accidents <- read.csv(file = 'DB/punt_coor_accid_lle.txt', sep = ';')
data_roads <- read.csv(file = 'DB/punts_id_transectes_lleida_5_pv10.2.txt', sep=';')
net_vertices <- read.csv(file = 'DB/punts_coord_id_v10.2.txt', sep = ';')
  
net_df <- data.frame( from = data_roads[, 'FID_1'],
                      to = data_roads[, 'FID_12'],
                      imd = data_roads[, 'IMD2015'])

  
#tst <- union(net_df$from, net_df$to)

net <- graph_from_data_frame(net_df, vertices = net_vertices[, c('FID', 'x', 'y')])
adj_mtx <- as.matrix(as_adjacency_matrix(net))

intnet <- intensitynet::intensitynet(adj_mtx, net_vertices[, c('x3', 'y3')], accidents[, c('x', 'y')])
intensitynet::PlotHeatmap(intnet)

g <- intnet$graph
components(g)