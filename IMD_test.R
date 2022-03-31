#devtools::install_github("LlagosteraPol/intensitynet")

rm(list = ls())

library(igraph)

# accidents <- read.csv(file = 'DB/punt_coor_accid_lle.txt', sep = ';')
# data_roads <- read.csv(file = 'DB/punts_id_transectes_lleida_2_pv9.txt', sep=';')
# net_vertices <- read.csv(file = 'DB/punts_coord_id_v9.txt', sep = ';')

accidents <- read.csv(file = 'DB/TXT/accidents_lleida.txt', sep = ';')
data_roads <- read.csv(file = 'DB/CSV/edges_lleida_7.csv', sep=';')
net_vertices <- read.csv(file = 'DB/CSV/nodes_lleida_1.csv', sep = ';')
#net_vertices <- read.csv(file = 'DB/TXT/nodes_lleida.txt', sep = ';')
  
net_df <- data.frame( from = data_roads[, 'FID_1'],
                      to = data_roads[, 'FID_12'],
                      imd = data_roads[, 'IMD2015'])

  
#tst <- union(net_df$from, net_df$to)
# intersect1 <- intersect(data_roads[, 'FID_1'], net_vertices[, 'FID'])
# intersect2 <- intersect(data_roads[, 'FID_12'], net_vertices[, 'FID'])
# vertice_ids <- union(intersect1, intersect2)
# filtered_vertices <- net_vertices[net_vertices$FID %in% vertice_ids, ] 
# write.csv2(filtered_vertices, 'DB/CSV/nodes_lleida_1.csv')

net <- graph_from_data_frame(net_df, vertices = net_vertices[, c('FID', 'x', 'y')])

isolated = which(degree(net)==0)
#net = delete.vertices(net, isolated)


adj_mtx <- as.matrix(as_adjacency_matrix(net))

intnet <- intensitynet::intensitynet(adj_mtx, net_vertices[, c('x3', 'y3')], accidents[, c('x', 'y')])
g <- intnet$graph

intensitynet::PlotHeatmap(intnet)

cmp <- components(g)

intensitynet::PlotHeatmap(intnet, net_vertices = vertex_attr(g, 'name', V(g)[isolated]))
intensitynet::PlotHeatmap(intnet, net_vertices = c(579,713,714,715,716,717,718,719,720,721,722,723,724,725,726,727,728,729,730,731,732,733,734,735,736,737,738,739,740,741,969,998,
                                                   580,714,715,716,717,718,719,720,721,722,723,724,725,726,727,728,729,730,731,732,733,734,735,736,737,738,739,740,741,742,970,999))

intensitynet::PlotHeatmap(intnet, net_vertices = which(cmp$membership == 2))
intensitynet::PlotHeatmap(intnet, net_vertices = which(cmp$membership == 51))
vertex_attr(g, 'name', V(g)[which(cmp$membership == 51)]) 


neighbors(g, '535')

components(g)