

library(ggplot2)
library(shp2graph)

# --------------Test shp2graph--------- 
data(ORN)
rtNEL1 <-readshpnw(ORN.nt)
igr<-nel2igraph(rtNEL1[[2]],rtNEL1[[3]],weight=rtNEL1[[5]][[1]])
# -------------------------------------

accidents_df <- read.csv(file = "DB/AccidentsAnimals_1014_CatPenins.csv", sep = ';')
spatial_lines_df <- rgdal::readOGR("DB/SHP/Lleida_complex.shp" , stringsAsFactors = F)
net_data <- shp2graph::readshpnw(spatial_lines_df)

#g <- shp2graph::nel2igraph(net_data[[2]], net_data[[3]], weight = net_data[[5]][[6]])
g <- shp2graph::nel2igraph(net_data[[2]], net_data[[3]])

adj_mtx <- as.matrix(igraph::as_adjacency_matrix(g))
accidents <- cbind(accidents_df[[3]], accidents_df[[4]])
#node_coords <- cbind(net_data[[6]], net_data[[7]])
node_coords <- cbind(igraph::vertex_attr(g)[[1]], igraph::vertex_attr(g)[[2]])

intnet <- intensitynet::intensitynet(adj_mtx, node_coords, accidents)
intensitynet::PlotHeatmap(intnet)



map <- ggplot() + geom_polygon(data = spatial_lines_df, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
map + theme_void()