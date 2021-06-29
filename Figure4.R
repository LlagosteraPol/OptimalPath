source("IgraphModel.R")

# -----------------Village node index Detailed----------------------

juneda = 63
soses = 161
menarguens = 197
belloc = 209

# This two lines plots the number of each node in the network
#mtx = matrix(cbind(vertex_attr(g)$V1, vertex_attr(g)$V2), ncol=2)
#plot(g, layout = mtx,window=FALSE, axes=FALSE, vertex.size=1, cex.main=1.25, cex.lab=1.5, cex.axis=0.75)
#----------------------------------------------------------

# The calculation of all paths takes time...

all_shortest_paths11 <- ordered_paths(graph = g, from = soses, to = belloc, edge_param = "distance")
all_shortest_paths21 <- ordered_paths(graph = g, from = juneda, to = menarguens, edge_param = "distance")

all_shortest_paths12 <- ordered_paths(graph = g, from = soses, to = belloc, edge_param = "weight")
all_shortest_paths22 <- ordered_paths(graph = g, from = juneda, to = menarguens, edge_param = "weight")


#----------------------------------PLOT FIGURE 4----------------------------------
pdf("Images/Figure4.pdf",height=12,width=13.5)
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))

print_path_graph(g, all_shortest_paths12[[1]]$path, color='green')
print_path_graph(g, all_shortest_paths11[[1]]$path, color='blue')

print_path_graph(g, all_shortest_paths22[[1]]$path, color='green')
print_path_graph(g, all_shortest_paths21[[1]]$path, color='blue')

dev.off()


#---------------------------------------------------------------------------------