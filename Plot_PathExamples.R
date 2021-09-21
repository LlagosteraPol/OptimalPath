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

all_shortest_paths11 <- get_k_shortest_paths(graph = g, from = soses, to = belloc, weight = "distance", k=1)
all_shortest_paths21 <- get_k_shortest_paths(graph = g, from = juneda, to = menarguens, weight = "distance", k=1)

all_shortest_paths12 <- get_k_shortest_paths(graph = g, from = soses, to = belloc, weight = "weight", k=1)
all_shortest_paths22 <- get_k_shortest_paths(graph = g, from = juneda, to = menarguens, weight = "weight", k=1)


all_shortest_paths11 <- get_shortest_paths(graph = g, from = soses, to = belloc, weight = "distance", k=1)
all_shortest_paths21 <- get_shortest_paths(graph = g, from = juneda, to = menarguens, weight = "distance", k=1)

all_shortest_paths12 <- get_shortest_paths(graph = g, from = soses, to = belloc, weight = "weight", k=1)
all_shortest_paths22 <- get_shortest_paths(graph = g, from = juneda, to = menarguens, weight = "weight", k=1)

#--------------------------------PLOT Path Examples--------------------------------
win <- c(minx,maxx,miny,maxy)
pdf("Images/PathExamples.pdf",height=12,width=13.5)
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))

print_path_graph(g, all_shortest_paths12[[1]], color='green')
text(-0.4, -0.4, 'S')
text(0.9, 0.1, 'B')

print_path_graph(g, all_shortest_paths11[[1]], color='blue')
text(-0.4, -0.4, 'S')
text(0.9, 0.1, 'B')

print_path_graph(g, all_shortest_paths22[[1]], color='green')
text(0.65, 0.83, 'M')
text(0.93, -0.3, 'J')

print_path_graph(g, all_shortest_paths21[[1]], color='blue')
text(0.65, 0.83, 'M')
text(0.93, -0.3, 'J')

dev.off()


#---------------------------------------------------------------------------------