

rm(list = ls())
source("functions.R")


net_data <- read.csv2('DB/CSV/net_data.csv')
node_data <- read.csv2('DB/CSV/node_data.csv')
crash_data <- read.csv2('DB/CSV/crash_data.csv')

colnames(net_data)[5] <-'density'


albages = 73
belloch = 209
alguaire = 156
soses = 161


g = PrepareIgraph(net_data = net_data, 
                  node_data = node_data, 
                  cov1 = 'intensity', 
                  cov2 = 'density', 
                  prop = 0.5, 
                  invert_cov1 = FALSE, 
                  invert_cov2 = TRUE)


g_df <- as.data.frame(igraph::as_data_frame(g))


# Heatmap
weight <- 'density'
pdf(paste0("Images/",weight, "_heatmap.pdf"),height=6,width=13.5)
  PlotNetwork(g, mode = weight) + 
    ggplot2::coord_fixed() + 
    ggplot2::scale_y_continuous(name = NULL) + 
    ggplot2::scale_x_continuous(name = NULL) +
    ggplot2::labs(title = NULL, color = weight) +
    ggplot2::theme(legend.title = ggplot2::element_blank())
dev.off()

short_path_sb <- igraph::shortest_paths(graph = g, 
                                        from = igraph::V(g)[albages], 
                                        to = igraph::V(g)[belloch],
                                        weight = igraph::edge_attr(g, 'distance'),
                                        output = 'both')

nodes_selection_ab <- as.character(short_path_sb$vpath[[1]])
edges_selection_ab <- as.character(short_path_sb$epath[[1]])

short_path_w_int_dens_ab <- igraph::shortest_paths(graph = g, 
                                     from = igraph::V(g)[albages], 
                                     to = igraph::V(g)[belloch],
                                     weight = igraph::edge_attr(g, 'W(l_i)'),
                                     output = 'both')

nodes_selection_w_ab <- as.character(short_path_w_int_dens_sb$vpath[[1]])
edges_selection_w_ab <- as.character(short_path_w_int_dens_sb$epath[[1]])

short_path_w_int_dens_as <- igraph::shortest_paths(graph = g, 
                                                   from = igraph::V(g)[alguaire], 
                                                   to = igraph::V(g)[soses],
                                                   weight = igraph::edge_attr(g, 'W(l_i)'),
                                                   output = 'both')

nodes_selection_w_as <- as.character(short_path_w_int_dens_jm$vpath[[1]])
edges_selection_w_as <- as.character(short_path_w_int_dens_jm$epath[[1]])

short_path_as <- igraph::shortest_paths(graph = g, 
                                        from = igraph::V(g)[alguaire], 
                                        to = igraph::V(g)[soses],
                                        weight = igraph::edge_attr(g, 'distance'),
                                        output = 'both')

nodes_selection_as <- as.character(short_path_jm$vpath[[1]])
edges_selection_as <- as.character(short_path_jm$epath[[1]])

pdf("Images/safest_path_ab.pdf",height=6,width=13.5)
PlotNetwork(g, net_vertices = nodes_selection_w_ab, net_edges = edges_selection_w_ab) + 
  ggplot2::coord_fixed() + 
  ggplot2::scale_y_continuous(name = NULL) + 
  ggplot2::scale_x_continuous(name = NULL) +
  ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x  = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y  = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank())
dev.off()


pdf("Images/safest_path_as.pdf",height=6,width=13.5)
PlotNetwork(g, net_vertices = nodes_selection_w_as, net_edges = edges_selection_w_as) + 
  ggplot2::coord_fixed() + 
  ggplot2::scale_y_continuous(name = NULL) + 
  ggplot2::scale_x_continuous(name = NULL) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x  = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y  = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(), 
    panel.grid.minor = ggplot2::element_blank())
dev.off()

pdf("Images/shortest_path_ab.pdf",height=6,width=13.5)
PlotNetwork(g, net_vertices = nodes_selection_ab, net_edges = edges_selection_ab) + 
  ggplot2::coord_fixed() + 
  ggplot2::scale_y_continuous(name = NULL) + 
  ggplot2::scale_x_continuous(name = NULL) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x  = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y  = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(), 
    panel.grid.minor = ggplot2::element_blank())
dev.off()

pdf("Images/shortest_path_as.pdf",height=6,width=13.5)
PlotNetwork(g, net_vertices = nodes_selection_as, net_edges = edges_selection_as) + 
  ggplot2::coord_fixed() + 
  ggplot2::scale_y_continuous(name = NULL) + 
  ggplot2::scale_x_continuous(name = NULL) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x  = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y  = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(), 
    panel.grid.minor = ggplot2::element_blank())
dev.off()


# Soses-Belloch
shortest_distance_ab    <- get_k_shortest_paths(graph = g, from = albages, to = belloch, weight = 'distance', k = 10, show_weight = TRUE)
shortest_intensity_ab   <- get_k_shortest_paths(graph = g, from = albages, to = belloch, weight = 'intensity', k = 10, show_weight = TRUE)
shortest_density_ab     <- get_k_shortest_paths(graph = g, from = albages, to = belloch, weight = 'density', k = 10, show_weight = TRUE)
shortest_t_intensity_ab <- get_k_shortest_paths(graph = g, from = albages, to = belloch, weight = 'T(intensity)', k = 10, show_weight = TRUE)
shortest_t_density_ab   <- get_k_shortest_paths(graph = g, from = albages, to = belloch, weight = 'T(density)', k = 10, show_weight = TRUE)
shortest_w_int_dens_ab  <- get_k_shortest_paths(graph = g, from = albages, to = belloch, weight = 'W(l_i)', k = 10, show_weight = TRUE)

# Juneda-Menarguens
shortest_distance_as    <- get_k_shortest_paths(graph = g, from = alguaire, to = soses, weight = 'distance', k = 10, show_weight = TRUE)
shortest_intensity_as   <- get_k_shortest_paths(graph = g, from = alguaire, to = soses, weight = 'intensity', k = 10, show_weight = TRUE)
shortest_density_as     <- get_k_shortest_paths(graph = g, from = alguaire, to = soses, weight = 'density', k = 10, show_weight = TRUE)
shortest_t_intensity_as <- get_k_shortest_paths(graph = g, from = alguaire, to = soses, weight = 'T(intensity)', k = 10, show_weight = TRUE)
shortest_t_density_as   <- get_k_shortest_paths(graph = g, from = alguaire, to = soses, weight = 'T(density)', k = 10, show_weight = TRUE)
shortest_w_int_dens_as  <- get_k_shortest_paths(graph = g, from = alguaire, to = soses, weight = 'W(l_i)', k = 10, show_weight = TRUE)



load(file="DB/RData/all_paths_sb.RData")
load(file="DB/RData/all_paths.RData")
all_paths_ab <- all_simple_paths(g, from = albages, to = belloch)
all_paths_as <- all_simple_paths(g, from = alguaire, to = soses)

# #-------------------------------------------------------10%------------------------------------------------------------

gd10 = filter_graph(graph=g, filter=10, weight='distance', paths = all_paths_sb)
sos_bell_d10 = rate_paths(graph = gd10, from = soses, to = belloc)
jun_men_d10 = rate_paths(graph = gd10, from = juneda, to = menarguens)

gi10 = filter_graph(graph=g, filter=10, weight='density', paths = all_paths_jm)
sos_bell_i10 = rate_paths(graph = gi10, from = soses, to = belloc)
jun_men_i10 = rate_paths(graph = gi10, from = juneda, to = menarguens)


# #-------------------------------------------------------25%------------------------------------------------------------

gd25 = filter_graph(graph=g, filter=25, weight='distance', paths = all_paths_sb)
sos_bell_d25 = rate_paths(graph = gd25, from = soses, to = belloc)
jun_men_d25 = rate_paths(graph = gd25, from = juneda, to = menarguens)

gi25 = filter_graph(graph=g, filter=25, weight='density')
sos_bell_i25 = rate_paths(graph = gi25, from = soses, to = belloc, paths = all_paths_jm)
jun_men_i25 = rate_paths(graph = gi25, from = juneda, to = menarguens)


# #-------------------------------------------------------50%------------------------------------------------------------

gd50 = filter_graph(graph=g, filter=50, weight='distance', paths = all_paths_sb)
sos_bell_d50 = rate_paths(graph = gd50, from = soses, to = belloc)
jun_men_d50 = rate_paths(graph = gd50, from = juneda, to = menarguens)

gi50 = filter_graph(graph=g, filter=50, weight='density')
sos_bell_i50 = rate_paths(graph = gi50, from = soses, to = belloc, paths = all_paths_jm)
jun_men_i50 = rate_paths(graph = gi50, from = juneda, to = menarguens)


# #-------------------------------------------------------75%------------------------------------------------------------

gd75 = filter_graph(graph=g, filter=75, weight='distance', paths = all_paths_sb)
sos_bell_d75 = rate_paths(graph = gd75, from = soses, to = belloc)
jun_men_d75 = rate_paths(graph = gd75, from = juneda, to = menarguens)

gi75 = filter_graph(graph=g, filter=75, weight='density', paths = all_paths_sb)
sos_bell_i75 = rate_paths(graph = gi75, from = soses, to = belloc, paths = all_paths_jm)
jun_men_i75 = rate_paths(graph = gi75, from = juneda, to = menarguens)


# -----------------Plot test 1--------------------------

mode <- 'density'

edge_int <- igraph::edge_attr(g, mode)
data_df <- data.frame(xcoord = igraph::vertex_attr(g, 'xcoord'), 
                      ycoord = igraph::vertex_attr(g, 'ycoord'))

node_coords <- data.frame(xcoord = igraph::vertex_attr(g)$xcoord, ycoord = igraph::vertex_attr(g)$ycoord)
rownames(node_coords) <- igraph::vertex_attr(g)$name
#get edges, which are pairs of node IDs
edgelist <- igraph::get.edgelist(g)
#convert to a four column edge data frame with source and destination coordinates
edges_df <- data.frame(node_coords[edgelist[,1],], node_coords[edgelist[,2],])
colnames(edges_df) <- c("xcoord1","ycoord1","xcoord2","ycoord2")


hplot <- ggplot2::ggplot(data_df, ggplot2::aes_string(x = 'xcoord', y = 'ycoord')) +
  viridis::scale_color_viridis(option = 'H') +
  ggplot2::labs(title = NULL,#paste0(mode, ' Heatmap\n'),
                color = mode) +
  ggplot2::geom_segment(ggplot2::aes_string(x = 'xcoord1', y = 'ycoord1', 
                                            xend = 'xcoord2', yend = 'ycoord2',
                                            colour = 'edge_int'),
                        data = edges_df,
                        size = 0.8) +
  ggplot2::geom_point(shape = 19,
                      size = 1.7,
                      colour="gray") +
  ggplot2::scale_y_continuous(name = NULL) + 
  ggplot2::scale_x_continuous(name = NULL) + 
  ggplot2::theme_bw() +
  ggplot2::theme(legend.title = ggplot2::element_text(face = "bold"),
                 plot.title = ggplot2::element_text( size = 14,
                                                     face = "bold",
                                                     hjust = 0.5) )

hplot





# -----------------Plot test 2--------------------------
df <- expand.grid(X1 = 1:10, X2 = 1:10)
df$value <- df$X1 * df$X2

p1 <- ggplot2::ggplot(df, ggplot2::aes(X1, X2)) + ggplot2::geom_tile(ggplot2::aes(fill = value))
p2 <- p1 + ggplot2::geom_point(ggplot2::aes(size = value))

# Basic form
p1 + ggplot2::scale_fill_continuous(guide = "colourbar")
p1  + ggplot2::guides(fill =  ggplot2::guide_colourbar(barwidth = 0.5, barheight = 10)) +  viridis::scale_color_viridis(option = 'H')



