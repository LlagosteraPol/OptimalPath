
library(gridExtra)

rm(list = ls())
source("functions.R")


net_data <- read.csv2('DB/CSV/net_data.csv')
node_data <- read.csv2('DB/CSV/node_data.csv')
crash_data <- read.csv2('DB/CSV/crash_data.csv')

colnames(net_data)[5] <-'density'


vilanova = 225
soses = 161
alcarras = 266
castelldans = 91


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

short_path_vs <- igraph::shortest_paths(graph = g, 
                                        from = igraph::V(g)[vilanova], 
                                        to = igraph::V(g)[soses],
                                        weight = igraph::edge_attr(g, 'distance'),
                                        output = 'both')

nodes_selection_vs <- as.character(short_path_vs$vpath[[1]])
edges_selection_vs <- as.character(short_path_vs$epath[[1]])

short_path_w_int_dens_vs <- igraph::shortest_paths(graph = g, 
                                     from = igraph::V(g)[vilanova], 
                                     to = igraph::V(g)[soses],
                                     weight = igraph::edge_attr(g, 'W(l_i)'),
                                     output = 'both')

nodes_selection_w_vs <- as.character(short_path_w_int_dens_vs$vpath[[1]])
edges_selection_w_vs <- as.character(short_path_w_int_dens_vs$epath[[1]])



short_path_w_int_dens_ac <- igraph::shortest_paths(graph = g, 
                                                   from = igraph::V(g)[alcarras], 
                                                   to = igraph::V(g)[castelldans],
                                                   weight = igraph::edge_attr(g, 'W(l_i)'),
                                                   output = 'both')

nodes_selection_w_ac <- as.character(short_path_w_int_dens_ac$vpath[[1]])
edges_selection_w_ac <- as.character(short_path_w_int_dens_ac$epath[[1]])

short_path_ac <- igraph::shortest_paths(graph = g, 
                                        from = igraph::V(g)[alcarras], 
                                        to = igraph::V(g)[castelldans],
                                        weight = igraph::edge_attr(g, 'distance'),
                                        output = 'both')

nodes_selection_ac <- as.character(short_path_ac$vpath[[1]])
edges_selection_ac <- as.character(short_path_ac$epath[[1]])


plot_safest_vs <- PlotNetwork(g, net_vertices = nodes_selection_w_vs, net_edges = edges_selection_w_vs) + 
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


plot_shortest_vs <- PlotNetwork(g, net_vertices = nodes_selection_vs, net_edges = edges_selection_vs) + 
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


plot_safest_ac <- PlotNetwork(g, net_vertices = nodes_selection_w_ac, net_edges = edges_selection_w_ac) + 
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

plot_shortest_ac <- PlotNetwork(g, net_vertices = nodes_selection_ac, net_edges = edges_selection_ac) + 
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

ggplot2::ggsave(
  filename = "Images/shortest_paths.pdf", 
  plot = gridExtra::marrangeGrob(list(plot_safest_vs, plot_safest_ac, plot_shortest_vs, plot_shortest_ac), 
                                 nrow=2, 
                                 ncol=2,
                                 top=NULL), 
  width = 9, height = 9
)

# pdf("Images/safest_path_vs.pdf",height=6,width=13.5)
# plot_safest_vs
# dev.off()
# 
# pdf("Images/shortest_path_vs.pdf",height=6,width=13.5)
# plot_shortest_vs
# dev.off()
# 
# 
# pdf("Images/safest_path_ac.pdf",height=6,width=13.5)
# plot_safest_ac
# dev.off()
# 
# pdf("Images/shortest_path_ac.pdf",height=6,width=13.5)
# plot_shortest_ac
# dev.off()


# Soses-Belloch
shortest_distance_vs    <- get_k_shortest_paths(graph = g, from = vilanova, to = soses, weight = 'distance', k = 10, show_weight = TRUE)
shortest_intensity_vs   <- get_k_shortest_paths(graph = g, from = vilanova, to = soses, weight = 'intensity', k = 10, show_weight = TRUE)
shortest_density_vs     <- get_k_shortest_paths(graph = g, from = vilanova, to = soses, weight = 'density', k = 10, show_weight = TRUE)
shortest_t_intensity_vs <- get_k_shortest_paths(graph = g, from = vilanova, to = soses, weight = 'T(intensity)', k = 10, show_weight = TRUE)
shortest_t_density_vs   <- get_k_shortest_paths(graph = g, from = vilanova, to = soses, weight = 'T(density)', k = 10, show_weight = TRUE)
shortest_w_int_dens_vs  <- get_k_shortest_paths(graph = g, from = vilanova, to = soses, weight = 'W(l_i)', k = 10, show_weight = TRUE)

# Juneda-Menarguens
shortest_distance_ac    <- get_k_shortest_paths(graph = g, from = alcarras, to = castelldans, weight = 'distance', k = 10, show_weight = TRUE)
shortest_intensity_ac   <- get_k_shortest_paths(graph = g, from = alcarras, to = castelldans, weight = 'intensity', k = 10, show_weight = TRUE)
shortest_density_ac     <- get_k_shortest_paths(graph = g, from = alcarras, to = castelldans, weight = 'density', k = 10, show_weight = TRUE)
shortest_t_intensity_ac <- get_k_shortest_paths(graph = g, from = alcarras, to = castelldans, weight = 'T(intensity)', k = 10, show_weight = TRUE)
shortest_t_density_ac   <- get_k_shortest_paths(graph = g, from = alcarras, to = castelldans, weight = 'T(density)', k = 10, show_weight = TRUE)
shortest_w_int_dens_ac  <- get_k_shortest_paths(graph = g, from = alcarras, to = castelldans, weight = 'W(l_i)', k = 10, show_weight = TRUE)



load(file="DB/RData/all_paths_vs.RData")
load(file="DB/RData/all_paths.RData")
all_paths_vs <- all_simple_paths(g, from = vilanova, to = soses)
all_paths_ac <- all_simple_paths(g, from = alcarras, to = castelldans)

# #-------------------------------------------------------10%------------------------------------------------------------

gdi10 = filter_graph(graph=g, filter=10, weight='distance')
sos_bell_di10 = rate_paths(graph = gdi10, from = vilanova, to = soses)
jun_men_di10 = rate_paths(graph = gdi10, from = alcarras, to = castelldans)

gde10 = filter_graph(graph=g, filter=10, weight='density', paths = all_paths_ac)
sos_bell_de10 = rate_paths(graph = gde10, from = vilanova, to = soses)
jun_men_de10 = rate_paths(graph = gde10, from = alcarras, to = castelldans)


# #-------------------------------------------------------25%------------------------------------------------------------

gdi25 = filter_graph(graph=g, filter=25, weight='distance')
sos_bell_di25 = rate_paths(graph = gdi25, from = vilanova, to = soses)
jun_men_di25 = rate_paths(graph = gdi25, from = alcarras, to = castelldans)

gde25 = filter_graph(graph=g, filter=25, weight='density')
sos_bell_de25 = rate_paths(graph = gde25, from = vilanova, to = soses)
jun_men_de25 = rate_paths(graph = gde25, from = alcarras, to = castelldans)


# #-------------------------------------------------------50%------------------------------------------------------------

gdi50 = filter_graph(graph=g, filter=50, weight='distance')
sos_bell_di50 = rate_paths(graph = gdi50, from = vilanova, to = soses)
jun_men_di50 = rate_paths(graph = gdi50, from = alcarras, to = castelldans)

gde50 = filter_graph(graph=g, filter=50, weight='density')
sos_bell_de50 = rate_paths(graph = gde50, from = vilanova, to = soses)
jun_men_de50 = rate_paths(graph = gde50, from = alcarras, to = castelldans)


# #-------------------------------------------------------75%------------------------------------------------------------

gdi75 = filter_graph(graph=g, filter=75, weight='distance')
sos_bell_di75 = rate_paths(graph = gdi75, from = vilanova, to = soses)
jun_men_di75 = rate_paths(graph = gdi75, from = alcarras, to = castelldans)

gde75 = filter_graph(graph=g, filter=75, weight='density')
sos_bell_de75 = rate_paths(graph = gde75, from = vilanova, to = soses)
jun_men_de75 = rate_paths(graph = gde75, from = alcarras, to = castelldans)