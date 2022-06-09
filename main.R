
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


# Heatmaps
load(file="DB/RData/Linear_pixel_final_18_Juny_2021_ultima_copia.RData")
ww_df <- as.data.frame(ww)

difusor_heatmap <- ggplot2::ggplot(ww_df, ggplot2::aes_string(x = 'xc', y = 'yc')) +
                    viridis::scale_color_viridis(option = 'H') +
                    ggplot2::labs(title = NULL,color = ww_df$values) +
                    ggplot2::geom_point(shape = 15, size = 0.5,
                                        ggplot2::aes(color = values) )+
                    ggplot2::scale_y_continuous(name = NULL, 
                                                limits = c(4587700, 4627450),
                                                breaks = c(4587700, 4595650, 4603600, 4611550, 4619500, 4627450),
                                                labels = c('4587700' = '100', '4595650' = '108', '4603600' = '116', '4611550' = '124', '4619500' = '132', '4627450' = '140')) + 
                    ggplot2::scale_x_continuous(name = NULL,
                                                limits = c(278950, 318500),
                                                breaks = c(278950, 286860, 294770, 302680, 310590, 318500),
                                                labels = c('278950' = 18, '286860' = 26, '294770' = 34, '302680' = 42, '310590' = 50, '318500' = 58)) +
                    ggplot2::theme_bw() +
                    ggplot2::theme(legend.title = ggplot2::element_text(face = "bold"),
                                   plot.title = ggplot2::element_text( size = 14,
                                                                       face = "bold",
                                                                       hjust = 0.5) )+
                    ggplot2::coord_fixed() +
                    ggplot2::theme(legend.title = ggplot2::element_blank())


g = PrepareIgraph(net_data = net_data, 
                  node_data = node_data, 
                  cov1 = 'intensity', 
                  cov2 = 'density', 
                  prop = 0.5, 
                  invert_cov1 = FALSE, 
                  invert_cov2 = TRUE)


g_df <- as.data.frame(igraph::as_data_frame(g))

net_plot <- PlotNetwork(g) + 
            ggplot2::geom_point(shape = 19, 
                                size = 0.7,
                                colour="black") +
            ggplot2::coord_fixed() + 
            ggplot2::scale_y_continuous(name = 'Km', 
                                        limits = c(4587700, 4627450),
                                        breaks = c(4587700, 4595650, 4603600, 4611550, 4619500, 4627450),
                                        labels = c('4587700' = '100', '4595650' = '108', '4603600' = '116', '4611550' = '124', '4619500' = '132', '4627450' = '140')) + 
            ggplot2::scale_x_continuous(name = 'Km',
                                        limits = c(278950, 318500),
                                        breaks = c(278950, 286860, 294770, 302680, 310590, 318500),
                                        labels = c('278950' = 18, '286860' = 26, '294770' = 34, '302680' = 42, '310590' = 50, '318500' = 58)) +
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                           panel.grid.minor = ggplot2::element_blank(),
                           text = ggplot2::element_text(size=20))

pdf(paste0("Images/plain_network.pdf"),height=6,width=13.5)
net_plot
dev.off()

intensity_heatmap <- PlotNetwork(g, mode = 'intensity') + 
                     ggplot2::geom_point(shape = 19, 
                                         size = 0.7,
                                         colour="gray") +
                     ggplot2::coord_fixed() + 
                     ggplot2::scale_y_continuous(name = NULL, 
                                                 limits = c(4587700, 4627450),
                                                 breaks = c(4587700, 4595650, 4603600, 4611550, 4619500, 4627450),
                                                 labels = c('4587700' = '100', '4595650' = '108', '4603600' = '116', '4611550' = '124', '4619500' = '132', '4627450' = '140')) + 
                     ggplot2::scale_x_continuous(name = NULL,
                                                 limits = c(278950, 318500),
                                                 breaks = c(278950, 286860, 294770, 302680, 310590, 318500),
                                                 labels = c('278950' = 18, '286860' = 26, '294770' = 34, '302680' = 42, '310590' = 50, '318500' = 58)) +
                     ggplot2::labs(title = NULL, color = 'intensity') +
                     ggplot2::theme(legend.title = ggplot2::element_blank())

density_heatmap <- PlotNetwork(g, mode = 'density') + 
                   ggplot2::geom_point(shape = 19, 
                                       size = 0.7,
                                       colour="gray") +
                     ggplot2::coord_fixed() + 
                     ggplot2::scale_y_continuous(name = NULL, 
                                                 limits = c(4587700, 4627450),
                                                 breaks = c(4587700, 4595650, 4603600, 4611550, 4619500, 4627450),
                                                 labels = c('4587700' = '100', '4595650' = '108', '4603600' = '116', '4611550' = '124', '4619500' = '132', '4627450' = '140')) + 
                     ggplot2::scale_x_continuous(name = NULL,
                                                 limits = c(278950, 318500),
                                                 breaks = c(278950, 286860, 294770, 302680, 310590, 318500),
                                                 labels = c('278950' = 18, '286860' = 26, '294770' = 34, '302680' = 42, '310590' = 50, '318500' = 58)) +
                     ggplot2::labs(title = NULL, color = 'density') +
                     ggplot2::theme(legend.title = ggplot2::element_blank()) + 
                     ggplot2::geom_point(shape = 19,
                                        size = 0.6,
                                        colour="gray")

wli_heatmap <- PlotNetwork(g, mode = 'W(l_i)') + 
               ggplot2::geom_point(shape = 19, 
                                   size = 0.3,
                                   colour="gray") +
                 ggplot2::coord_fixed() + 
                 ggplot2::scale_y_continuous(name = NULL, 
                                             limits = c(4587700, 4627450),
                                             breaks = c(4587700, 4595650, 4603600, 4611550, 4619500, 4627450),
                                             labels = c('4587700' = '100', '4595650' = '108', '4603600' = '116', '4611550' = '124', '4619500' = '132', '4627450' = '140')) + 
                 ggplot2::scale_x_continuous(name = NULL,
                                             limits = c(278950, 318500),
                                             breaks = c(278950, 286860, 294770, 302680, 310590, 318500),
                                             labels = c('278950' = 18, '286860' = 26, '294770' = 34, '302680' = 42, '310590' = 50, '318500' = 58)) +
                 ggplot2::labs(title = NULL, color = 'W(l_i)') +
                 ggplot2::theme(legend.title = ggplot2::element_blank())

ggplot2::ggsave(
  filename = "Images/heatmaps_4.pdf",
  plot = gridExtra::marrangeGrob(list(difusor_heatmap, density_heatmap, intensity_heatmap, wli_heatmap ),
                                 nrow=2,
                                 ncol=2,
                                 top=NULL),
  width = 10, height = 7
)

ggplot2::ggsave(
  filename = "Images/heatmaps_4_2.pdf", 
  plot = gridExtra::grid.arrange(difusor_heatmap, intensity_heatmap, density_heatmap, wli_heatmap, layout_matrix = rbind(c(1,2),c(3,4))), 
  width = 10, height = 7
)

pdf(paste0("Images/intensity_heatmap.pdf"),height=6,width=13.5)
intensity_heatmap
dev.off()

pdf(paste0("Images/density_heatmap.pdf"),height=6,width=13.5)
density_heatmap
dev.off()

pdf(paste0("Images/wli_heatmap.pdf"),height=6,width=13.5)
wli_heatmap
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
                    ggplot2::scale_y_continuous(name = NULL, 
                                                limits = c(4587700, 4627450),
                                                breaks = c(4587700, 4595650, 4603600, 4611550, 4619500, 4627450),
                                                labels = c('4587700' = '100', '4595650' = '108', '4603600' = '116', '4611550' = '124', '4619500' = '132', '4627450' = '140')) + 
                    ggplot2::scale_x_continuous(name = NULL,
                                                limits = c(278950, 318500),
                                                breaks = c(278950, 286860, 294770, 302680, 310590, 318500),
                                                labels = c('278950' = 18, '286860' = 26, '294770' = 34, '302680' = 42, '310590' = 50, '318500' = 58)) +
                                      ggplot2::theme(
                                        axis.title.x = ggplot2::element_blank(),
                                        axis.title.y = ggplot2::element_blank(),
                                        panel.grid.major = ggplot2::element_blank(), 
                                        panel.grid.minor = ggplot2::element_blank())


plot_shortest_vs <- PlotNetwork(g, net_vertices = nodes_selection_vs, net_edges = edges_selection_vs) + 
                      ggplot2::coord_fixed() + 
                      ggplot2::scale_y_continuous(name = NULL, 
                                                  limits = c(4587700, 4627450),
                                                  breaks = c(4587700, 4595650, 4603600, 4611550, 4619500, 4627450),
                                                  labels = c('4587700' = '100', '4595650' = '108', '4603600' = '116', '4611550' = '124', '4619500' = '132', '4627450' = '140')) + 
                      ggplot2::scale_x_continuous(name = NULL,
                                                  limits = c(278950, 318500),
                                                  breaks = c(278950, 286860, 294770, 302680, 310590, 318500),
                                                  labels = c('278950' = 18, '286860' = 26, '294770' = 34, '302680' = 42, '310590' = 50, '318500' = 58)) +
                      ggplot2::theme(
                        axis.title.x = ggplot2::element_blank(),
                        axis.title.y = ggplot2::element_blank(),
                        panel.grid.major = ggplot2::element_blank(), 
                        panel.grid.minor = ggplot2::element_blank())


plot_safest_ac <- PlotNetwork(g, net_vertices = nodes_selection_w_ac, net_edges = edges_selection_w_ac) + 
                    ggplot2::coord_fixed() + 
                    ggplot2::scale_y_continuous(name = NULL, 
                                                limits = c(4587700, 4627450),
                                                breaks = c(4587700, 4595650, 4603600, 4611550, 4619500, 4627450),
                                                labels = c('4587700' = '100', '4595650' = '108', '4603600' = '116', '4611550' = '124', '4619500' = '132', '4627450' = '140')) + 
                    ggplot2::scale_x_continuous(name = NULL,
                                                limits = c(278950, 318500),
                                                breaks = c(278950, 286860, 294770, 302680, 310590, 318500),
                                                labels = c('278950' = 18, '286860' = 26, '294770' = 34, '302680' = 42, '310590' = 50, '318500' = 58)) +
                    ggplot2::theme(
                      axis.title.x = ggplot2::element_blank(),
                      axis.title.y = ggplot2::element_blank(),
                      panel.grid.major = ggplot2::element_blank(), 
                      panel.grid.minor = ggplot2::element_blank())

plot_shortest_ac <- PlotNetwork(g, net_vertices = nodes_selection_ac, net_edges = edges_selection_ac) + 
                      ggplot2::coord_fixed() + 
                      ggplot2::scale_y_continuous(name = NULL, 
                                                  limits = c(4587700, 4627450),
                                                  breaks = c(4587700, 4595650, 4603600, 4611550, 4619500, 4627450),
                                                  labels = c('4587700' = '100', '4595650' = '108', '4603600' = '116', '4611550' = '124', '4619500' = '132', '4627450' = '140')) + 
                      ggplot2::scale_x_continuous(name = NULL,
                                                  limits = c(278950, 318500),
                                                  breaks = c(278950, 286860, 294770, 302680, 310590, 318500),
                                                  labels = c('278950' = 18, '286860' = 26, '294770' = 34, '302680' = 42, '310590' = 50, '318500' = 58)) +
                      ggplot2::theme(
                        axis.title.x = ggplot2::element_blank(),
                        axis.title.y = ggplot2::element_blank(),
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


# Vilanova-Soses
shortest_distance_vs    <- get_k_shortest_paths(graph = g, from = vilanova, to = soses, weight = 'distance', k = 10, show_weight = TRUE)
shortest_intensity_vs   <- get_k_shortest_paths(graph = g, from = vilanova, to = soses, weight = 'intensity', k = 10, show_weight = TRUE)
shortest_density_vs     <- get_k_shortest_paths(graph = g, from = vilanova, to = soses, weight = 'density', k = 10, show_weight = TRUE)
shortest_t_intensity_vs <- get_k_shortest_paths(graph = g, from = vilanova, to = soses, weight = 'T(intensity)', k = 10, show_weight = TRUE)
shortest_t_density_vs   <- get_k_shortest_paths(graph = g, from = vilanova, to = soses, weight = 'T(density)', k = 10, show_weight = TRUE)
shortest_w_int_dens_vs  <- get_k_shortest_paths(graph = g, from = vilanova, to = soses, weight = 'W(l_i)', k = 10, show_weight = TRUE)

# Alcarras-Castelldans
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
vs_di10 = rate_paths(graph = gdi10, from = vilanova, to = soses)
ac_di10 = rate_paths(graph = gdi10, from = alcarras, to = castelldans)

gde10 = filter_graph(graph=g, filter=10, weight='density')
vs_de10 = rate_paths(graph = gde10, from = vilanova, to = soses)
ac_de10 = rate_paths(graph = gde10, from = alcarras, to = castelldans)

gw10 = filter_graph(graph=g, filter=10, weight='W(l_i)')
vs_w10 = rate_paths(graph = gw10, from = vilanova, to = soses)
ac_w10 = rate_paths(graph = gw10, from = alcarras, to = castelldans)

# #-------------------------------------------------------20%------------------------------------------------------------

gdi20 = filter_graph(graph=g, filter=20, weight='distance')
vs_di20 = rate_paths(graph = gdi20, from = vilanova, to = soses)
ac_di20 = rate_paths(graph = gdi20, from = alcarras, to = castelldans)

gde20 = filter_graph(graph=g, filter=20, weight='density')
vs_de20 = rate_paths(graph = gde20, from = vilanova, to = soses)
ac_de20 = rate_paths(graph = gde20, from = alcarras, to = castelldans)

gw20 = filter_graph(graph=g, filter=20, weight='W(l_i)')
vs_w20 = rate_paths(graph = gw20, from = vilanova, to = soses)
ac_w20 = rate_paths(graph = gw20, from = alcarras, to = castelldans)

# #-------------------------------------------------------25%------------------------------------------------------------

gdi25 = filter_graph(graph=g, filter=25, weight='distance')
vs_di25 = rate_paths(graph = gdi25, from = vilanova, to = soses)
ac_di25 = rate_paths(graph = gdi25, from = alcarras, to = castelldans)

gde25 = filter_graph(graph=g, filter=25, weight='density')
vs_de25 = rate_paths(graph = gde25, from = vilanova, to = soses)
ac_de25 = rate_paths(graph = gde25, from = alcarras, to = castelldans)

gw25 = filter_graph(graph=g, filter=25, weight='W(l_i)')
vs_w25 = rate_paths(graph = gw25, from = vilanova, to = soses)
ac_w25 = rate_paths(graph = gw25, from = alcarras, to = castelldans)

# #-------------------------------------------------------40%------------------------------------------------------------

gdi40 = filter_graph(graph=g, filter=40, weight='distance')
vs_di40 = rate_paths(graph = gdi40, from = vilanova, to = soses)
ac_di40 = rate_paths(graph = gdi40, from = alcarras, to = castelldans)

gde40 = filter_graph(graph=g, filter=40, weight='density')
vs_de40 = rate_paths(graph = gde40, from = vilanova, to = soses)
ac_de40 = rate_paths(graph = gde40, from = alcarras, to = castelldans)

gw40 = filter_graph(graph=g, filter=40, weight='W(l_i)')
vs_w40 = rate_paths(graph = gw40, from = vilanova, to = soses)
ac_w40 = rate_paths(graph = gw40, from = alcarras, to = castelldans)


# #-------------------------------------------------------50%------------------------------------------------------------

gdi50 = filter_graph(graph=g, filter=50, weight='distance')
vs_di50 = rate_paths(graph = gdi50, from = vilanova, to = soses)
ac_di50 = rate_paths(graph = gdi50, from = alcarras, to = castelldans)

gde50 = filter_graph(graph=g, filter=50, weight='density')
vs_de50 = rate_paths(graph = gde50, from = vilanova, to = soses)
ac_de50 = rate_paths(graph = gde50, from = alcarras, to = castelldans)

gw50 = filter_graph(graph=g, filter=50, weight='W(l_i)')
vs_w50 = rate_paths(graph = gw50, from = vilanova, to = soses)
ac_w50 = rate_paths(graph = gw50, from = alcarras, to = castelldans)

# #-------------------------------------------------------60%------------------------------------------------------------

gdi60 = filter_graph(graph=g, filter=60, weight='distance')
vs_di60 = rate_paths(graph = gdi60, from = vilanova, to = soses)
ac_di60 = rate_paths(graph = gdi60, from = alcarras, to = castelldans)

gde60 = filter_graph(graph=g, filter=60, weight='density')
vs_de60 = rate_paths(graph = gde60, from = vilanova, to = soses)
ac_de60 = rate_paths(graph = gde60, from = alcarras, to = castelldans)

gw60 = filter_graph(graph=g, filter=60, weight='W(l_i)')
vs_w60 = rate_paths(graph = gw60, from = vilanova, to = soses)
ac_w60 = rate_paths(graph = gw60, from = alcarras, to = castelldans)


# #-------------------------------------------------------75%------------------------------------------------------------

gdi75 = filter_graph(graph=g, filter=75, weight='distance')
vs_di75 = rate_paths(graph = gdi75, from = vilanova, to = soses)
ac_di75 = rate_paths(graph = gdi75, from = alcarras, to = castelldans)

gde75 = filter_graph(graph=g, filter=75, weight='density')
vs_de75 = rate_paths(graph = gde75, from = vilanova, to = soses)
ac_de75 = rate_paths(graph = gde75, from = alcarras, to = castelldans)

gw75 = filter_graph(graph=g, filter=75, weight='W(l_i)')
vs_w75 = rate_paths(graph = gw75, from = vilanova, to = soses)
ac_w75 = rate_paths(graph = gw75, from = alcarras, to = castelldans)


# #-------------------------------------------------------80%------------------------------------------------------------

gdi80 = filter_graph(graph=g, filter=80, weight='distance')
vs_di80 = rate_paths(graph = gdi80, from = vilanova, to = soses)
ac_di80 = rate_paths(graph = gdi80, from = alcarras, to = castelldans)

gde80 = filter_graph(graph=g, filter=80, weight='density')
vs_de80 = rate_paths(graph = gde80, from = vilanova, to = soses)
ac_de80 = rate_paths(graph = gde80, from = alcarras, to = castelldans)

gw80 = filter_graph(graph=g, filter=80, weight='W(l_i)')
vs_w80 = rate_paths(graph = gw80, from = vilanova, to = soses)
ac_w80 = rate_paths(graph = gw80, from = alcarras, to = castelldans)

# Get the maximum W(l_i) from all possible paths between Vilanova and Soses
max_wli <- 0
for(path in all_paths_vs){
  tmp <- max(igraph::edge_attr(g, 'W(l_i)', index = path))
  
  if(tmp > max_wli) max_wli <- tmp
}




