

rm(list = ls())

source("plots.R")
source("functions.R")


net_data <- read.csv2('DB/CSV/net_data.csv')
vertice_data <- read.csv2('DB/CSV/node_data.csv')
crash_data <- read.csv2('DB/CSV/crash_data.csv')

imd <- net_data[,'imd2015']

inverted_imd <- mapply(FUN = `-`, max(imd), imd)

transformed_weights <- weighted_data(net_data[,'intensity'], inverted_imd, 0.5, 0.5)
transformed_accIntensities <- transformed_weights$cov1_comb
transformed_volumes <- transformed_weights$cov2_comb

weighted_segments <- cbind(net_data, transformed_accIntensities)
weighted_segments <- cbind(weighted_segments, transformed_volumes) 

colnames(weighted_segments) <- c("from","to", "distanse", "intensity", "density", "t_intensity", "t_density")

g = graph_from_data_frame(weighted_segments, directed = FALSE, vertices = vertice_data)

PlotNetwork(g, mode = 'intensity')

