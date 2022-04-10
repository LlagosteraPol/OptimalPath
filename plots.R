

PlotNetwork <- function(g, net_vertices = NULL, net_edges = NULL, high_size = 1, events = NULL, alpha = 1, ...){
  
  data_df <- data.frame(xcoord = igraph::vertex_attr(g, 'xcoord'), 
                        ycoord = igraph::vertex_attr(g, 'ycoord'))
  
  highlighted_df <- data_df[as.numeric(net_vertices),]
  
  
  node_coords <- data.frame(xcoord = igraph::vertex_attr(g)$xcoord, ycoord = igraph::vertex_attr(g)$ycoord)
  rownames(node_coords) <- igraph::vertex_attr(g)$name
  #get edges, which are pairs of node IDs
  edgelist <- igraph::get.edgelist(g)
  #convert to a four column edge data frame with source and destination coordinates
  edges_df <- data.frame(node_coords[edgelist[,1],], node_coords[edgelist[,2],])
  colnames(edges_df) <- c("xcoord1","ycoord1","xcoord2","ycoord2")
  
  if( is.null(net_vertices) && is.null(net_edges) ){
      hplot <- ggplot2::ggplot(data_df, ggplot2::aes_string(x = 'xcoord', y = 'ycoord'), ...) + 
        ggplot2::geom_segment(ggplot2::aes_string(x = 'xcoord1', y = 'ycoord1', 
                                                  xend = 'xcoord2', yend = 'ycoord2'), 
                              data = edges_df, 
                              size = 0.8, 
                              colour = "grey") +
        ggplot2::geom_point(shape = 19, 
                            size = 1.7) +
        ggplot2::scale_y_continuous(name = "y-coordinate") + 
        ggplot2::scale_x_continuous(name = "x-coordinate") + 
        ggplot2::theme_bw()
    }else{
      edge_ends <- igraph::ends(g, net_edges)
      
      #convert to a four column edge data frame with source and destination coordinates
      sub_edges_df <- data.frame(node_coords[edge_ends[,1],], node_coords[edge_ends[,2],])
      colnames(sub_edges_df) <- c("xcoord1","ycoord1","xcoord2","ycoord2")
      
      hplot <- ggplot2::ggplot(data_df, ggplot2::aes_string(x = 'xcoord', y = 'ycoord'), ...) + 
        ggplot2::geom_segment(ggplot2::aes_string(x = 'xcoord1', y = 'ycoord1', 
                                                  xend = 'xcoord2', yend = 'ycoord2'),
                              data = edges_df,
                              size = 0.8,
                              colour = 'grey') +
        ggplot2::geom_segment(ggplot2::aes_string(x = 'xcoord1', y = 'ycoord1', 
                                                  xend = 'xcoord2', yend = 'ycoord2'),
                              data = sub_edges_df,
                              size = 0.8 * high_size,
                              colour = 'green') +
        ggplot2::geom_point(shape = 19, 
                            size = 1.7,
                            colour="gray") +
        ggplot2::geom_point(data = highlighted_df,
                            shape = 19,
                            size = 1.7 * high_size,
                            colour = 'darkgreen',
                            ggplot2::aes_string(x = 'xcoord', y = 'ycoord')) +
        ggplot2::scale_y_continuous(name = "y-coordinate") + 
        ggplot2::scale_x_continuous(name = "x-coordinate") + 
        ggplot2::theme_bw()
    }

  
  if(!is.null(events)){
    hplot + ggplot2::geom_point(data = as.data.frame(events),
                                mapping = ggplot2::aes(x = xcoord, y = ycoord),
                                shape = 22, fill = 'orange', color = 'orange',
                                alpha = alpha)
  }else{
    hplot
  }
}
