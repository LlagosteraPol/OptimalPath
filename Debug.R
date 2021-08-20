



#load("C:/Users/usuari/RProjects/OptimalPath/DB/RData/Rating_0505_28_july_2021.RData")

safest_jun_lst<- list()

tmp <- jun_men_weight_ordered[[1]][5]$weight
ctr <- 1
i <- 1
for (path in jun_men_weight_ordered){
  if(jun_men_weight_ordered[[i]][5]$weight == tmp){
    ctr <- ctr + 1
  }else{
    tmp <- jun_men_weight_ordered[[i]][5]$weight
    if(length(safest_jun_lst) == 0){
      safest_jun_lst <- list(list(amount = ctr, weight = tmp))
    }else{
      safest_jun_lst <- rbind(safest_jun_lst, list(list(amount = ctr, weight = tmp)))
    }
    ctr <- 1
  }
  i <- i+1
}

# vertices_data <- data.frame(ID=c(1,2,3,4,5,6,7,8,9,10))
# 
# edges_data <- cbind(c(1,  1,  1,  2,  2,  2,  2,  3,  3,  4,  4,  5,  5,  6,  7,  7,  8,  9), 
#                     c(2,  4,  6,  3,  4,  5,  7,  5,  8,  6,  7,  7,  8,  9,  9,  10, 10, 10))
# 
# intensity_data <-   c(3,  7,  2,  8,  6,  10, 5,  10, 3,  4,  8,  6,  2,  1,  9,  8,  7,  1)
# distance_data <-    c(12, 31, 38, 15, 28, 29, 32, 40, 22, 19, 24, 31, 19, 33, 39, 24, 31, 17)
# 
# segments_info <- cbind(edges_data, intensity_data) 
# segments_info <- cbind(segments_info, distance_data) 
# colnames(segments_info) <- c("from","to","intensity","distance")
# 
# edges_df = data.frame(from = segments_info[, 'from'],
#                       to = segments_info[, 'to'],
#                       intensity = segments_info[, 'intensity'],
#                       distance = segments_info[, 'distance'])
# 
# 
# g_t = graph_from_data_frame(edges_df, directed=FALSE, vertices=vertices_data)
# 
# f = 1
# t = 10
# 
# 
# all_paths_t <- all_simple_paths(g_t, from=f, to=t)
# ipaths_t <- list()
# for (path in all_paths_t){
#   
#   intensity_sum <- sum(E(g_t, path = unlist(path))$intensity)
#   
#   distance_sum <- sum(E(g_t, path = unlist(path))$distance)
#   
#   ipaths_t[[length(ipaths_t)+1]] <- list(from = f, to=t, path = as.numeric(unlist(as_ids(path))), 
#                                          intensity = intensity_sum, 
#                                          distance = distance_sum)
# }
# 
# # distance order
# ipaths_t <- ipaths_t[order(sapply(ipaths_t,'[[',4))]
# for(i in 1:length(ipaths_t)){
#   ipaths_t[[i]] <- c(ipaths_t[[i]], n_distance=i)
# }
# 
# #intensity order
# ipaths_t <- ipaths_t[order(sapply(ipaths_t,'[[',5))]
# for(i in 1:length(ipaths_t)){
#   ipaths_t[[i]] <- c(ipaths_t[[i]], n_weight=i)
# }