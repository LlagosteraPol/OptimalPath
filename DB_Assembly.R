source("imdAssembly.R")
source("NdviAssembly.R")
source("VelocityAssembly.R")


net_data <- cbind(Dades_segments, Dades_distancies) 
net_data <- cbind(net_data, Dades_pesos)
net_data <- cbind(net_data, imd2)
net_data <- cbind(net_data, ndvi)
net_data <- cbind(net_data, vel)

colnames(net_data) <- c("from","to","distance", "intensity", "imd2015", "ndvi", "velocity")
write.csv2(net_data, "DB/CSV/net_data.csv", row.names = FALSE)