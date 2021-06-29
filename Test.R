rm(list=ls())

library(sp)
library(spatstat)
library(rgeos)
library(maptools)

# get.edge.ids(g, c(1,2, 4,5)) # to know the id of edges given its vertices
# as_ids(E(g)[33 %--% 35]) # same as before
# E(g)[33 %--% 35]$distance <- 999 # set weight
# adjacent_vertices(g, c(1, 34))

#-----------------------------Color palette functions------------------------------
# rainbow(n), heat.colors(n), terrain.colors(n), topo.colors(n), and cm.colors(n)
#----------------------------------------------------------------------------------

#has de carregar aquest fitxer
load(file="DB/RData/Linear_pixel_final_18_Juny_2021_ultima_copia.RData")

#Ara aquí tens:

#----------------------------------PLOT HEATMAP------------------------------------
#ww es el mapa de densitat (pixel a pixel) que tens de intensitat d'accidents
pdf("Images/Heatmap.pdf",height=6,width=13.5)
plot(ww, main = "")
dev.off()
#----------------------------------------------------------------------------------

points(LN_pp,pch=19,col="red", cex=0.5) #LN_pp és el patró puntual projectat
#LNnew és el mapa carreteres (simplificat, però realista) on no surt 
# aquella conneccio (aquesta hauria d'anar a la Figura1)

plot(LNnew)

#L'objected lpp (network més point pattern) és l'objecte LN_pp
#Objected linnet amb el patró puntual i LNnew, és LN_pp
plot(LN_pp,axes=TRUE)

#El vector amb els pesos de les intensitat per ara 436 arestes es    seg_m # 

#El vector amb les distancies es  length_seg_entre_cross
ls(LNnew)

#---------------------------------------------------PLOT------------------------------------------------------
source("functions.R")
image(ww,col=topo.colors(100)) ##per mostrar els pesos per pixel, ww
x11()
plot.lpp.lines(LNnew,seg_m) ##per ensenyar els pesos per lines

#-------------------------------Check ID of element in array by value----------------------------------------
a <- c(3, 2, -7, -3, 5, 2)
c <- which(a==-7) # this will give you numerical value

b <- list(list(3,4,5))
d <- which(unlist(b)==4)

#---------------------------------------------------Create graph from DataFrame EXAMPLE---------------------------------------------------
## A simple example with a couple of actors
## The typical case is that these tables are read in from files....
actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)

## The opposite operation
as_data_frame(g, what="vertices")
as_data_frame(g, what="edges")

#----------------------------------------------------------Get info about the paths-------------------------------------------------------

#all_shortest_paths <- best_paths(graph = g, from = vilanova, to = seros, weight = "distance")
#top_shortest_paths <- all_shortest_paths[1:10]

#all_safest_paths <- best_paths(graph = g, from = vilanova, to = seros, weight = "weight")
#top_safest_paths <- all_safest_paths[1:10]

#best<- best_paths(graph = g, from = "1", to = "15", weight = "all")
#top_paths <- best[1:10]

source("functions.R")
source("IgraphModel.R")
#filtered_paths <- filter_paths(graph = g, from = alcarras, to = albages, edge_param = "distance", filter = 10000)


juneda = 63
soses = 161
menarguens = 197
belloc = 209

#---------------------------------------------------------------------------------
#infopaths <- paths_info(graph = g, from = soses, to = belloc)
sos_bell <- rate_paths(graph = g, from = soses, to = belloc)
sos_bell_distance_ordered <- sos_bell[order(sapply(sos_bell,'[[',6))]
sos_bell_weight_ordered <- sos_bell[order(sapply(sos_bell,'[[',7))]

jun_men <- rate_paths(graph = g, from = juneda, to = menarguens)
jun_men_distance_oreded <- jun_men[order(sapply(jun_men,'[[',6))]
jun_men_weight_oreded <- jun_men[order(sapply(jun_men,'[[',7))]
#-----------------------------------------------------------------------------------------------------------------------------------------