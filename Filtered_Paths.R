source("functions.R")
source("IgraphModel.R")


juneda = 63
soses = 161
menarguens = 197
belloc = 209


max_distance  = max(E(g)$distance)
max_accidents = max(E(g)$weight)
sos_bell_paths <- all_simple_paths(g, from=soses, to=belloc)
jun_men_paths  <- all_simple_paths(g, from=juneda, to=menarguens)

#-------------------------------------------------------10%------------------------------------------------------------
sos_bell_d10 <- filter_paths(graph = g, from = soses, to = belloc, weight='distance', filter = max_distance*0.10, paths = sos_bell_paths)
jun_men_d10  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='distance', filter = max_distance*0.10, paths = jun_men_paths)

sos_bell_i10 <- filter_paths(graph = g, from = soses, to = belloc, weight='weight', filter = max_weight*0.10, paths = sos_bell_paths)
jun_men_i10  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='weight', filter = max_weight*0.10, paths = jun_men_paths)


#-------------------------------------------------------25%------------------------------------------------------------
sos_bell_d25 <- filter_paths(graph = g, from = soses, to = belloc, weight='distance', filter = max_distance*0.25, paths = sos_bell_paths)
jun_men_d25  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='distance', filter = max_distance*0.25, paths = jun_men_paths)

sos_bell_i25 <- filter_paths(graph = g, from = soses, to = belloc, weight='weight', filter = max_weight*0.25, paths = sos_bell_paths)
jun_men_i25  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='weight', filter = max_weight*0.25, paths = jun_men_paths)


#-------------------------------------------------------50%------------------------------------------------------------
sos_bell_d50 <- filter_paths(graph = g, from = soses, to = belloc, weight='distance', filter = max_distance*0.50, paths = sos_bell_paths)
jun_men_d50  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='distance', filter = max_distance*0.50, paths = jun_men_paths)

sos_bell_i50 <- filter_paths(graph = g, from = soses, to = belloc, weight='weight', filter = max_weight*0.50, paths = sos_bell_paths)
jun_men_i50  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='weight', filter = max_weight*0.50, paths = jun_men_paths)


#-------------------------------------------------------75%------------------------------------------------------------
sos_bell_d75 <- filter_paths(graph = g, from = soses, to = belloc, weight='distance', filter = max_distance*0.75, paths = sos_bell_paths)
jun_men_d75  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='distance', filter = max_distance*0.75, paths = jun_men_paths)

sos_bell_i75 <- filter_paths(graph = g, from = soses, to = belloc, weight='weight', filter = max_weight*0.75, paths = sos_bell_paths)
jun_men_i75  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='weight', filter = max_weight*0.75, paths = jun_men_paths)
