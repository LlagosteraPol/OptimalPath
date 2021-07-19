source("functions.R")
source("IgraphModel.R")


juneda = 63
soses = 161
menarguens = 197
belloc = 209


max_distance  = max(E(g)$distance)
max_accidents = max(E(g)$weight)


#-------------------------------------------------------10%------------------------------------------------------------
sos_bell_d10 <- filter_paths(graph = g, from = soses, to = belloc, weight='distance', filter = max_distance*0.10)
jun_men_d10  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='distance', filter = max_distance*0.10)

sos_bell_i10 <- filter_paths(graph = g, from = soses, to = belloc, weight='weight', filter = max_weight*0.10)
jun_men_i10  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='weight', filter = max_weight*0.10)


#-------------------------------------------------------25%------------------------------------------------------------
sos_bell_d25 <- filter_paths(graph = g, from = soses, to = belloc, weight='distance', filter = max_distance*0.25)
jun_men_d25  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='distance', filter = max_distance*0.25)

sos_bell_i25 <- filter_paths(graph = g, from = soses, to = belloc, weight='weight', filter = max_weight*0.25)
jun_men_i25  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='weight', filter = max_weight*0.25)


#-------------------------------------------------------50%------------------------------------------------------------
sos_bell_d50 <- filter_paths(graph = g, from = soses, to = belloc, weight='distance', filter = max_distance*0.50)
jun_men_d50  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='distance', filter = max_distance*0.50)

sos_bell_i50 <- filter_paths(graph = g, from = soses, to = belloc, weight='weight', filter = max_weight*0.50)
jun_men_i50  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='weight', filter = max_weight*0.50)


#-------------------------------------------------------75%------------------------------------------------------------
sos_bell_d75 <- filter_paths(graph = g, from = soses, to = belloc, weight='distance', filter = max_distance*0.75)
jun_men_d75  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='distance', filter = max_distance*0.75)

sos_bell_i75 <- filter_paths(graph = g, from = soses, to = belloc, weight='weight', filter = max_weight*0.75)
jun_men_i75  <- filter_paths(graph = g, from = juneda, to = menarguens, weight='weight', filter = max_weight*0.75)
