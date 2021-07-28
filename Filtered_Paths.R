source("functions.R")
source("IgraphModel.R")


juneda = 63
soses = 161
menarguens = 197
belloc = 209

#percents <- c(10,25,50,75)
#sos_bell_paths <- all_simple_paths(g, from=soses, to=belloc)
#jun_men_paths  <- all_simple_paths(g, from=juneda, to=menarguens)
#load("~/RProjects/OptimalPath/DB/RData/JunMen_allPaths_18_Jul_2021.RData")


#sos_bell_dfilter <- filter_paths(graph = g, from = soses, to = belloc, weight='distance', filter = percents, paths = sos_bell_paths)
#jun_men_dfilter <- filter_paths(graph = g, from = soses, to = belloc, weight='distance', filter = percents, paths = jun_men_paths)

#sos_bell_wfilter <- filter_paths(graph = g, from = soses, to = belloc, weight='weight', filter = percents, paths = sos_bell_paths)
#jun_men_wfilter <- filter_paths(graph = g, from = soses, to = belloc, weight='weight', filter = percents, paths = jun_men_paths)

#------------TEST----------------
max_distance  = max(E(g)$distance)
max_weight  = max(E(g)$weight)
which(E(g)$distance>=(max_distance*(75/100)))
which(E(g)$weight>=(max_weight*(75/100)))

sos_bell_d10_all_paths <- all_simple_paths(gd10, from=soses, to=belloc)
#-------------------------------


# #-------------------------------------------------------10%------------------------------------------------------------

gd10 = filter_graph(graph=g, filter=10, weight='distance')
sos_bell_d10 = rate_paths(graph = gd10, from = soses, to = belloc)
jun_men_d10 = rate_paths(graph = gd10, from = juneda, to = menarguens)

gi10 = filter_graph(graph=g, filter=10, weight='weight')
sos_bell_i10 = rate_paths(graph = gi10, from = soses, to = belloc)
jun_men_i10 = rate_paths(graph = gi10, from = juneda, to = menarguens)


# #-------------------------------------------------------25%------------------------------------------------------------

gd25 = filter_graph(graph=g, filter=25, weight='distance')
sos_bell_d25 = rate_paths(graph = gd25, from = soses, to = belloc)
jun_men_d25 = rate_paths(graph = gd25, from = juneda, to = menarguens)

gi25 = filter_graph(graph=g, filter=25, weight='weight')
sos_bell_i25 = rate_paths(graph = gi25, from = soses, to = belloc)
jun_men_i25 = rate_paths(graph = gi25, from = juneda, to = menarguens)


# #-------------------------------------------------------50%------------------------------------------------------------

gd50 = filter_graph(graph=g, filter=50, weight='distance')
sos_bell_d50 = rate_paths(graph = gd50, from = soses, to = belloc)
jun_men_d50 = rate_paths(graph = gd50, from = juneda, to = menarguens)

gi50 = filter_graph(graph=g, filter=50, weight='weight')
sos_bell_i50 = rate_paths(graph = gi50, from = soses, to = belloc)
jun_men_i50 = rate_paths(graph = gi50, from = juneda, to = menarguens)


# #-------------------------------------------------------75%------------------------------------------------------------

gd75 = filter_graph(graph=g, filter=75, weight='distance')
sos_bell_d75 = rate_paths(graph = gd75, from = soses, to = belloc)
jun_men_d75 = rate_paths(graph = gd75, from = juneda, to = menarguens)

gi75 = filter_graph(graph=g, filter=75, weight='weight')
sos_bell_i75 = rate_paths(graph = gi75, from = soses, to = belloc)
jun_men_i75 = rate_paths(graph = gi75, from = juneda, to = menarguens)


