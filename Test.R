rm(list=ls())

library(sp)
library(spatstat)
library(rgeos)
library(maptools)

#has de carregar aquest fitxer
load(file="DB/RData/Linear_pixel_final_4_Juny_2021_ultima_copia.RData")

#Ara aquí tens:

#ww es el mapa de densitat (pixel a pixel) que tens de intensitat d'accidents

plot(ww)

points(LN_pp,pch=19,col="red", cex=0.5) #LN_pp és el patró puntual projectat

#LNnew és el mapa carreteres (simplificat, però realista) on no surt 
# aquella conneccio (aquesta hauria d'anar a la Figura1)

plot(LNnew)

#L'objected lpp (network més point pattern) és l'objecte LN_pp
plot(LN_pp,axes=TRUE)