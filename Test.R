rm(list=ls())

library(sp)
library(spatstat)
library(rgeos)
library(maptools)

# get.edge.ids(g, c(1,2, 4,5)) # to know the id of edges given its vertices
# as_ids(E(g)[33 %--% 35]) # same as before
# E(g)[33 %--% 35]$distance <- 999 # set weight

#has de carregar aquest fitxer
load(file="DB/RData/Linear_pixel_final_9_Juny_2021_ultima_copia.RData")

#Ara aquí tens:

#ww es el mapa de densitat (pixel a pixel) que tens de intensitat d'accidents

plot(ww)

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
image(ww,col=terrain.colors(100)) ##per mostrar els pesos per pixel, ww
x11()
plot.lpp.lines(LNnew,seg_m) ##per ensenyar els pesos per lines

##funció plot.lpp.lines()
plot.lpp.lines<- function(LNnew,seg_m,width_line=0.1){
  
  x0<-c();y0<-c();x1<-c();y1<-c()
  for(i in 1:LNnew$lines$n){
    x0[i]<-LNnew$lines$ends$x0[i]
    y0[i]<-LNnew$lines$ends$y0[i]
    x1[i]<-LNnew$lines$ends$x1[i]
    y1[i]<-LNnew$lines$ends$y1[i]
  }
  
  #generar punts
  j1<-0;x<-c();y<-c();mk<-c()
  for(i in 1:LNnew$lines$n){
    m<-(y1[i]-y0[i])/(x1[i]-x0[i])
    a<-y0[i]-m*x0[i]
    for(j in 1:100){
      j1<-j1+1
      x[j1]<-runif(1,min(x0[i],x1[i]),max(x0[i],x1[i]))
      y[j1]<-m*x[j1]+a
      mk[j1]<-seg_m[i]
    }
  }
  pppp<-ppp(x,y,marks=mk,window=LNnew$window)
  plot(LNnew)
  plot(pppp, pch=19,cex=width_line,cols=terrain.colors(100),add=TRUE)
}

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

#-----------------------------------------------------------------------------------------------------------------------------------------