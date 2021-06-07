rm(list=ls())

library(rgdal)
library(spatstat)
library(sp)
library(maptools)

getwd()
# Read R database code
load(file="DB/RData/Linear_pixel_final_4_Juny_2021_ultima_copia.RData")

## Lectura dels shapefiles
pv.spain<-readOGR("DB/SHP/Spain_3.shp")
pv.map<-readOGR("DB/SHP/Carret_IMD2015_CatPenins.shp")
pv.cat<-readOGR("DB/SHP/CatalunyaPeninsular.shp")
pv.CCAA<-readOGR("DB/SHP/ll_autonomicas_inspire_peninbal_etrs89_30N.shp")
madrid<-read.csv("DB/Madrid_Location.csv", sep=";")

## Lectura dels accidents
ACC<-read.table("DB/Data/Projected_Accidents_Cat.data", header=F)

##Per obtenir l'ampliació has de cridar
pv.map.ln<- as.linnet.SpatialLines(pv.map, fuse=TRUE)

## Marges del mapa d'Espanya
xlim_sp<-c(0,1126923)
ylim_sp<-c(4000000,4859517)

## Marges de la zona de Catalunya en Espanya
xmin_csp<-771000
xmax_csp<-xmin_csp+250000
ymin_csp<-4490000
ymax_csp<-ymin_csp+265000

## Marges de la zona de Catalunya (proporcionalment)
xmin_cat<-260000
xmax_cat<-527500
ymin_cat<-4488000
ymax_cat<-4749000

##marges de la zona de Lleida
xmin_lld<-278486
xmax_lld<-xmin_lld+40000
ymin_lld<-4587647
ymax_lld<-ymin_lld+40000

win=owin(xrange=c(xmin_lld,xmax_lld), yrange= c(ymin_lld, ymax_lld))
##Generate new linnet object (new Linear Network)
LNnew<-pv.map.ln[win]

#Extract point pattern
ACC_win<-ppp(ACC$V1,ACC$V2, window=win)

######################Inici gràfic############################################
zoombox = "black"

pdf("Images/Figure1_new_Pol.pdf",height=6,width=13.5)
#layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
layout(matrix(c(1,2,3),1,3,byrow=TRUE))

par(mar =0+c(3, 2, 2.5, 2))
#par(mar =0+c(1, 0, 0.5, 0))

### Mapa general d'Espanya
plot(pv.CCAA,col="grey") #,xlim=c(300000,800000),ylim=c(3950000,4850000))
plot(pv.spain,lwd=1.2,add=T)
rect(xmin_csp, ymin_csp, xmax_csp, ymax_csp, border=zoombox,lwd=2) # Quadre Catalunya
points(madrid$X[1],madrid$Y[1],pch=16)
#mtext("Madrid", at=madrid$X[1], line = -8.2, cex=1.4)
mtext("Madrid", at=madrid$X[1], line = -17.2, cex=1.4)
alpha1=0.4
cex_point<-0.6

#### Plot zona estudi (catalunya)
plot(pv.cat)

#par(mar =0+c(1, 0, 0.5, 0))
#par(mar=0+c(5,4,4.5,4))
par(mar =0+c(3.2, 2.2, 3.2, 2.2))
plot(pv.map, col="grey",axes=FALSE,add=TRUE)

axis(1, at=c(xmin_cat, 
             xmin_cat+500+(xmax_cat-xmin_cat)/5, 
             xmin_cat+500*2+2*(xmax_cat-xmin_cat)/5,
             xmin_cat+500*3+3*(xmax_cat-xmin_cat)/5,
             xmin_cat+500*4+4*(xmax_cat-xmin_cat)/5, 
             xmax_cat+500*5), 
    labels = c(0,54,54*2,54*3,54*4,54*5),
    pos=c(ymin_cat, xmin_cat),cex.axis=1.40, mgp=c(0, 0.7, 0))
##Que significa xmax+500*5 (metros) en realidad el eje x en total comienza en el km 260 y acaba en el 527.5, por tanto tiene una alargada de 267.5 km y al poner 270 tenemos que poner xmax+2500 para colocar el label 270 en el valor de las x=270. Sucede lo mismo para el resto de valores en incrementos de 500

axis(2, at=c(ymin_cat, 
             ymin_cat + 1800 + (ymax_cat-ymin_cat)/5,
             ymin_cat + 1800*2 + 2*(ymax_cat-ymin_cat)/5,
             ymin_cat + 1800*3 + 3*(ymax_cat-ymin_cat)/5,
             ymin_cat + 1800*4 + 4*(ymax_cat-ymin_cat)/5, 
             ymax_cat + 1800*5), 
labels = c(0,54,54*2,54*3,54*4,54*5),
    pos=c(xmin_cat, ymin_cat),cex.axis=1.40, mgp=c(0, 0.7, 0),las=2)
rect(xmin_cat,ymin_cat,xmax_cat+500*5,ymax_cat+1800*5,border="black",lwd=2) 
points(ACC$V1,ACC$V2,pch=20,col=rgb(0,0,0,alpha=alpha1),cex=cex_point) # Punts Catalunya
#points(xmin+145000,ymin+80000,pch=16,col="red")
#mtext("Sitges", at=xmin+145000, line = -13.10, cex=1.4)

##Aquí has d'afeguir un rectangle/quadrat al voltant de Lleida (la nostra àrea d'observació, amb un marc negre) per deixar clar quina és l'àrea de treball que apareixerà en la següent finesta (plot(LNnew, etc...)
rect(xmin_lld, ymin_lld, xmax_lld, ymax_lld, border="black",lwd=2) # Quadre Lleida

#L'objected lpp (network més point pattern) és l'objecte LN_pp
plot(LN_pp,axes=FALSE, main="")

propx = round((xmin_lld - xmin_cat) / 1000) # Proporciò referent a Catalunya en km arrodonit (per l'eix x dels labels)
propy = round((ymin_lld - ymin_cat) / 1000) # Igual pro per l'eix y dels labels
gap = 10

par(mar =0.2+c(3.2, 1.5, 2.2, 2))
#plot(LNnew, main="",axes=FALSE,col="grey")
axis(1, at=c(xmin_lld,
             xmin_lld + (xmax_lld-xmin_lld)/5,
             xmin_lld + 2*(xmax_lld-xmin_lld)/5,
             xmin_lld + 3*(xmax_lld-xmin_lld)/5,
             xmin_lld + 4*(xmax_lld-xmin_lld)/5,
             xmax_lld),
     labels = c(propx,propx+(gap*1),propx+(gap*2),propx+(gap*3),propx+(gap*4),propx+(gap*5)),
     pos=c(ymin_lld, xmin_lld),cex.axis=1.40, mgp=c(0, 0.7, 0))

axis(2, at=c(ymin_lld,
             ymin_lld + (ymax_lld-ymin_lld)/5,
             ymin_lld + 2*(ymax_lld-ymin_lld)/5,
             ymin_lld + 3*(ymax_lld-ymin_lld)/5,
             ymin_lld + 4*(ymax_lld-ymin_lld)/5,
             ymax_lld),
    labels = c(propy,propy+(gap*1),propy+(gap*2),propy+(gap*3),propy+(gap*4),propy+(gap*5)),
    pos=c(xmin_lld, ymin_lld),cex.axis=1.40, mgp=c(0, 0.7, 0),las=2)
rect(xmin_lld,ymin_lld,xmax_lld,ymax_lld,border="black",lwd=2)

dev.off()


#---------------------------------Old code to plot Lleida-------------------------------

# propx = round((xmin_lld - xmin_cat) / 1000) # Proporciò referent a Catalunya en km arrodonit (per l'eix x dels labels)
# propy = round((ymin_lld - ymin_cat) / 1000) # Igual pro per l'eix y dels labels
# gap = 10
# 
# par(mar =0.2+c(3.2, 1.5, 2.2, 2))
# plot(LNnew, main="",axes=FALSE,col="grey")
# axis(1, at=c(xmin_lld,
#              xmin_lld + (xmax_lld-xmin_lld)/5,
#              xmin_lld + 2*(xmax_lld-xmin_lld)/5,
#              xmin_lld + 3*(xmax_lld-xmin_lld)/5,
#              xmin_lld + 4*(xmax_lld-xmin_lld)/5,
#              xmax_lld),
#     labels = c(propx,propx+(gap*1),propx+(gap*2),propx+(gap*3),propx+(gap*4),propx+(gap*5)),
#     pos=c(ymin_lld, xmin_lld),cex.axis=1.40, mgp=c(0, 0.7, 0))
# 
# axis(2, at=c(ymin_lld, 
#              ymin_lld + (ymax_lld-ymin_lld)/5,
#              ymin_lld + 2*(ymax_lld-ymin_lld)/5,
#              ymin_lld + 3*(ymax_lld-ymin_lld)/5,
#              ymin_lld + 4*(ymax_lld-ymin_lld)/5, 
#              ymax_lld), 
#     labels = c(propy,propy+(gap*1),propy+(gap*2),propy+(gap*3),propy+(gap*4),propy+(gap*5)),
#     pos=c(xmin_lld, ymin_lld),cex.axis=1.40, mgp=c(0, 0.7, 0),las=2)
# rect(xmin_lld,ymin_lld,xmax_lld,ymax_lld,border="black",lwd=2)
# points(ACC_win$x,ACC_win$y,pch=20,col=rgb(0,0,0,alpha=alpha1),cex=1.2)




