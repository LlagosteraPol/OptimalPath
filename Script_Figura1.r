rm(list=ls())

library(rgdal)
library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(raster)
library(ks) #para la ventana de las derivadas de g*
library(deldir)
library(RandomFields)


## Lectura dels shapefiles
pv.spain<-readOGR("Spain_3.shp")
pv.map<-readOGR("Carret_IMD2015_CatPenins.shp")
pv.cat<-readOGR("CatalunyaPeninsular.shp")
pv.CCAA<-readOGR("ll_autonomicas_inspire_peninbal_etrs89_30N.shp")
madrid<-read.csv("Madrid_Location.csv", sep=";")

## Marges del mapa d'Espanya
xlim_sp<-c(0,1126923)
ylim_sp<-c(4000000,4859517)

## Marges de la zona d'estudi
xmin<-771000
xmax<-xmin+250000
ymin<-4490000
ymax<-ymin+265000


## Lectura dels accidents
ACC<-read.table("Projected_Accidents_Cat.data", header=F)

##Per obtenir l'ampliació has de cridar
pv.map.ln<- as.linnet.SpatialLines(pv.map, fuse=TRUE)

minx<-365000
maxx<-minx+50000
miny<-4580000
maxy<-miny+50000

win=owin(xrange=c(minx,maxx), yrange= c(miny, maxy))
##Generate new linnet object (new Linear Network)
LNnew<-pv.map.ln[win]

#Extract point pattern
ACC_win<-ppp(ACC$V1,ACC$V2, window=win)

pdf("Figure1_new_26_jul_2021.pdf",height=6,width=13.5)
#layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
layout(matrix(c(1,2,3),1,3,byrow=TRUE))

par(mar =0+c(3, 2, 2.5, 2))
#par(mar =0+c(1, 0, 0.5, 0))
### Mapa general d'Espanya
plot(pv.CCAA,col="grey",axes=FALSE) #,xlim=c(300000,800000),ylim=c(3950000,4850000))
plot(pv.spain,lwd=1.2,add=T)

rect(madrid$X[1],3892590, madrid$X[1]+93910,3892590+10000,border="black",lwd=1,col="black")
rect(madrid$X[1]+93910,3892590, madrid$X[1]+93910*2,3892590+10000,border="black",lwd=1)
rect(madrid$X[1]+93910*2,3892590, madrid$X[1]+93910*3,3892590+10000,border="black",lwd=1,col="black")
mtext("0", at=madrid$X[1], line = -31.5, cex=0.70)
mtext("100", at=madrid$X[1]+93910, line = -31.5, cex=0.70)
mtext("200", at=madrid$X[1]+93910*2, line = -31.5, cex=0.70)
mtext("300", at=madrid$X[1]+93910*3, line = -31.5, cex=0.70)
mtext("km", at=madrid$X[1]+140865, line = -33, cex=0.70)

rect(xmin+500000,ymin,xmax+500000,ymax+5000,border="black",lwd=2)
points(madrid$X[1],madrid$Y[1],pch=16)
#mtext("Madrid", at=madrid$X[1], line = -8.2, cex=1.4)
mtext("Madrid", at=madrid$X[1], line = -17.2, cex=1.4)
alpha1=0.4
cex_point<-0.6

#par(mar =0+c(3, 2, 2.5, 2))

par(mar =0+c(4,4 ,4, 4))
#### Plot zona estudi
plot(pv.cat,ylab="Km",cex.lab=1.5)
xmin<-260000
xmax<-527500
ymin<-4488000
ymax<-4749000

#par(mar =0+c(1, 0, 0.5, 0))
#par(mar=0+c(5,4,4.5,4))

#par(mar =0+c(3.2, 2.2, 3.2, 2.2))

plot(pv.map, col="grey",axes=FALSE,add=TRUE)

axis(1, at=c(xmin, xmin+500+(xmax-xmin)/5,xmin+500*2+2*(xmax-xmin)/5,xmin+500*3+3*(xmax-xmin)/5,xmin+500*4+4*(xmax-xmin)/5, xmax+500*5), 
labels = c(0,54,54*2,54*3,54*4,54*5),
     pos=c(ymin, xmin),cex.axis=1.40, mgp=c(0, 0.7, 0))
##Que significa xmax+500*5 (metros) en realidad el eje x en total comienza en el km 260 y acaba en el 527.5, por tanto tiene una alargada de 267.5 km y al poner 270 tenemos que poner xmax+2500 para colocar el label 270 en el valor de las x=270. Sucede lo mismo para el resto de valores en incrementos de 500

axis(2, at=c(ymin, ymin+1800+(ymax-ymin)/5,ymin+1800*2+2*(ymax-ymin)/5,ymin+1800*3+3*(ymax-ymin)/5,ymin+1800*4+4*(ymax-ymin)/5, ymax+1800*5), 
labels = c(0,54,54*2,54*3,54*4,54*5),
    pos=c(xmin, ymin),cex.axis=1.40, mgp=c(0, 0.7, 0),las=2)
rect(xmin,ymin,xmax+500*5,ymax+1800*5)
points(ACC$V1,ACC$V2,pch=20,col=rgb(0,0,0,alpha=alpha1),cex=cex_point)
#points(xmin+145000,ymin+80000,pch=16,col="red")
mtext("Km", at=xmin+145000, line = -35, cex=0.90)

#par(mar =0.2+c(3.2, 1.5, 2.2, 2))

plot(LNnew, main="",axes=FALSE,col="grey")
axis(1, at=c(minx, minx+(maxx-minx)/5,minx+2*(maxx-minx)/5,minx+3*(maxx-minx)/5,minx+4*(maxx-minx)/5, maxx), 
labels = c(106,106+10,106+20,106+30,106+40,106+50),
     pos=c(miny, minx),cex.axis=1.40, mgp=c(0, 0.7, 0))

axis(2, at=c(miny, miny+(maxy-miny)/5,miny+2*(maxy-miny)/5,miny+3*(maxy-miny)/5,miny+4*(maxy-miny)/5, maxy), 
labels = c(92,92+10,92+20,92+30,92+40,92+50),
     pos=c(minx, miny),cex.axis=1.40, mgp=c(0, 0.7, 0),las=2)

rect(minx,miny,maxx,maxy)
points(ACC_win$x,ACC_win$y,pch=20,col=rgb(0,0,0,alpha=alpha1),cex=1.2)
mtext("Km", at=xmin+97000, line = -19.5, cex=0.90,las=2)
mtext("Km", at=xmin+130000, line = -35, cex=0.90)

dev.off()

