rm(list=ls())

library(rgdal)
library(spatstat)
library(sp)
library(maptools)

getwd()

#function 'par' parameter: mar – A numeric vector of length 4, 
#which sets the margin sizes in the following order: 
#bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).



# Read R database code
load(file="DB/RData/Linear_pixel_final_18_Juny_2021_ultima_copia.RData")

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

###-----------------------------------------------Plot Start----------------------------------------------------
zoombox = "black"

pdf("Images/StudyArea.pdf",height=6,width=13.5)
#layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
layout(matrix(c(1,2,3),1,3,byrow=TRUE))

par(mar =0+c(3, 2, 2.5, 2))
#par(mar =0+c(1, 0, 0.5, 0))

###---------------------------------------------Plot Spain map--------------------------------------------------
plot(pv.CCAA,col="grey",axes=FALSE)
plot(pv.spain,lwd=1.2,add=T)

# Scale
rect(madrid$X[1],3892590, madrid$X[1]+93910,3892590+10000,border="black",lwd=1,col="black")
rect(madrid$X[1]+93910,3892590, madrid$X[1]+93910*2,3892590+10000,border="black",lwd=1)
rect(madrid$X[1]+93910*2,3892590, madrid$X[1]+93910*3,3892590+10000,border="black",lwd=1,col="black")
mtext("0", at=madrid$X[1], line = -31.5, cex=0.70)
mtext("100", at=madrid$X[1]+93910, line = -31.5, cex=0.70)
mtext("200", at=madrid$X[1]+93910*2, line = -31.5, cex=0.70)
mtext("300", at=madrid$X[1]+93910*3, line = -31.5, cex=0.70)
mtext("km", at=madrid$X[1]+140865, line = -33, cex=0.70)

rect(xmin_csp, ymin_csp, xmax_csp, ymax_csp, border=zoombox,lwd=2) # Quadre Catalunya
points(madrid$X[1],madrid$Y[1],pch=16)
#mtext("Madrid", at=madrid$X[1], line = -8.2, cex=1.4)
mtext("Madrid", at=madrid$X[1], line = -17.2, cex=1.4)
alpha1=0.4
cex_point<-0.6


par(mar =0+c(4,4 ,4, 4))

####----------------------------------------Study area (Catalunya)---------------------------------------------
plot(pv.cat,ylab="Km",cex.lab=1.5)

plot(pv.map, col="grey",axes=FALSE,add=TRUE)

axis(1, at=c(xmin_cat, 
             xmin_cat+500+(xmax_cat-xmin_cat)/5, 
             xmin_cat+500*2+2*(xmax_cat-xmin_cat)/5,
             xmin_cat+500*3+3*(xmax_cat-xmin_cat)/5,
             xmin_cat+500*4+4*(xmax_cat-xmin_cat)/5, 
             xmax_cat+500*5), 
    labels = c(0,54,54*2,54*3,54*4,54*5),
    pos=c(ymin_cat, xmin_cat),cex.axis=1.40, mgp=c(0, 0.7, 0))
##Que significa xmax+500*5 (metros) en realidad el eje x en total comienza en el km 260 y acaba en el 527.5, 
#por tanto tiene una alargada de 267.5 km y al poner 270 tenemos que poner xmax+2500 para colocar el label 
#270 en el valor de las x=270. Sucede lo mismo para el resto de valores en incrementos de 500

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

#Lleida square
rect(xmin_lld, ymin_lld, xmax_lld, ymax_lld, border="black",lwd=2)

#Axis
mtext("Km", at=xmin_cat+((xmax_cat-xmin_cat)/2), line = -35, cex=0.90)

par(mar =0.2+c(3.2, 1.5, 2.2, 5.6))

####-----------------------------------------Study area (Lleida)-------------------------------------------------

#L'objected lpp (network més point pattern) és l'objecte LN_pp
plot(LN_pp,axes=FALSE, main="")

# Proporciò referent a Catalunya en km arrodonit (per l'eix x dels labels)
propx_min = round((xmin_lld - xmin_cat) / 1000) 
propx_max = round((xmax_lld - xmin_cat) / 1000) 

# Igual pro per l'eix y dels labels
propy_min = round((ymin_lld - ymin_cat) / 1000) 
propy_max = round((ymax_lld - ymin_cat) / 1000) 

par(mar =0.2+c(4.2, 1.5, 1.2, 5.6))
#plot(LNnew, main="",axes=FALSE,col="grey")
axis(1, at=c(xmin_lld,
             xmin_lld + (xmax_lld-xmin_lld)/5,
             xmin_lld + 2*(xmax_lld-xmin_lld)/5,
             xmin_lld + 3*(xmax_lld-xmin_lld)/5,
             xmin_lld + 4*(xmax_lld-xmin_lld)/5,
             xmax_lld),
     labels = c(propx_min,
                propx_min+(propx_max-propx_min)/5,
                propx_min+2*(propx_max-propx_min)/5,
                propx_min+3*(propx_max-propx_min)/5,
                propx_min+4*(propx_max-propx_min)/5,
                propx_max),
     pos=c(ymin_lld, xmin_lld),cex.axis=1.40, mgp=c(0, 0.7, 0))

axis(2, at=c(ymin_lld,
             ymin_lld + (ymax_lld-ymin_lld)/5,
             ymin_lld + 2*(ymax_lld-ymin_lld)/5,
             ymin_lld + 3*(ymax_lld-ymin_lld)/5,
             ymin_lld + 4*(ymax_lld-ymin_lld)/5,
             ymax_lld),
    labels = c(propy_min,
               propy_min+(propy_max-propy_min)/5,
               propy_min+2*(propy_max-propy_min)/5,
               propy_min+3*(propy_max-propy_min)/5,
               propy_min+4*(propy_max-propy_min)/5,
               propy_max),
    pos=c(xmin_lld, ymin_lld),cex.axis=1.40, mgp=c(0, 0.7, 0),las=2)
rect(xmin_lld,ymin_lld,xmax_lld,ymax_lld,border="black",lwd=2)
#Axis
mtext("Km", at=xmin_lld-7000, line = -22, cex=0.90,las=2)
mtext("Km", at=xmin_lld+((xmax_lld-xmin_lld)/2), line = -37.5, cex=0.90)

dev.off()

