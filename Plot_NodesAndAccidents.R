rm(list = ls())

source("DB_preparation.R")


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

# Proporciò referent a Catalunya en km arrodonit (per l'eix x dels labels)
#propx_min = round((xmin_lld - xmin_cat) / 1000) 
propx_min = 0
propx_max = round((xmax_lld - xmin_lld) / 1000) 


# Igual pro per l'eix y dels labels
#propy_min = round((ymin_lld - ymin_cat) / 1000) 
propy_min = 0
propy_max = round((ymax_lld - ymin_lld) / 1000) 


#--------------------------------------Plot map with nodes--------------------------------------
pdf("Images/Nodes.pdf",height=8,width=8)

plot(LN_vertex,main="")
points(ppp_vertex,pch=19,col="black", cex=1)
rect(minx,miny,maxx,maxy,border="black",lwd=2)



axis(1, at=c(xmin_lld,
             xmin_lld + (xmax_lld-xmin_lld)/4,
             xmin_lld + 2*(xmax_lld-xmin_lld)/4,
             xmin_lld + 3*(xmax_lld-xmin_lld)/4,
             xmax_lld),
     labels = c(propx_min,
                propx_min+(propx_max-propx_min)/4,
                propx_min+2*(propx_max-propx_min)/4,
                propx_min+3*(propx_max-propx_min)/4,
                propx_max),
     pos=c(ymin_lld, xmin_lld),cex.axis=1.40, mgp=c(0, 0.7, 0))

axis(2, at=c(ymin_lld,
             ymin_lld + (ymax_lld-ymin_lld)/4,
             ymin_lld + 2*(ymax_lld-ymin_lld)/4,
             ymin_lld + 3*(ymax_lld-ymin_lld)/4,
             ymax_lld),
     labels = c(propy_min,
                propy_min+(propy_max-propy_min)/4,
                propy_min+2*(propy_max-propy_min)/4,
                propy_min+3*(propy_max-propy_min)/4,
                propy_max),
     pos=c(xmin_lld, ymin_lld),cex.axis=1.40, mgp=c(0, 0.7, 0),las=2)
rect(xmin_lld,ymin_lld,xmax_lld,ymax_lld,border="black",lwd=2)
#Axis
mtext("Km", at=xmin_lld-7000, line = -16, cex=0.90,las=2)
mtext("Km", at=xmin_lld+((xmax_lld-xmin_lld)/2), line = -34, cex=0.90)

dev.off()

#-------------------------------Plot map with projected accidents-------------------------------
pdf("Images/Accidents.pdf",height=8,width=8)
par(mar = c(1, 3, 1, 1)) # Reduce margins

##Passar de class linnet to class psp
pv.map.psp<-as.psp(LN_vertex)
is.psp(pv.map.psp)
Acc_LN<-project2segment(ACC_win, pv.map.psp) # project accidents
rect(minx,miny,maxx,maxy,border="black",lwd=2)
#plot(Acc_LN$Xproj)

##The output ppp object is
Acc_LN$Xproj

##Ara ja pots generar el objecte que contingui el point pattern i les carreteres
LN_pp<-lpp(Acc_LN$Xproj,LN_vertex)
plot(LN_pp, main="", cex=1)
rect(minx,miny,maxx,maxy,border="black",lwd=2)

dev.off()
#----------------------------------------------------------------------------------------------