rm(list = ls())

source("DB_preparation.R")


#--------------------------------------Plot map with nodes--------------------------------------
pdf("Images/Nodes.pdf",height=8,width=8)

plot(LN_vertex,main="")
points(ppp_vertex,pch=19,col="black", cex=1)
rect(minx,miny,maxx,maxy,border="black",lwd=2)

dev.off()

#-------------------------------Plot map with projected accidents-------------------------------
pdf("Images/Accidents.pdf",height=8,width=8)

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