rm(list = ls())

source("DB_preparation.R")


#----------------------------------------Figure 2 PLOTS-----------------------------------------
pdf("Images/Figure2.pdf",height=6,width=13.5)
layout(matrix(c(1,2),1,2,byrow=TRUE))

#--------------------------------------Plot map with nodes--------------------------------------
plot(LN_vertex,main="")
points(ppp_vertex,pch=19,col="black", cex=1)


#-------------------------------Plot map with projected accidents-------------------------------
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