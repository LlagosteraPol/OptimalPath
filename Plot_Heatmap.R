source("functions.R")


load(file="DB/RData/Linear_pixel_final_18_Juny_2021_ultima_copia.RData")

pdf("Images/Figure5.pdf",height=6,width=13.5)
layout(matrix(c(1,2),1,2,byrow=TRUE))


plot.lpp.lines(LNnew,seg_m, 0.05) ##per ensenyar els pesos per lines

image(ww,col=topo.colors(100), main="",
      xlim_min=LNnew$window$xrange[1],
      xlim_max=LNnew$window$xrange[2],
      ylim_min=LNnew$window$yrange[1],
      ylim_max=LNnew$window$yrange[2]) ##per mostrar els pesos per pixel, ww

dev.off()