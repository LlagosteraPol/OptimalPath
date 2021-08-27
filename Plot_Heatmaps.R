source("functions.R")


load(file="DB/RData/Linear_pixel_final_18_Juny_2021_ultima_copia.RData")

pdf("Images/Heatmaps.pdf",height=6,width=13.5)

# mar: bottom, left, top, and right.
# Reduce margins
par(mfrow = c(1, 2),     # 1x2 layout
    oma = c(1, 0, 1, 1), # one row of text at the outer right bottom and top margins
    mar = c(0, 0, 0, 0), # no space to separate plots
    xpd = NA)            # allow content to protrude into outer margin (and beyond)


plot.lpp.lines2(LNnew,seg_m, 0.05) ##per ensenyar els pesos per lines

image(ww,col=topo.colors(100), main="",
      xlim_min=LNnew$window$xrange[1],
      xlim_max=LNnew$window$xrange[2],
      ylim_min=LNnew$window$yrange[1],
      ylim_max=LNnew$window$yrange[2]) ##per mostrar els pesos per pixel, ww

dev.off()