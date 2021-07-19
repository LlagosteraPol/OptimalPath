source("functions.R")
library(ggplot2)
library(latticeExtra)

load(file="DB/RData/Linear_pixel_final_18_Juny_2021_ultima_copia.RData")

plot.lpp.lines2 <- function(LNnew,seg_m,width_line=0.1){
  
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
  mk2 <- ifelse(mk==0,NA,mk)
  pppp<-ppp(x,y,marks=mk,window=LNnew$window)
  t_i <- as.im(pppp)
  t_i[t_i==0] <- NA
  #t_i <- as.im(pppp)
  #t_i2 <- image(x=x, y=y, z=mk, col = hcl.colors(100, "terrain"), axes = FALSE)
  
  #plot(t_i2, image=TRUE, col= topo.colors(100))
  #plot(intensity(pppp), pch=19,cex=width_line, image=TRUE, las=1, main="", col= topo.colors(100), add=TRUE)
  plot(LNnew, main="")
  
  plot(t_i, main="", col=heat.colors(20,alpha = 1))
  #plot(pppp, pch=19,cex=width_line,col=topo.colors(100),add=TRUE, main="", image=TRUE)
  #rect(LNnew$window$xrange[1],LNnew$window$yrange[1],LNnew$window$xrange[2],LNnew$window$yrange[2],
      # border="black",lwd=1)
}
pdf("Images/FigTest.pdf",height=6,width=13.5)
plot.lpp.lines2(LNnew,seg_m, 0.05) ##per ensenyar els pesos per lines
dev.off()



plot.lpp.lines3 <- function(LNnew,seg_m,width_line=0.1){
  
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
  mk2 <- ifelse(mk==0,NA,mk)
  pppp<-ppp(x,y,marks=mk,window=LNnew$window)
  t_i <- as.im(pppp)
  t_i[t_i==0] <- NA
  #t_i <- as.im(pppp)
  #t_i2 <- image(x=x, y=y, z=mk, col = hcl.colors(100, "terrain"), axes = FALSE)
  
  ppp_df<-as.data.frame(pppp)
  
  

  spplot(ppp_df) 

  
  
  
  #ggplot(ppp_df)
  #ggplot() +  geom_point( data = ppp_df, aes(x=x, y=y)) +scale_fill_gradient(low="white", high="blue") 
  #ggplot(ppp_df, aes(x=x,y=y)) + geom_point(alpha=0.2, aes(color=mk)) + scale_color_gradient(low = "red", high = "yellow")
  #ggplot(ppp_df, aes(x = x, y = y))+theme(panel.background = element_rect(fill='white', colour='red'))
  #plot(t_i2, image=TRUE, col= topo.colors(100))
  #plot(intensity(pppp), pch=19,cex=width_line, image=TRUE, las=1, main="", col= topo.colors(100), add=TRUE)
  #plot(LNnew, main="") 
  
  #plot(t_i, main="", col=heat.colors(20,alpha = 0.1))
  #plot(pppp, pch=19,cex=width_line,col=topo.colors(100),add=TRUE, main="", image=TRUE)
  #rect(LNnew$window$xrange[1],LNnew$window$yrange[1],LNnew$window$xrange[2],LNnew$window$yrange[2],
  # border="black",lwd=1)
}



plot.lpp.lines3(LNnew,seg_m, 0.05) ##per ensenyar els pesos per lines