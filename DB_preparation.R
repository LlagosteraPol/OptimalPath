
rm(list = ls())

source("functions.R")


load(file="DB/RData/Linear_pixel_final_18_Juny_2021_ultima_copia.RData")

#Importa dades
Dades_vertex<-+data.frame(V1=LNnew$vertices$x, V2=LNnew$vertices$y)
Dades_segments<-data.frame(V1=LNnew$from, V2=LNnew$to)
Dades_pesos<-data.frame(V1=seg_m)
Dades_distancies<-data.frame(V1=length_seg_entre_cross)

a = 0.5
b = 0.5


transformed_accIntensities <- data.frame(V1=mapply(FUN = `/`, Dades_pesos, 
                                                   max(Dades_pesos), SIMPLIFY = FALSE))

transformed_accIntensities <- data.frame(V1=mapply(FUN = `*`, transformed_accIntensities, 
                                                   a, SIMPLIFY = FALSE))

transformed_distances <- data.frame(V1=mapply(FUN = `/`, Dades_distancies, 
                                              max(Dades_distancies), SIMPLIFY = FALSE))

transformed_distances <- data.frame(V1=mapply(FUN = `*`, transformed_distances, 
                                              b, SIMPLIFY = FALSE))


#all_data <- combine_weights(transformed_accIntensities, transformed_distances, 0.8)
all_data <- transformed_distances+transformed_accIntensities
# transformed_all <- a*transformed_distances + b*transformed_distances 

# Dades_vertex<-read.table("DB/Data/Dades_vertex_4_juny.data",header=FALSE)
# Dades_segments<-read.table("DB/Data/Dades_segments_4_juny.data",header=FALSE)
# Dades_pesos<-read.table("DB/Data/Dades_pesos_750_4_juny.data",header=FALSE)
# Dades_distancies<-read.table("DB/Data/Dades_Dist_creua_4_juny.data",header=FALSE)

#Dades_vertex<-read.table("DB/Data/Dades_vertex.data",header=FALSE)
#Dades_segments<-read.table("DB/Data/Dades_segments.data",header=FALSE)
#Dades_pesos<-read.table("DB/Data/Dades_pesos_750.data",header=FALSE)
#Dades_distancies<-read.table("DB/Data/Dades_Dist_creua.data",header=FALSE)

ACC<-read.table("DB/Data/Projected_Accidents_Cat.data", header=F) ## Lectura dels accidents


##finestre d'observació
minx<-278486
maxx<-minx+40000
miny<-4587647
maxy<-miny+40000
wind=owin(c(minx,maxx),c(miny,maxy))

#Extract point pattern
ACC_win<-ppp(ACC$V1,ACC$V2, window=wind)

#crida vertex
xvt<-Dades_vertex$V1
yvt<-Dades_vertex$V2

##crea un point pattern dels vertexs
ppp_vertex<-ppp(xvt,yvt,window=wind)

#crida els segments (arestes)
from1<-Dades_segments$V1 #son els from (index del vertex de sortida del segment)
to1<-Dades_segments$V2 #son els to (index del vertex de arribada del segment)
edgs<-cbind(from1,to1)


#crea el linnet
LN_vertex<- linnet(ppp_vertex, edges=edgs)


#si necessites la matriu d'adjacencies només et cal fer
#Matriu_adja<-LN_vertex$m

#crida el pesos de cada segment/aresta
#Pesos<-Dades_pesos$V1

#per exemple, Pesos[1] és el pes de l'aresta 1 (amb el mateix ordre que estan en l'objecte LN_vertex)
#----------------------------------------------------------------------------------------------









