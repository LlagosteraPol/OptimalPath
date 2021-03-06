ndvi <- c(0.5,
          0.1,
          0.3,
          0.3,
          0.3,
          0.1,
          0.5,
          0.1,
          0.1,
          0.3, #10
          0.3,
          0.3,
          0.5,
          0.5,
          0.5,
          0.5,
          0.5,
          0.3,
          0.5,
          0.3, #20
          0.1,
          0.7,
          0.3,
          0.3,
          0.3,
          0.3,
          0.3,
          0.3,
          0.3,
          0.3, #30
          0.3,
          0.3,
          0.3,
          0.3,
          0.3,
          0.3,
          0.3,
          0.7,
          0.7,
          0.7, #40
          0.7,
          0.3,
          0.5,
          0.7,
          0.3,
          0.7,
          0.5,
          0.7,
          0.5,
          0.5, #50
          0.5,
          0.1,
          0.1,
          0.5,
          0.3,
          0.7,
          0.5,
          0.3,
          0.5,
          0.3, #60
          0.5,
          0.5,
          0.5,
          0.5,
          0.3,
          0.5,
          0.3,
          0.9,
          0.1,
          0.7, #70
          0.7,
          0.9,
          0.7,
          0.7,
          0.5,
          0.7,
          0.5,
          0.3,
          0.7,
          0.7, #80
          0.5,
          0.7,
          0.3,
          0.3,
          0.7,
          0.7,
          0.7,
          0.5,
          0.9,
          0.5, #90
          0.5,
          0.7,
          0.7,
          0.7,
          0.3,
          0.3,
          0.3,
          0.5,
          0.3,
          0.3, #100
          0.5, 
          0.5,
          0.5,
          0.7,
          0.5,
          0.5,
          0.1,
          0.1,
          0.5,
          0.3, #110
          0.3,
          0.3,
          0.3,
          0.3,
          0.3,
          0.5,
          0.5,
          0.5,
          0.7,
          0.7, #120
          0.9,
          0.7,
          0.7,
          0.7,
          0.7,
          0.7,
          0.7,
          0.7,
          0.9,
          0.9, #130
          0.7,
          0.9,
          0.7,
          0.5,
          0.5,
          0.5,
          0.5,
          0.9,
          0.5,
          0.7, #140
          0.5,
          0.7,
          0.5,
          0.3,
          0.3,
          0.7,
          0.5,
          0.5,
          0.5,
          0.5, #150
          0.3,
          0.3,
          0.3,
          0.5,
          0.7,
          0.5,
          0.3,
          0.3,
          0.3,
          0.3, #160
          0.9,
          0.7,
          0.5,
          0.5,
          0.3,
          0.1,
          0.7,
          0.7,
          0.5,
          0.5, #170
          0.3,
          0.3,
          0.9,
          0.3,
          0.5,
          0.7,
          0.7,
          0.7,
          0.7,
          0.5, #180
          0.3,
          0.7,
          0.7,
          0.7,
          0.5,
          0.7,
          0.3,
          0.3,
          0.3,
          0.3, #190
          0.1,
          0.3,
          0.1,
          0.1,
          0.1,
          0.3,
          0.7,
          0.1,
          0.1,
          0.3, #200
          0.5,
          0.5,
          0.7,
          0.5,
          0.7,
          0.5,
          0.9,
          0.9,
          0.7,
          0.3, #210
          0.3,
          0.3,
          0.5,
          0.5,
          0.5,
          0.7,
          0.7,
          0.5,
          0.5,
          0.5, #220
          0.3,
          0.3,
          0.5,
          0.5,
          0.5,
          0.5,
          0.5,
          0.5,
          0.5,
          0.5, #230
          0.3,
          0.3,
          0.5,
          0.7,
          0.9,
          0.3,
          0.1,
          0.1,
          0.9,
          0.3, #240
          0.7,
          0.9,
          0.7,
          0.7,
          0.5,
          0.7,
          0.7,
          0.1,
          0.7,
          0.1, #250
          0.3,
          0.3,
          0.3,
          0.3,
          0.3,
          0.7,
          0.9,
          0.7,
          0.5,
          0.1, #260
          0.1,
          0.1,
          0.3,
          0.7,
          0.1,
          0.5,
          0.5,
          0.1,
          0.5,
          0.7, #270
          0.5,
          0.5,
          0.7,
          0.3,
          0.3,
          0.3,
          0.7,
          0.9,
          0.3,
          0.5, #280
          0.5,
          0.5,
          0.7,
          0.7,
          0.7,
          0.7,
          0.7,
          0.3,
          0.1,
          0.1, #290
          0.7,
          0.7,
          0.5,
          0.5,
          0.3,
          0.7,
          0.5,
          0.3,
          0.3,
          0.3, #300
          0.1,
          0.5,
          0.7,
          0.1,
          0.7,
          0.3,
          0.5,
          0.7,
          0.7,
          0.5, #310
          0.7,
          0.1,
          0.5,
          0.3,
          0.7,
          0.7,
          0.9,
          0.1,
          0.3,
          0.5, #320
          0.7,
          0.5,
          0.1,
          0.3,
          0.3,
          0.3,
          0.3,
          0.7,
          0.5,
          0.3, #330
          0.5,
          0.5,
          0.3,
          0.3,
          0.3,
          0.3,
          0.3,
          0.9,
          0.3, 
          0.3, #340
          0.3,
          0.9,
          0.5,
          0.7,
          0.7,
          0.5,
          0.3,
          0.9,
          0.1,
          0.5, #350
          0.9,
          0.5,
          0.7,
          0.5,
          0.5,
          0.7,
          0.5,
          0.3,
          0.7,
          0.5, #360
          0.7,
          0.7,
          0.7,
          0.5,
          0.5,
          0.5,
          0.3,
          0.7,
          0.5,
          0.5, #370
          0.7,
          0.5,
          0.5,
          0.1,
          0.5,
          0.5,
          0.3,
          0.3,
          0.5,
          0.1, #380
          0.5,
          0.7,
          0.5,
          0.5,
          0.5,
          0.7,
          0.7,
          0.3,
          0.7,
          0.7, #390
          0.5,
          0.3,
          0.7,
          0.9,
          0.1,
          0.7,
          0.5,
          0.7,
          0.7,
          0.5, #400
          0.9,
          0.7,
          0.9,
          0.3,
          0.5,
          0.3,
          0.3,
          0.3,
          0.7,
          0.7, #410
          0.3,
          0.1,
          0.3,
          0.5,
          0.5,
          0.5,
          0.5,
          0.5,
          0.5,
          0.5, #420
          0.7,
          0.7,
          0.5,
          0.5,
          0.5,
          0.5,
          0.7,
          0.3,
          0.1,
          0.1, #430
          0.1,
          0.1,
          0.1,
          0.7,
          0.5,
          0.3,
          0.5 #437
)


#PlotNetwork(g, high_size = 1, net_edges = igraph::E(g)[1], net_vertices = igraph::ends(g, E(g)[1])) #show edge in map

