library("igraph")

el=read.csv(file.choose(), sep=" ")
el[,1]=as.character(el[,1]) 
el[,2]=as.character(el[,2])
el=as.matrix(el) 

g=graph.edgelist(el[,1:2]) # load the first 10 lines

E(g)$weight=as.numeric(runif(length(g),min = 0, max = 1))
E(g)$weight /strength(g,mode="out")[get.edgelist(g)[,1]]
V(g)$weight=as.numeric(runif(length(V(g)),min = 0, max = 1))


plot(g,layout=layout.fruchterman.reingold,edge.width=E(g)$weight/2)
