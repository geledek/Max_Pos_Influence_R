library("igraph")

el=read.csv(file.choose())
el[,1]=as.character(el[,1]) 
el[,2]=as.character(el[,2])
el=as.matrix(el) 
g=graph.edgelist(el[1:10,1:2]) # load the first 10 lines
E(g)$weight=as.numeric(el[,3])

plot(g,layout=layout.fruchterman.reingold,edge.width=E(g)$weight/2)