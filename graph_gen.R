library("igraph")
source("functions.R")
source("queue.R")

g  <- InitiateGraph()
PlotPreConfig(g)
output <- Trial(g, 43) # output$graph, output$score