library("igraph")
source("functions.R")
source("queue.R")
source("greedy.R")

g <- InitiateGraph()
PlotPreConfig(g)
Greedy(g, 10)