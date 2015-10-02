source("functions.R")
source("greedy.R")

g <- InitiateGraph()
PlotPreConfig(g)
Greedy(g, 10)
