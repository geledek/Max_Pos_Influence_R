source("functions.R")
source("greedy.R")
source("greedy_plus.R")
source("greedy_ovm_up.R")
source("ovm.R")
source("test.R")

g <- InitiateGraph(200) # pass number of edges as argument
PlotPreConfig(g)

test(g)
