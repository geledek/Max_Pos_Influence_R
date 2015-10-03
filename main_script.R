source("functions.R")
source("greedy.R")
source("greedy_plus.R")
source("ovm_select.R")

g <- InitiateGraph(50) # pass number of edges as argument
PlotPreConfig(g)
message("Greedy")
Greedy(g, 10)
message("Greedy_plus")
GreedyPlus(g,10)
message("OVM-Potential Candidate Selection")
OVMSelect(g,10,4)
