source("functions.R")
source("greedy.R")
source("greedy_ovm_up.R")


g <- InitiateGraph(50) # pass number of edges as argument
PlotPreConfig(g)
message("Greedy")
Greedy(g, 10)
message("Greedy with ovm update")
GreedyOVMUp(g, 10)
message("OVM-Potential Candidate Selection")
OVMSelect(g,10,4)
