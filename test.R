test <- function(g) {
  message("Greedy")
  time <- system.time(output1 <- Greedy(g, 5))
  message("seeds:              ", sprintf("%d,",output1$seed))
  message("influence obtained: ", output1$influence)
  message("time spent:         ", sprintf("%f", time[3]))
  message("")
  
  message("GreedyPlus")
  time <- system.time(output2 <- GreedyPlus(g, 5))
  message("seeds:              ", sprintf("%d,",output2$seed))
  message("influence obtained: ", output2$influence)
  message("time spent:         ", sprintf("%f", time[3]))
  message("")
  
  message("Greedy with ovm update")
  time <- system.time(output3 <- GreedyOVMUp(g, 5))
  message("seeds:              ", sprintf("%d,",output3$seed))
  message("influence obtained: ", output3$influence)
  message("time spent:         ", sprintf("%f", time[3]))
  message("")
  
  message("OVM-Potential Candidate Selection")
  time <- system.time(output4 <- OVMSearch(g, 5, 1))
  message("seeds:              ", sprintf("%d,",output4$seed))
  message("influence obtained: ", output4$influence)
  message("time spent:         ", sprintf("%f", time[3]))
  message("")
}