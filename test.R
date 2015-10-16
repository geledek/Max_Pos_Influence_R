test <- function(g,k) {
  cat(k)
  
  message("Greedy")
  time1 <- system.time(output1 <- Greedy(g, k))
  message("seeds:              ", sprintf("%d,",output1$seed))
  message("influence obtained: ", output1$influence)
  message("time spent:         ", sprintf("%f", time1[3]))
  message("")
  
  message("GreedyPlus")
  time2 <- system.time(output2 <- GreedyPlus(g, k))
  message("seeds:              ", sprintf("%d,",output2$seed))
  message("influence obtained: ", output2$influence)
  message("time spent:         ", sprintf("%f", time2[3]))
  message("")
  
  message("Greedy with ovm update")
  time3 <- system.time(output3 <- GreedyOVMUp(g, k))
  message("seeds:              ", sprintf("%d,",output3$seed))
  message("influence obtained: ", output3$influence)
  message("time spent:         ", sprintf("%f", time3[3]))
  message("")
  
  message("OVM-Potential Candidate Selection")
  time4 <- system.time(output4 <- OVMSearch(g, k, 1))
  message("seeds:              ", sprintf("%d,",output4$seed))
  message("influence obtained: ", output4$influence)
  message("time spent:         ", sprintf("%f", time4[3]))
  message("")
  
  message("OVM-Potential Candidate Selection")
  time5 <- system.time(output5 <- OVMSearch(g, k, 2))
  message("seeds:              ", sprintf("%d,",output5$seed))
  message("influence obtained: ", output5$influence)
  message("time spent:         ", sprintf("%f", time5[3]))
  message("")
  
  return(list("time" = c(time1[3],time2[3],time3[3],time4[3],time5[3]),
              "opinion" = c(output1$influence,output2$influence,output3$influence,output4$influence,output5$influence)))
}