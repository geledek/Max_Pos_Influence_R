Greedy <- function(g, k) {
  S <- c()
  O <- 0
  for (i in 1:k) {
    message("trying ", i, "th time...")
    deltaMax <- 0
    gMax <- g
    oMax <- O
    u <- 0
    
    for (v in V(g)) {
      if (v %in% S) next
      
      output <- Trial(g, c(S, v))
      message("---------------------tried ", v, "th vertex... old O=", O, " new O=", output$score)
      deltaV <- output$score - O
      if (deltaV > deltaMax) {
        deltaMax <- deltaV
        u <- v
        gMax <- output$graph
        oMax <- output$score
      }
    }
    
    if (deltaMax == 0) {
      return(S)
    }
    
    message("----------------------------------------select vertex ", u)
    S <- c(S, u)
    O <- oMax
    PlotPreConfig(gMax)
  }
  return(S)
}