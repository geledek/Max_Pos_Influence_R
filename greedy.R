Greedy <- function(g, k) {
  S <- c()
  O <- 0
  for (i in 1:k) {
    deltaMax <- 0
    gMax <- g
    u <- 0
    
    for (v in V(g)) {
      if (v %in% S) next
      
      output <- Trial(g, c(S, v))
      deltaV <- output$score - O
      if (deltaV > deltaMax) {
        deltaMax <- deltaV
        u <- v
        gMax <- output$graph
      }
    }
    
    if (deltaMax == 0) {
      return(S)
    }
    S <- c(S, u)
    g <- gMax
    PlotPreConfig(g)
  }
  return(S)
}