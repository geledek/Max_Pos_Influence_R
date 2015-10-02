GreedyPlus <- function(g, k) {
  S <- c()
  O <- 0
  # Candidates
  C <- rep(-1, length(V(g)))
  Ov <- rep(0, length(V(g)))
  
  #dd <- data.frame(b = 1:length(V(g)), 
  #                 x = V(g)$o)
  #dd[ order(-dd[,2]), ]
  
  for (i in 1:k) {
    deltaMax <- 0
    gMax <- g
    oMax <- O
    u <- 0
    
    for (v in V(g)) {
      if (v %in% S) next
      
      output <- Trial(g, c(S, v))
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
    S <- c(S, u)
    O <- oMax
    PlotPreConfig(gMax)
  }
  return(S)
}