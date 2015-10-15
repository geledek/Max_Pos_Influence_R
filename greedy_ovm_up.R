source("ovm.R")

GreedyOVMUp <- function(g, k) {
  # setup
  # round no.
  V(g)$r <- as.numeric(rep(.Machine$integer.max, length(V(g))))
  # activated opinion value
  V(g)$oActed <- as.numeric(rep(0, length(V(g))))
  S <- c()
  O <- 0
  for (i in 1:k) {
    #message("trying ", i, "th time...")
    deltaMax <- 0
    gMax <- g
    oMax <- O
    u <- 0

    for (v in V(g)[!(V(g) %in% S)]) {
      #if (v %in% S) next
      
      output <- OVMUpdate(g, v)
      #message("---------------------tried ", v, "th vertex... old O=", O, " new O=", output$score)
      deltaV <- output$score - O
      if (deltaV > deltaMax) {
        deltaMax <- deltaV
        u <- v
        gMax <- output$graph
        oMax <- output$score
      }
    }

    if (deltaMax == 0) {
      return(list("seed" = S, "influence" = O))
    }

    #message("select vertex ", u)
    S <- c(S, u)
    O <- oMax
    g <- gMax
    #PlotPreConfig(g)
  }
  return(list("seed" = S, "influence" = O))
}