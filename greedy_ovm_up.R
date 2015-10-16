source("ovm.R")

GreedyOVMUp <- function(g, k) {
  V(g)$r <- as.numeric(rep(.Machine$integer.max, length(V(g))))         # round no.
  V(g)$oActed <- as.numeric(rep(0, length(V(g))))                       # activated opinion value
  S <- c()
  O <- 0
  for (i in 1:k) {
    #message("trying ", i, "th time...")
    deltaMax <- 0
    gMax <- g
    oMax <- O
    u <- 0

    for (v in V(g)[!(V(g) %in% S)]) {
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