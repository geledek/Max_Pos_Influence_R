source("queue.R")

Greedy <- function(g, k) {
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
      output <- GreedyUpdate(g, c(S, v))
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
    #PlotPreConfig(gMax)
  }
  return(list("seed" = S, "influence" = O))
}

GreedyUpdate <- function(g, indices) {
  q <- queue(FALSE)
  enqueue(q, indices)
  V(g)[indices]$activated <- TRUE
  V(g)[indices]$oActed <- V(g)[indices]$o
  V(g)[indices]$r <- 1

  while (length(q) > 0) {
    v <- dequeue(q)
    currentR <- V(g)[v]$r + 1
    for (i in V(g)[nei(v, mode="out")][!(V(g)[nei(v, mode="out")]$activated & (V(g)[nei(v, mode="out")]$r <= V(g)[v]$r))]) {
      influencingNei <- V(g)[nei(i, mode="in")][V(g)[nei(i, mode="in")]$activated & (V(g)[nei(i, mode="in")]$r < currentR)]
      theta <- sum( E(g)[from(influencingNei) & to(i)]$w)
      if (theta >= V(g)[i]$theta) {
        V(g)[i]$activated <- TRUE
        V(g)[i]$r <- currentR
        V(g)[i]$oActed <- V(g)[i]$o
        for (vnei in influencingNei) {
          V(g)[i]$oActed <- V(g)[i]$oActed + V(g)[vnei]$oActed * E(g)[from(vnei) & to(i)]$w
        }
        V(g)[i]$oActed <- ifelse(V(g)[i]$oActed > 1, 1, V(g)[i]$oActed)
        V(g)[i]$oActed <- ifelse(V(g)[i]$oActed < -1, -1, V(g)[i]$oActed)
        enqueue(q, i)
      }
    }
  }

  return(list("graph" = g, "score" = sum(V(g)$oActed)))
}