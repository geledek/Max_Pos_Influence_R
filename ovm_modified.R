source("queue.R")

OVMSearch_Modified <- function(g, k, pc) {
  # setup
  # round no.
  V(g)$r <- as.numeric(rep(.Machine$integer.max, length(V(g))))
  # activated opinion value
  V(g)$oActed <- as.numeric(rep(0, length(V(g))))
  
  S <- c();
  O <- 0;

  gMax <- g
  oMax <- O

  while (length(S) < k) {
    deltaMax <- 0
    vMax <- 0
    message("trying ", length(S)+1, "th time...")

    PC <- OVMSelect_Modified(g, k, pc) # Potential Candidates
    
    for (v in PC) {
      output <- OVMUpdate(g, v)
      message("---------------------tried ", v, "th vertex... old O=", O, " new O=", output$score)
      deltaC <- output$score - O
      if (deltaC > deltaMax) {
        deltaMax <- deltaC
        gMax <- output$graph
        oMax <- output$score
        vMax <- v
      }
    }
    
    if (deltaMax <= 0) {
      return(S)
    }
    
    message("select vertex ", vMax)
    S <- c(S, vMax)
    O <- oMax
    g <- gMax
  }
  
  return(S)
}

OVMSelect_Modified <- function(g, k, p) {
  P <- as.numeric(rep(.Machine$integer.min, length(V(g))))
  for (v in V(g)) {
    if (V(g)[v]$activated) next

    n_active <- sum(V(g)[nei(v, mode="out")][activated==TRUE]$o
                    + V(g)[v]$o * E(g)[from(v)]$w
                    * ((V(g)[nei(v, mode="out")]$activated==TRUE)*1))
    
    n_inactive <- sum((V(g)[nei(v, mode="out")][activated==FALSE]$o
                       + V(g)[v]$o * E(g)[from(v)]$w*((V(g)[nei(v, mode="out")]$activated==FALSE)*1))
                      * V(g)[v]$o * E(g)[from(v)]$w*((V(g)[nei(v, mode="out")]$activated==FALSE)*1)
                      / strength(g, mode = "in",weights = E(g)$w)[V(g)[nei(v, mode="out")]])
    P[v] <- V(g)[v]$o  + n_active + n_inactive
  }
  P <- cbind(P, 1:length(P))
  return(P[order(-P[,1]),2][1:pmin(2^p*k, length(P[,1]))])
}