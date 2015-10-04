Trial_2 <- function(g, indices) {
  q <- queue(FALSE) #those node that have been activated but haven't updated its influence to neighbours
  enqueue(q, indices)
  V(g)[indices]$activated <- TRUE
  while (length(q) > 0) {
    v <- dequeue(q);
    for (i in V(g)[nei(v, mode="out")]) {
      if (V(g)[i]$activated) {
        next
      }
      theta <- sum(E(g)[to(i)]$w * V(g)[nei(i, mode="in")]$activated)
      if (theta >= V(g)[i]$theta) {
        V(g)[i]$activated <- TRUE
        V(g)[i]$o <- sum(V(g)[nei(i, mode="in")]$activated * E(g)[to(i)]$w * V(g)[nei(i, mode="in")]$o)
        V(g)[i]$o <- ifelse(V(g)[i]$o > 1, 1, V(g)[i]$o)
        V(g)[i]$o <- ifelse(V(g)[i]$o < -1, -1, V(g)[i]$o)
        enqueue(q, i)
      }
    }
  }
  
  output <- list("graph" = g, "score" = EvalGraph_2(g))
  return(output)
}

EvalGraph_2 <- function(g) {
  return(sum(V(g)$activated))
}


max_influence <- function(g, k){
  S <- c();
  O <- 0;
  for (i in 1:k) {
    message("trying ", i, "th time...")
    deltaMax <- 0
    gMax <- g
    oMax <- O
    u <- 0
    
    for (v in V(g)) {
      if (v %in% S) next
      
      output <- Trial_2(g, c(S, v))
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
      return(S)
    }
    
    message("----------------------------------------select vertex ", u)
    S <- c(S, u)
    O <- oMax
    PlotPreConfig(gMax)
  }
  return(S)
}