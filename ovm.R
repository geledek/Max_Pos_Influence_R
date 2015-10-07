source("queue.R")

OVMMain <- function(g, k, pc){
  Q <- queue(FALSE)
  S <- c()
  C <- OVMSelect(g, k, pc)
  for (u in C){
    ### TO-DO
    # compute S = O({u})
    enqueue(Q, u)
  }

  while (length(S) < k){
    repeat{
      u <- dequeue(Q)
      OVMUpdateActivationStatus(u)
      OVMUpdateOpition(G,L)
      if(condition){
        break
      }
    }

    ## TO-DO
    if (thetau(S) <= 0){
      return(S)
    }

    S <- c(S, u)
    OVMUpdateActivationStatus(u)
    OVMUpdateOpition(G,L)
    return(S)
  }
}

OVMSelect <- function(g, k, p) {
    C <- c()
    P <- c()
    o <- 0
    for (v in V(g)) {
        o_v <- V(g)[v]$o 

        n_active <- sum(V(g)[nei(v, mode="out")][activated==TRUE]$o
                    + V(g)[v]$o * E(g)[from(v)]$w
                    * ((V(g)[nei(v, mode="out")]$activated==TRUE)*1))
        
        n_inactive <- sum((V(g)[nei(v, mode="out")][activated==FALSE]$o
                        + V(g)[v]$o * E(g)[from(v)]$w*((V(g)[nei(v, mode="out")]$activated==FALSE)*1))
                        * V(g)[v]$o * E(g)[from(v)]$w*((V(g)[nei(v, mode="out")]$activated==FALSE)*1)
                        / strength(g, mode = "in",weights = E(g)$w)[V(g)[nei(v, mode="out")]])
        P[v] <- o + n_active + n_inactive
    }
    P <- cbind(P, 1:length(P))
    P <- P[order(-P[,1]),2][1:pmin(2^p*k, length(P))]
    return(P)
}

OVMUpdate <- function(g, u) {
  q <- queue(FALSE)
  enqueue(q, u)
  V(g)[u]$activated <- TRUE
  V(g)[u]$oActed <- V(g)[u]$o
  V(g)[u]$r <- 1

  while (length(q) > 0) {
    v <- dequeue(q)
    for (i in V(g)[nei(v, mode="out")]) {
      if (V(g)[i]$activated & V(g)[i]$r <= V(g)[v]$r) next

      if (V(g)[i]$activated & V(g)[i]$r > V(g)[v]$r) { # check if vertex i can be activated earlier or the value should be changed
        maxR <- V(g)[i]$r
      }
      if (!V(g)[i]$activated) { # check if vertex can be activated once v is added
        maxR <- max(V(g)[nei(i, mode="in")][V(g)[nei(i, mode="in")]$activated]$r) # activated neighbours that have the largest r
        if (maxR == .Machine$integer.max) {
          message("WTF?!?!?!")
        }
        maxR <- maxR+1
      }

      # check if the vertex can be activated from round V(g)[v]$r+1 to maxR
      for (r in (V(g)[v]$r+1):maxR) {
        influencingNei <- V(g)[nei(i, mode="in")]$activated & (V(g)[nei(i, mode="in")]$r < r)
        theta <- sum( E(g)[to(i)]$w * influencingNei)
        if (theta >= V(g)[i]$theta) {
          V(g)[i]$activated <- TRUE
          V(g)[i]$r <- r
          V(g)[i]$oActed <- V(g)[i]$o + sum(V(g)[nei(i, mode="in")]$oActed * E(g)[to(i)]$w * influencingNei)
          V(g)[i]$oActed <- ifelse(V(g)[i]$oActed > 1, 1, V(g)[i]$oActed)
          V(g)[i]$oActed <- ifelse(V(g)[i]$oActed < -1, -1, V(g)[i]$oActed)
          enqueue(q, i)
          break
        }
      }

    }
  }

  return(list("graph" = g, "score" = sum(V(g)$oActed)))
}