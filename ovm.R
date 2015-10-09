source("queue.R")

OVMSearch <- function(g, k, pc) {
  # setup
  # round no.
  V(g)$r <- as.numeric(rep(.Machine$integer.max, length(V(g))))
  # activated opinion value
  V(g)$oActed <- as.numeric(rep(0, length(V(g))))

  S <- c();
  O <- 0;
  PC <- OVMSelect(g, k, pc) # Potential Candidates
  Q <- data.frame(v <- PC, d <- rep(.Machine$integer.max, length(PC)))

  # calculate initial delta
  for (i in 1:length(Q$v)) {
    output <- OVMUpdate(g, Q[i, 1])
    Q[i, 2] <- output$score - O
  }
  Q <- Q[order(-Q[,2]), ]

  while (length(S) < k) {
    message("trying ", length(S)+1, "th time...")
    repeat {
      output <- OVMUpdate(g, Q[1, 1])
      message("---------------------tried ", Q[1,1], "th vertex... old O=", O, " new O=", output$score)
      Q[1, 2] <- output$score - O
      if (Q[1, 2] >= Q[2, 2]) {
        if (Q[1, 2] <= 0) {
          return(S)
        }
        message("select vertex ", Q[1, 1])
        S <- c(S, Q[1, 1])
        O <- output$score
        g <- output$graph
        Q <- Q[-c(1), ]
        break # end repeat
      } else {
        Q <- Q[order(-Q[,2]), ]
      }
    }

  }

  return(S)
}

OVMSelect <- function(g, k, p) {
  P <- as.numeric(rep(.Machine$integer.min, length(V(g))))
  for (v in V(g)) {
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
  return(P[order(-P[,1]),2][1:pmin(2^p*k, length(P)/2)])
}

OVMUpdate <- function(g, u) {
  q <- queue(FALSE)
  enqueue(q, u)
  V(g)[u]$activated <- TRUE
  V(g)[u]$oActed <- V(g)[u]$o
  V(g)[u]$r <- 1

  while (length(q) > 0) {
    v <- dequeue(q)
    for (i in V(g)[nei(v, mode="out")][!(V(g)[nei(v, mode="out")]$activated & (V(g)[nei(v, mode="out")]$r <= V(g)[v]$r))]) {
      # check if vertex i can be activated earlier or the value should be changed
      if (V(g)[i]$activated & (V(g)[i]$r > V(g)[v]$r)) {
        maxR <- V(g)[i]$r
      }
      if (!V(g)[i]$activated) {
        maxR <- max(V(g)[nei(i, mode="in")][V(g)[nei(i, mode="in")]$activated]$r) # activated neighbours that have the largest r
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