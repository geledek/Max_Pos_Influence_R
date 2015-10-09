GreedyPlus <- function(g, k) {
  # setup
  # round no.
  V(g)$r <- as.numeric(rep(.Machine$integer.max, length(V(g))))
  # activated opinion value
  V(g)$oActed <- as.numeric(rep(0, length(V(g))))
  
  S <- c();
  O <- 0;
  Q <- data.frame(v <- 1:length(V(g)),
                  d <- rep(.Machine$integer.max, length(V(g))))
  
  # calculate initial delta
  for (i in 1:length(Q$v)) {
    output <- OVMUpdate(g, Q[i, 1])
    Q[i, 2] <- output$score - O
  }
  Q <- Q[order(-Q[,2]), ]
  
  while (length(S) < k) {
    message("trying ", length(S)+1, "th time...")
    repeat {
      if (Q[1, 1] == 13) {
        foo <- 1
      }
      
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

GreedyPlus_old <- function(g, k) {
  S <- c()
  O <- 0
  # Candidates and their respective marginal gain
  C <- data.frame(v <- 1:length(V(g)),
                  d <- rep(1000, length(V(g))))
  for (i in 1:k) {
    message("trying ", i, "th time...")
    selected <- FALSE
    
    for (j in 1:(length(C$v)+2)) {           # maximun length(C$v) trial to find candidate
      output <- Trial(g, c(S, C[1,1]))       # update the highest piority candidate
      C[1,2] <- output$score - O
      message("---------------------tried ", C[1,1], "th vertex... old O=", O, " new O=", output$score)
      
      if (C[1,2] > C[2,2]) {
        if (C[1,2] > 0) {
          message("------------------------------------------------------select vertex ", C[1,1])
          selected <- TRUE
          S <- c(S, C[1,1])
          O <- output$score
          C <- C[-c(1), ]  
        }
        break
      } else {
        if (C[2,2] < 0) {  # if C$d[1] < C$d[2] and C$d[2] is even smaller than 
          break            # 0, which means there won't be any candidate that 
        }                  # can increase the overall gain
        C <- C[order(-C[,2]), ]
      }
    }

    if (!selected) {
      return(S)
    }

  }
  return(S)
}