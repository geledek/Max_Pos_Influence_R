GreedyPlus <- function(g, k) {
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