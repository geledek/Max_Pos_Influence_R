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
    P <- sort(P, decreasing = TRUE)[1:(2^p*k)]
    return(P)
}