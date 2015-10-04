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

OVMUpdateActivationStatus <- function(g, u){
    Q <- queue(FALSE)
    Qcr <- queue(FALSE)
    enqueue(Q, u)
    enqueue(Qcr, 0)
    
    L <- c()
    A <- c()
    rx <- 0
    while (length(Q) > 0) {
        u <- dequeue.queue(Q)
        rcx <- dequeue.queue(Qcr)
        for (x in V(g)[nei(u, mode="out")]$id){
            if (!(is.element(x,A)) && sum(E(g)[to(x)]$w * V(g)[nei(x, mode="in")]$activated) > V(g)[x]$theta){
                A <- c(A,x)
                rx <- rcx
                enqueue(Q, x)
                enqueue(Qcr, rx)
            }
            else if(is.element(x,A) && crx < rx) {
                rx <- crx
            }
            else{
                L <- c(L, x)
            }
            rx <- rx + 1
        }
    }
}

OVMUpdateOpition <- function(G, L){
    
}

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
            return S
        }
        
        S <- c(S, u)
        OVMUpdateActivationStatus(u)
        OVMUpdateOpition(G,L)
        return S
    }
}