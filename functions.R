library("igraph")

# R is passing by value, not by reference
InitiateGraph <- function(n) {
  el <- read.csv(file.choose(), sep=" ")
  if(missing(n)){
    n <- length(el[,1])
  }
  el[,1] <- as.character(el[,1]) 
  el[,2] <- as.character(el[,2])
  el <- as.matrix(el) 

  g <- graph.edgelist(el[1:n,1:2])
  
  # Edge Weight
  E(g)$w <- as.numeric(runif(length(E(g)),min=0,max=1))
  E(g)$w = E(g)$w / strength(g, mode = "in",weights = E(g)$w)[get.edgelist(g)[,2]] #Normalize only the incoming edge
  
  # Activation Threshold
  V(g)$theta <- as.numeric(runif(length(V(g)), min = 0, max = 1))

  # Opinion
  # uniform
  # V(g)$o <- as.numeric(runif(length(V(g)), min = -1, max = 1))
  # norm
  V(g)$o <- as.numeric(rnorm(length(V(g))))
  # norm mean = 0.5
  # V(g)$o <- as.numeric(rnorm(length(V(g)), mean = 0.5))
  #>norm mean = -0.5
  # V(g)$o <- as.numeric(rnorm(length(V(g)), mean = -0.5))
  
  # regulate values of opinion to [-1, 1]
  V(g)$o <- ifelse(V(g)$o >  1,  1, V(g)$o)
  V(g)$o <- ifelse(V(g)$o < -1, -1, V(g)$o)

  # opinion upon activation
  V(g)$activated <- as.logical(rep(FALSE, length(V(g))))

  # !!!!!!!! for easy debugging, should be removed later !!!!!
  for (i in V(g)) {
    V(g)[i]$id <- i
  }
  for (i in E(g)) {
    E(g)[i]$id <- i
  }

  return(g)
}

PlotPreConfig <- function(g) {
  plot(g,
       layout            = layout.fruchterman.reingold,
       vertex.size       = 10,
       vertex.label      = V(g)$id,    #!!!!!!!! for easy debugging, should be removed later !!!!!
       # vertex.label.dist = 0.5,
       vertex.color      = ifelse(V(g)$activated, "red", "lightblue"),
       #edge.label        = E(g)$id,           #!!!!!!!! for easy debugging, should be removed later !!!!!
       edge.curved       = 0.2,
       edge.width        = E(g)$w * 5,
       edge.arrow.size   = 0.3)
}
