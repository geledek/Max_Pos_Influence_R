source("functions.R")
source("greedy.R")
source("greedy_plus.R")
source("greedy_ovm_up.R")
source("ovm.R")
source("test.R")

g <- InitiateGraph(100) # pass number of edges as argument
PlotPreConfig(g)

opinion <- c()
time <- c()
for (k in seq(from = 5, to = 50, by = 5)){
  data <- test(g,k)
  opinion <- cbind(opinion, data$opinion)
  time <- cbind(time, data$time)
}
  
