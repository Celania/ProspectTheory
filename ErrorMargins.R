if(!exists("simulateMeasuring", mode="function")) source("ProspectTheory.R")
if(!exists("calculateEstimatedSet", mode="function")) source("TradeOffMethod.R")

analyse <- function(p, x0, g, G, n, r){
  experimentalSet <- simulateMeasuring(g, G, p, x0, n)
  print(experimentalSet)
  
  estimatedSet <- calculateEstimatedSet(experimentalSet, floor(length(experimentalSet)/2),r)
  print(estimatedSet)
    
  return (data.frame(experimentalSet, estimatedSet))  
}

plot(analyse(0.5,1500,300,1000,10,0.5), type="l")

