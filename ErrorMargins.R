if(!exists("simulateMeasuring", mode="function")) source("ProspectTheory.R")
if(!exists("calculateEstimatedSet", mode="function")) source("TradeOffMethod.R")

analyseGains <- function(p, x0, g, G, n, r){
  experimentalSet <- simulateMeasuring(g, G, p, x0, n, "Gain")
  print(experimentalSet)
  
  print(experimentalSet^0.88)
  
  
  estimatedSet <- calculateEstimatedSet(experimentalSet, floor(length(experimentalSet)/2),r)
  print(estimatedSet)
  
  plot(calculateSlope(experimentalSet^0.88), type="l")
  plot(calculateSlope(estimatedSet), type="l")
  
  plot(experimentalSet, experimentalSet^0.88, type="l")
  plot(experimentalSet, estimatedSet, type="l")

  return (data.frame(experimentalSet, estimatedSet))
  
}

analyseLosses <- function(p, y0, l, L, n, r) {
  experimentalSet <- simulateMeasuring(l, L, p, y0, n, "Loss")
  print(experimentalSet)
  
  print(experimentalSet^0.88)
  
  
  #estimatedSet <- calculateEstimatedSet(experimentalSet, floor(length(experimentalSet)/2),r)
  #print(estimatedSet)
  
  #plot(calculateSlope(experimentalSet^0.88), type="l")
  #plot(calculateSlope(estimatedSet), type="l")
  
  #plot(experimentalSet, experimentalSet^0.88, type="l")
  #plot(experimentalSet, estimatedSet, type="l")
  
  #return (data.frame(experimentalSet, estimatedSet))    
}

analyseGains(0.5,1500,300,1000,10,0.5)
analyseLosses(0.5,-200,-50,-100,10,0.5)
