if(!exists("simulateMeasuring", mode="function")) source("ProspectTheory.R")
if(!exists("calculateEstimatedSet", mode="function")) source("TradeOffMethod.R")

analyseGains <- function(p, x0, g, G, n, r){
  experimentalSet <- simulateMeasuringGain(g, G, p, x0, n)
  print(experimentalSet)
  
  experimentalUtilitySet <- (experimentalSet^0.88)
  print(experimentalUtilitySet)
  
  estimatedSet <- calculateEstimatedSetGain(experimentalSet, floor(length(experimentalSet)/2),r)
  print(estimatedSet)
  
  plot(calculateSlope(experimentalUtilitySet), type="l")
  plot(calculateSlope(estimatedSet), type="l")
  
  plot(experimentalSet, experimentalUtilitySet, type="l")
  plot(experimentalSet, estimatedSet, type="l")

  return (data.frame(experimentalSet, estimatedSet))
  
}

analyseLosses <- function(p, y0, l, L, n, r) {
  experimentalSet <- simulateMeasuringLoss(l, L, p, y0, n)
  print(experimentalSet)
  
  experimentalUtilitySet <- (-2.25*(-experimentalSet)^0.88)
  print(experimentalUtilitySet)
  
  estimatedSet <- calculateEstimatedSetLoss(experimentalSet, floor(length(experimentalSet)/2),r)
  print(estimatedSet)
  
  plot(calculateSlope(experimentalUtilitySet), type="l")
  plot(calculateSlope(estimatedSet), type="l")
  
  plot(experimentalSet, experimentalUtilitySet, type="l")
  plot(experimentalSet, estimatedSet, type="l")
  
  return (data.frame(experimentalSet, estimatedSet))    
}

analyseLinking <- function(p, y0, l, L, n, r) {
#  experimentalSetLinking <- simulateMeasuring(l, L, p, y0, n)
#  print(experimentalSet)
  
#  experimentalUtilitySet <- (-2.25*(-experimentalSet)^0.88)
#  print(experimentalUtilitySet)
  
#  estimatedSet <- calculateEstimatedSetLoss(experimentalSet, floor(length(experimentalSet)/2),r)
#  print(estimatedSet)
  
#  plot(calculateSlope(experimentalUtilitySet), type="l")
#  plot(calculateSlope(estimatedSet), type="l")
  
#  plot(experimentalSet, experimentalUtilitySet, type="l")
#  plot(experimentalSet, estimatedSet, type="l")
  
#  return (data.frame(experimentalSet, estimatedSet))    
}

# combine in one function to ensure the right data for linking
analyseGains(0.5,1500,300,1000,10,0.5)
analyseLosses(0.5,-200,-50,-100,10,0.5)
#analyseLinking(TODO)
