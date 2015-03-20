if(!exists("simulateMeasuringGain", mode="function")) source("ProspectTheory.R")
if(!exists("calculateEstimatedSetGain", mode="function")) source("TradeOffMethod.R")

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

analyseLinking <- function(p, y0, y1, x0, n, r) {
   
  d <- (prospectTheoreticalValueMixed(y1, y0, p, x0)) 
  print(d)
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

dataGain <- (analyseGains(0.5,1500,300,1000,10,0.5))
dataLoss <- (analyseLosses(0.5,-200,-50,-100,10,0.5))
analyseLinking(0.5, dataLoss[1,1], dataLoss[2,1], dataGain[1,1], 10, 0.5) #x0,y0,y1 from experimentalSet?
