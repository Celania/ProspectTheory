if(!exists("prospectTheoreticalValueGain", mode="function")) source("ProspectTheory.R")
if(!exists("humanRounding", mode="function")) source("Rounding.R")
if(!exists("probabilityWeightingGain", mode="function")) source("probabilityWeighting.R")

simulateMeasuringGain <- function(g, G, p, x0, n) {
  a <- rep(x0, n+1)
  for (i in seq(2,n+1)) {
    pValue <- prospectTheoreticalValueGain(g, G, p, a[i-1])
    epsilon <- pValue * 0.05
    random <- (runif(1, -epsilon, epsilon)) 
    a[i] <- pValue #+ random
    #a[i] <- humanRounding(a[i])
  }
  return(a)
}

simulateMeasuringLoss <- function(l, L, p, y0, n) {
  a <- rep(y0, n+1)
  for (i in seq(2,n+1)) {
    pValue <- prospectTheoreticalValueLoss(l, L, p, a[i-1])
    epsilon <- pValue * 0.05
    random <- (runif(1, epsilon, -epsilon))
    a[i] <- pValue #+ random
    #a[i] <- humanRounding(a[i])
  }
  return(a)
}

estimateProbabilityWeightingGain <- function(p){
  return (probabilityWeightingGain(p))
}

estimateProbabilityWeightingLoss <- function(p){
  return (probabilityWeightingLoss(p))
}

probabilityProportionalityGain <- function(p){
  return ((1-estimateProbabilityWeightingGain(p))/estimateProbabilityWeightingGain(p))
}

probabilityProportionalityLoss <- function(p){
  return ((1-estimateProbabilityWeightingLoss(p))/estimateProbabilityWeightingLoss(p))
}

probabilityProportionalityMixed <- function(p){
  return (estimateProbabilityWeightingGain(p)/estimateProbabilityWeightingLoss(1-p))
}


# Extrapolation muss implementiert werden für den Fall das b außerhalb des Experimental Sets liegt
findFirstElementGain <- function(experimentalSet,b){
  for (i in seq(2,length(experimentalSet))){
    if (experimentalSet[i] > b)
      return (i)        
  }
  return (-1)  
}

# Extrapolation muss implementiert werden für den Fall das b außerhalb des Experimental Sets liegt
findFirstElementLoss <- function(experimentalSet,b){
  for (i in seq(2,length(experimentalSet))){
    if (experimentalSet[i] < b)
      return (i)        
  }
  return (-1)
}

# Extrapolation muss implementiert werden für den Fall das b außerhalb des Experimental Sets liegt
findFirstElementMixed <- function(experimentalSet,d){
  for (i in seq(1, length(experimentalSet))){
    if (experimentalSet[i] > d){
      return (i)
    }
  }  
}

calculates0 <- function(r, i, b, experimentalSet){
  return (1/(probabilityProportionalityGain(r))) * (i-2+(b-experimentalSet[i-1])/(experimentalSet[i]-experimentalSet[i-1]))
}

calculatesz <- function(r, k, d, experimentalSet){
  return (probabilityProportionalityMixed(r) * (k - 1 + (d - experimentalSet[k-1])/(experimentalSet[k] - experimentalSet[k-1]))) 
}

calculateStepSizeLinking <- function(sz, utilityStepSize){
  return (sz * utilityStepSize)
}

calculateStepSize <- function(s0){
  return (1/s0)
}

# experimentalSet <- the experimental data gained from examining a person
# normalisedPoint <- which point to use to normalise the estimated utility at (1 <= normalisedPoint <= length(experimentalSet))
# r               <- the probability for the additional measuring required (0 <= r <= 1)
calculateEstimatedSetGain <- function(experimentalSet, normalisedPoint, r){
  b <- simulateMeasuringGain(0,experimentalSet[1],r,experimentalSet[2],1)[2]
  i <- findFirstElementGain(experimentalSet, b)
  result <- rep(NA, length(experimentalSet))  
  s0 <- (1/(probabilityProportionalityGain(r))) * (i-2+(b-experimentalSet[i-1])/(experimentalSet[i]-experimentalSet[i-1]))
  for (i in seq(1, length(experimentalSet))){
    result[i] <- ((s0+i)/(s0+normalisedPoint))
  }
  return (result)
}

calculateEstimatedSetLoss <- function(experimentalSet, normalisedPoint, r){
  b <- simulateMeasuringLoss(0,experimentalSet[1],r,experimentalSet[2],1)[2]
  i <- findFirstElementLoss(experimentalSet, b)
  result <- rep(NA, length(experimentalSet))
  s0 <- (1/(probabilityProportionalityLoss(r))) * (i-2+(b-experimentalSet[i-1])/(experimentalSet[i]-experimentalSet[i-1]))  
  for (i in seq(1, length(experimentalSet))){
    result[i] <- (-(s0+i)/(s0+normalisedPoint))
  }
  return (result) 
}

calculateRelation <- function(sety){
  result <- rep (0, length(sety))
  for (i in seq(2,length(sety)))
    result[i] <- (sety[i]/sety[i-1])
  return (result)
}