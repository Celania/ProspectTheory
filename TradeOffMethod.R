if(!exists("prospectTheoreticalValueGain", mode="function")) source("ProspectTheory.R")
if(!exists("humanRounding", mode="function")) source("Rounding.R")
if(!exists("probabilityWeightingGain", mode="function")) source("probabilityWeighting.R")

simulateMeasuring <- function(g, G, p, x0, n, type) {
  a <- rep(x0, n+1)
  for (i in seq(2,n+1)) {
    if (type == "Gain") {
    pValue <- prospectTheoreticalValueGain(g, G, p, a[i-1])
    }
    else {
      pValue <- prospectTheoreticalValueLoss(g, G, p, a[i-1])
    }
    epsilon <- pValue * 0.05
    a[i] <- pValue + runif(1, -epsilon, epsilon)
    a[i] <- humanRounding(a[i])
  }
  return (a)
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


# Extrapolation muss implementiert werden für den Fall das b außerhalb des Experimental Sets liegt
findFirstElementGreater <- function(experimentalSet,b){
  for (i in seq(2,length(experimentalSet))){
    if (experimentalSet[i] > b)
      return (i)
  }
  return (-1)
}

calculates0 <- function(r, i, b, experimentalSet){
  return (1/(probabilityProportionalityGain(r))) * (i-2+(b-experimentalSet[i-1])/(experimentalSet[i]-experimentalSet[i-1]))
}

calculateStepSize <- function(s0){
  return (1/s0)
}

# experimentalSet <- the experimental data gained from examining a person
# normalisedPoint <- which point to use to normalise the estimated utility at (1 <= normalisedPoint <= length(experimentalSet))
# r               <- the probability for the additional measuring required (0 <= r <= 1)
calculateEstimatedSet <- function(experimentalSet, normalisedPoint, r){
  b <- simulateMeasuring(0,experimentalSet[1],r,experimentalSet[2],1, "Gain")[2]
  i <- findFirstElementGreater(experimentalSet, b)
  
  s0 <- (1/(probabilityProportionalityGain(r))) * (i-2+(b-experimentalSet[i-1])/(experimentalSet[i]-experimentalSet[i-1]))
  
  result <- rep(NA, length(experimentalSet))
  for (i in seq(1, length(experimentalSet))){
    result[i] <- ((s0+i)/(s0+normalisedPoint))
  }
  return (result)
}

calculateSlope <- function(set){
  result <- rep (NA, length(set))
  for (i in seq(2,length(set)))
    result[i] <- (set[i]/set[i-1])
  return (result)
}