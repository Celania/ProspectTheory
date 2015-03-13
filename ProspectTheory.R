if(!exists("probabilityWeightingGain", mode="function")) source("probabilityWeighting.R")
if(!exists("utilityFunctionGain", mode="function")) source("utilityFunctions.R")

prospectTheoreticalFunctionalGain <- function(g, b, p) {
  return (probabilityWeightingGain(p)*utilityFunctionGain(g) + 
            (1-probabilityWeightingGain(p))*utilityFunctionGain(b))
}

prospectTheoreticalFunctionalLoss <- function(g, b, p) {
  return (probabilityWeightingLoss(p)*utilityFunctionLoss(b) + 
            (1-probabilityWeightingLoss(p))*utilityFunctionLoss(g))
}

prospectTheoreticalValue <- function(g, G, p, x0) {
  low <- x0
  high <- x0^2 # is there an underlying rule for determining a value?
  x0value <- prospectTheoreticalFunctionalGain(x0,G,p)
  epsilon <- 0.00001
  while (low <= high) {
    mid <- (low + high) / 2
    if ((abs(prospectTheoreticalFunctionalGain(mid,g,p) - x0value)) < 
          epsilon) {
      return (mid)
    }
    else if ((prospectTheoreticalFunctionalGain(mid,g,p) - x0value) > 0) {
      high <- mid
    }
    else {
      low <- mid
    }
  }
  return (mid)
}