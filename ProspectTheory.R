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

prospectTheoreticalFunctionalMixed <- function(g, b, p) {
  return (probabilityWeightingGain(p)*utilityFunctionGain(g) + 
            probabilityWeightingLoss(1-p)*utilityFunctionLoss(b))
}
  
prospectTheoreticalValueGain <- function(g, G, p, x0) {
  low <- x0
  high <- x0^2 # is there an underlying rule for determining a value?
  x0value <- prospectTheoreticalFunctionalGain(x0,G,p)
  epsilon <- errorTolerance
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

prospectTheoreticalValueLoss <- function(l, L, p, y0) {
  high <- y0
  low <- y0*10 # is there an underlying rule for determining a value?
  y0value <- prospectTheoreticalFunctionalLoss(y0,L,p)
  epsilon <- errorTolerance
  while (high >= low) {
    mid <- (high + low) / 2
    if ((abs(prospectTheoreticalFunctionalLoss(mid,l,p) - y0value)) < 
          epsilon) {
      return (mid)
    }
    else if ((prospectTheoreticalFunctionalLoss(mid,l,p) - y0value) < 0) {
      low <- mid
    }
    else {
      high <- mid
    }
  }
  return (mid)
}

prospectTheoreticalValueMixed <- function(y1, y0, p, x0) {
  low <- x0
  high <- x0 * 10 # is there an underlying rule for determining a value?
  x0value <- prospectTheoreticalFunctionalMixed(x0,y0,p)
  epsilon <- errorTolerance
  while (low <= high) {
    mid <- (low + high) / 2
    if ((abs(prospectTheoreticalFunctionalMixed(mid,y1,p) - x0value)) < 
          epsilon) {
      return (mid)
    }
    else if ((prospectTheoreticalFunctionalMixed(mid,y1,p) - x0value) > 0) {
      high <- mid
    }
    else {
      low <- mid
    }
  }
  return (mid)
}
