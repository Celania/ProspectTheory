lambda <- 2.25  # lambda > 1
alpha <- 0.88   # 0 < alpha < 1
beta <- 0.88    # 0 < beta < 1

templateProbabilityWeighting <- function(p, gamma) {
  return (p^gamma / (p^gamma + (1-p)^gamma)^(1/gamma))
}
probabilityWeightingGain <- function(p) {
  return (templateProbabilityWeighting(p, 0.61))
}
probabilityWeightingLoss <- function(p) {
  return (templateProbabilityWeighting(p, 0.69))
}
utilityFunctionGain <- function(x) {
  return (x^alpha)
}

utilityFunctionLoss <- function(x) {
  return (-lambda*(-x)^beta)
}
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

humanRounding <- function(x) {
  errorTolerance <- 0.03
  test <- 0
  for (i in seq(floor(log(x,10))+1,1)) {
    test <- round(x,-i)
    if ((test <= x * (1+errorTolerance)) & (test >= x* (1-errorTolerance))){
      return (test)
    }
  }
  return (x)
}

humanRounding2 <- function(x) {
  return (round(x, -1* max(floor(log(x,10))+1-3,1)))
}

simulateMeasuring <- function(g, G, p, x0, n) {
  a <- rep(x0, n+1)
  for (i in seq(2,n+1)) {
    pValue <- prospectTheoreticalValue(g, G, p, a[i-1])
    epsilon <- pValue * 0.05
    a[i] <- pValue # + runif(1, -epsilon, epsilon)
    # a[i] <- humanRounding(a[i])
  }
  return (a)
}

experimentalSet <- simulateMeasuring(500, 1000, 0.5, 3000, 10)

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

r <- 0.3
b <- simulateMeasuring(0,experimentalSet[1],r,experimentalSet[2],1)[2]

# Extrapolation muss implementiert werden für den Fall das b außerhalb des Experimental Sets liegt
findFirstElementGreater <- function(){
  for (i in seq(2,length(experimentalSet))){
    if (experimentalSet[i] > b)
      return (i)
  }
  return (-1)
}

i <- findFirstElementGreater()
s0 <- (1/(probabilityProportionalityGain(r))) * (i-2+(b-experimentalSet[i-1])/(experimentalSet[i]-experimentalSet[i-1]))
utilityStepSize <- (1/s0) 

cat("InputSet = ",experimentalSet)
print("")

# pointToNormalise affects error margins?
calculateNormalisedSet <- function(experimentalSet, valueToNormalise){
  return (experimentalSet/(valueToNormalise))
}

df = data.frame(experimentalSet, sapply(experimentalSet,utilityFunctionGain))
plot(df)


cat("b = ",b)
print("")
cat("s0 = ",s0)
print("")
cat("deltau = ",utilityStepSize)
print("")

calculateEstimatedSet <- function(experimentalSet, normalisedPoint){
  result <- rep(NA, length(experimentalSet))
  for (i in seq(1, length(experimentalSet))){
    result[i] <- ((s0+i)/(s0+normalisedPoint))
  }
  return (result)
}



estimatedSet <- calculateEstimatedSet(normalisedSet, 3)

calculateProportional <- function(set){
  result <- rep(NA, length(set))
  for (i in seq(2,length(set))){
    result[i] <- ((set[i])/(set[i-1]))
  }
  return (result)
}

difference <- function(set){
  result = rep(NA, length(set))
  for (i in seq(2,length(set))){
    result[i] = (set[i]-set[i-1])
  }
  return (result)
}

cat("experimentalUtility = ", sapply(sapply(experimentalSet, utilityFunctionGain),difference))
print("")


estimatedSetProportional <- calculateProportional(estimatedSet)
cat("estimatedSetProportional = ", estimatedSetProportional)
print("")


experimentalUtilitySetProportional <- calculateProportional(sapply(experimentalSet, utilityFunctionGain)) 

cat("experimentalUtilitySetProportional = ", experimentalUtilitySetProportional)
print("")

cat("estimated Set = ",estimatedSet)
print("")
cat("difference = ", estimatedSetProportional - experimentalUtilitySetProportional)

df = data.frame(experimentalSet, estimatedSet)

plot (df, type="o")

# plot(normalisedSet,type="l",col=2) # x-achse 
# lines(estimatedSet,col=3)