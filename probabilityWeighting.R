templateProbabilityWeighting <- function(p, gamma) {
  return (p^gamma / (p^gamma + (1-p)^gamma)^(1/gamma))
}
probabilityWeightingGain <- function(p) {
  return (templateProbabilityWeighting(p, gammaGain)) #0.61
}
probabilityWeightingLoss <- function(p) {
  return (templateProbabilityWeighting(p, gammaLoss)) #0.69
}