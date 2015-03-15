lambda <- 2.25  # lambda > 1
alpha <- 0.60   # 0 < alpha < 1
beta <- 0.88    # 0 < beta < 1

# alpha <- 0.60   # 0 < alpha < 1
# beta <- 0.88    # 0 < beta < 1

utilityFunctionGain <- function(x) {
  return (x^alpha)
}

utilityFunctionLoss <- function(x) {
  return (-lambda*(-x)^beta)
}