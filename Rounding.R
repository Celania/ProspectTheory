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