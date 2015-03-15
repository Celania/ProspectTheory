humanRounding <- function(x) {
  negative <- (FALSE)
  if (x < 0) {
    negative <- (TRUE)
    x <- (-x)
  }
  errorTolerance <- 0.03
  test <- 0
  for (i in seq(floor(log(x,10))+1,1)) {
    test <- round(x,-i)
    if ((test <= x * (1+errorTolerance)) & (test >= x* (1-errorTolerance))){
      if (negative == TRUE) {
        test <- (-test)
      }
      return (test)
    }
  }
  if (negative == TRUE) {
    x <- (-x)
  }
  return (x)
}

humanRounding2 <- function(x) {
  return (round(x, -1* max(floor(log(x,10))+1-3,1)))
}