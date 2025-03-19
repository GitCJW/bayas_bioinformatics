#' Calculates the 90% credible interval for a vector of success and failure counts.
#' @noRd
powerEstimator <- function(successes){
  nSuc <- sum(successes)
  nFail <- length(successes) - nSuc
  a <- nSuc +2
  b <- nFail +2
  ret <- NULL
  try({
    ret <- hdi(qbeta, shape1 = a, shape2 = b, credMass = 0.90)
    ret05 <- hdi(qbeta, shape1 = a, shape2 = b, credMass = 0.50)
  })
  if(is.null(ret)) stop("Couldn't calculate hdi of power")

  pwrWidth <- ret[2] - ret[1]
  pwrLow <- ret[1]
  pwrHigh <- ret[2]
  pwrMean <- a/(a+b)

  return(list(pwrWidth=pwrWidth, pwrLow=pwrLow, pwrHigh=pwrHigh, pwrMean=pwrMean, pwrMidHigh=ret05[2]))
}
