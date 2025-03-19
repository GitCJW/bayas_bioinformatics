#' An approximation function for the relationship between sample size and power,
#' used to estimate the optimal sample size candidate.
#' Returns the power for a given sample size.
#' @noRd
powerFct <- function (N, a, b) {
  power <- ((1/(1+a^-(N*b)))*2)-1
  return (power)
}

#' An approximation function for the relationship between sample size and power,
#' used to estimate the optimal sample size candidate.
#' Returns the sample size for a given power.
#' @noRd
NFct <- function (power, a, b) {
  N <- (-log((2/(power+1))-1, a)/b)
  return (N)
}

#' Calculates the weighted squared difference between the approximated power and a computed power.
#' Serves as a loss function for 'optim' to determine the parameters 'a' and 'b' for the 'powerFct' function.
#' @noRd
squaredPowerDiff <- function (params, N, power, weights) {
  a <- params[1]
  b <- params[2]
  powerDiff <- ((powerFct(N, a, b) - power)^2)*weights
  return (sum(powerDiff))
}

