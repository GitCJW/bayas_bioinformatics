
#' Generates a candidate for the next sample size iteration using a
#' power~N approximation function based on previous iterations.
#' @noRd
nextN <- function (ssd){
  powerDesired <- ssd$intern$powerDesired
  N <- ssd$intern$N
  NMax <- ssd$intern$maxN
  NMin <- ssd$intern$minN
  resultsSSD <- ssd$intern$resultsSSD
  possN <- ssd$intern$possN

  # Init
  NHigh <- NMax
  NLow <- NMin
  resultsSSD <- nextN.reviseCertainty(resultsSSD)
  onlyCertain <- resultsSSD[resultsSSD$certainty==T,]

  # Get the sample sizes below and above the desired power that are closest to it
  ret <- nextN.lowAndHigh(resultsSSD, NLow, NHigh)
  NLow <- ret[1]
  NHigh <- ret[2]

  # Malfunction
  if(NLow > NHigh) stop("NLow > NHigh")
  if(!all(c(NLow,NHigh) %in% c(possN,Inf))) stop("NLow and/or NHigh not a valid N")

  possibleNs <- length(possN[possN > NLow & possN < NHigh])

  nextN <- NULL
  continueN <- TRUE

  # Use approximation function if no special case occurs
  if (N == 2 && NHigh == 2 && NMax != 2 && resultsSSD$certainty[resultsSSD$N == N]) {
    nextN <- NHigh
    continueN <- FALSE
  } else if (possibleNs == 0 && all(c(NLow, NHigh) %in% onlyCertain$N)) {
    nextN <- NHigh
    continueN <- FALSE
  } else {
    data <- resultsSSD
    if (nrow(data) < 2 || all(data$power==1)) {
      data <- rbind(data, c(2, 1, 0, 0, 0))
    }

    #Get next N candidate according to power~N approximation
    nextN <- nextN.approx(data=data, powerDesired=powerDesired, NMax=NMax)

    #If proposed N is lower than a certain N with tendency < 1
    nextN <- nextN.verifyNCandidatePotentialLow(resultsSSD=resultsSSD, nextN=nextN)

    #Is next N candidate in limits
    if (nextN < NMin){
      nextN <- NMin
    }else if (nextN < 2) {
      nextN <- 2
    }else if (nextN > NMax) {
      nextN <- NMax
    }


    #Is next N candidate certain, choose uncertain close neighbor
    continueN <- T
    onlyCertain <- resultsSSD[resultsSSD$certainty==T,]
    if (possibleNs == 0 && all(c(NLow, NHigh) %in% onlyCertain$N)){
      continueN <- F
    }else{
      nextN <- nextN.verifyNCandidate(resultsSSD=resultsSSD, nextN=nextN)
    }
  }

  #If factorN is used (e.g., equally distributed across groups), return a close sample size.
  if(!nextN %in% possN){
    nLow <- NMin
    nHigh <- NMax
    if(length(possN[possN<nextN]) > 0) nLow <- max(possN[possN<nextN])
    if(length(possN[possN>nextN]) > 0) nHigh <- min(possN[possN>nextN])
    nextN <- getValidNextN(nLow, nHigh, resultsSSD, N)
  }

  if (nextN < NMin){
    continueN <- F
    nextN <- NMin
  }
  result <- list(nextN, continueN)

  return(result)
}

#' Set certainty of Ns to FALSE in resultsSSD$certain if there is oscillation.
#' Set certainty to FALSE for N values with a tendency of 0 or -1 if a smaller N has a tendency of 1 and more iterations.
#' Set certainty to FALSE for N values with a tendency of 1 if a higher N has a tendency of -1 and more iterations.
#' @noRd
nextN.reviseCertainty <- function(resultsSSD){
  onlyCertain <- resultsSSD[resultsSSD$certainty==T,]

  if(length(unique(onlyCertain$tendency)) > 1){
    high <- onlyCertain[onlyCertain$tendency==1,]
    low <- onlyCertain[onlyCertain$tendency==-1,]

    for(nn in low$N){
      nnI <- resultsSSD$i[resultsSSD$N==nn]
      resultsSSD$certainty[resultsSSD$tendency > -1 & resultsSSD$N < nn & resultsSSD$i <= nnI] <- F
    }
    for(nn in high$N){
      nnI <- resultsSSD$i[resultsSSD$N==nn]
      resultsSSD$certainty[resultsSSD$tendency < 1 & resultsSSD$N > nn & resultsSSD$i <= nnI] <- F
    }
  }
  return(resultsSSD)
}


#' Updates 'NLow' and 'NHigh' with Ns lower/higher the desired power that are certain.
#' @noRd
nextN.lowAndHigh <- function(resultsSSD, NLow, NHigh){
  if (1 %in% resultsSSD$tendency){
    certainHighs <- resultsSSD$N[resultsSSD$tendency > 0 & resultsSSD$certainty]
    if(length(certainHighs) > 0)
      NHigh <- min(certainHighs)
  }
  if (any(c(0,1) %in% resultsSSD$tendency)){
    certainLows <- resultsSSD$N[resultsSSD$tendency == -1  & resultsSSD$certainty]

    if(length(certainLows) > 0)
      NLow <- max(certainLows)
  }
  return(c(NLow, NHigh))
}


#' Returns a sample size candidate using a power~N approximation
#' (non-linear, or linear if it fails, or a simple rule when both approximations fail).
#' @noRd
nextN.approx <- function(data, powerDesired, NMax){
  nextN <- NULL
  try({
    fit <- optim(par = c(1.01, 0.01), lower = c(1.001, 0.001),
                 method = "L-BFGS-B", fn = squaredPowerDiff,
                 N = data$N, power = data$power, weights = data$i)
    params <- fit$par
    aApprox <- params[1]
    bApprox <- params[2]
    nextN <- round(NFct(powerDesired, aApprox, bApprox))
  })

  if(is.null(nextN)){
    try({
      fit <- glm(N ~ power, family = poisson(), data = data, weights = data$i)
      b0 <- fit$coefficients[1]
      b1 <- fit$coefficients[2]
      nextN <- ceiling(exp(b0+b1*powerDesired))
    })
  }

  if (is.null(nextN)){
    if (all(data$power==1)){
      nextN <- min(data$N)-1
    }else{
      nextN <- NMax+1
    }
  }

  return(nextN)
}


#' If a sample size candidate is already certain, choose a close one.
#' @noRd
nextN.verifyNCandidate <- function(resultsSSD, nextN){
  Ns <- c()
  count <- 0
  while(length(resultsSSD$certainty[resultsSSD$N == nextN]) > 0 &&
        resultsSSD$certainty[resultsSSD$N == nextN]){
    count <- count +1
    if (resultsSSD$tendency[resultsSSD$N == nextN] == 0){
      if (nextN %in% Ns) break
      nextN <- nextN+1
    }else if (resultsSSD$tendency[resultsSSD$N == nextN] < 0){
      nextN <- nextN+1
    }else if (resultsSSD$tendency[resultsSSD$N == nextN] > 0){
      nextN <- nextN-1
    }
    Ns <- c(Ns, nextN)

    if (sum(nextN==Ns) > 2){
      stop("Can't determine next N.")
    }
  }

  return(nextN)
}


#' If a sample size candidate is smaller than a certain with tendency < 1 return max certain low
#' @noRd
nextN.verifyNCandidatePotentialLow <- function(resultsSSD, nextN){
  onlyCertainTendencyLow <- resultsSSD[resultsSSD$certainty & resultsSSD$tendency < 1,]
  if(length(onlyCertainTendencyLow$N) > 0 &&
     max(onlyCertainTendencyLow$N) > nextN)
    return(max(onlyCertainTendencyLow$N))
  return(nextN)
}


#' If the sample size is equally distributed across groups, return a potentially suitable sample size.
#' @noRd
getValidNextN <- function (nLow, nHigh, resultsSSD, nextN) {
  diffLow <- abs(nextN-nLow)
  diffHigh <- abs(nextN-nHigh)
  favN <- nLow
  if(diffLow > diffHigh)
    favN <- nHigh

  if(!favN %in% resultsSSD$N) return(favN)

  if(resultsSSD$certainty[resultsSSD$N==favN]){
    if(favN==nHigh || (nHigh %in% resultsSSD$N==nHigh &&
                       resultsSSD$certainty[resultsSSD$N==nHigh])) return(nLow)
    return(nHigh)
  }

  return(favN)
}

