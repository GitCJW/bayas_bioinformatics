#' Starts the first iteration of a sample size determination
#' @noRd
startSSD <- function(model, dataCreationFunction, powerDesired, possN, goals,
                     con, iParallel){
  goals <- singleGoalsAsList(goals)

  maxN <- max(possN)

  prob <- powerDesired
  alpha <- prob*(con-2)+1
  beta <- (1-prob)*(con-2)+1
  acceptHDI <- hdi(qbeta, shape1=alpha, shape2=beta, credMass=0.9)
  acceptHDIwidth <- acceptHDI[2] - acceptHDI[1]


  furtherArgs <- list(
    iParallel=iParallel,
    modelSeed = sample(1e5,iParallel),
    acceptHDIwidth = acceptHDIwidth,
    acceptHDIConcentration = con
  )

  initData <- lapply(1:iParallel, function(i){
    dataCreationFunction(maxN)
  })

  ssd <- initSSD(model = model, data = initData,
                 powerDesired = powerDesired, possN = possN,
                 goals = goals,
                 furtherArgs = furtherArgs)
  return(ssd)
}


#' Runs an additional iteration
#' @noRd
updateSSD <- function(ssd, dataCreationFunction){

  newData <- lapply(1:ssd$intern$furtherArgs$iParallel, function(i){
    dataCreationFunction(ssd$extern$N)
  })

  ssd <- continueSSD(ssd, newData)

  return(ssd)
}


#' Initialize sample size determination and run first iteration.
#' @noRd
initSSD <- function(model, data, powerDesired, possN, goals,
                     furtherArgs){
  minN <- min(possN)
  maxN <- max(possN)

  intern <- list(
    model = model,
    data = data,
    powerDesired = powerDesired,
    possN = possN,
    maxN = maxN,
    minN = minN,
    goals = goals,
    furtherArgs = furtherArgs,

    allCalc = list(N=numeric(0),
                   success=numeric(0),
                   goalAchievement=as.list(rep(numeric(0),length = length(goals)))),
    resultsSSD = data.frame(N = numeric(0), i = numeric(0), power = numeric(0),
                            certainty = logical(0), tendency = numeric(0)),
    resultsPowerBinomial = data.frame(N = numeric(0), powerMean = numeric(0),
                                      powerLow = numeric(0), powerHigh = numeric(0)),
    continueI = T,
    counter = 0,
    continueN = T,
    N = maxN,
    NHigh = maxN,

    NMaxTooLow = F
  )

  extern <- list(
    continue = T,
    N = maxN
  )

  ssd <- list(
    intern=intern,
    extern=extern
  )

  return(doSSD(ssd, data))
}


#' Wrapper for function 'doSSD'
#' @noRd
continueSSD <- function(ssd, newData){
  return(doSSD(ssd, newData))
}


#' Top-level function for a single SSD iteration, executing separate functions.
#' @noRd
doSSD <- function(ssd, data){
  resultsSimulation <- doMultSimulation(ssd, data)

  ssd <- storeGoalResult(ssd, resultsSimulation)

  ssd <- storeNResult(ssd)

  ssd <- proceedSimulation(ssd)

  class(ssd) <- c("bayesianssd", class(ssd))

  return(ssd)
}


#' Runs several fits in parallel (if available)
#' @noRd
doMultSimulation <- function(ssd, data){
  resultsSimulation <- foreach(
    sim = 1:ssd$intern$furtherArgs$iParallel,
    .combine = 'rbind',
    .options.future = list(seed = TRUE)
  ) %dofuture% {
    mem <- sum(c(object.size(ssd$intern$model),
                 object.size(data),
                 object.size(ssd$intern$goals),
                 object.size(ssd$intern$furtherArgs$modelSeed)))
    ret <- doSimulation(model = ssd$intern$model, data = data[[sim]], goals = ssd$intern$goals,
                        modelSeed = ssd$intern$furtherArgs$modelSeed[[sim]])
    ret
  }
  resultsSimulation
}


#' A single fit and goal checking within an iteration
#' @noRd
doSimulation <- function(model, data, goals, modelSeed) {
  fit <- suppressMessages(fitModel(model, data, modelSeed))
  if(is.null(fit)) stop("Couldn't fit model")

  summ <- summary(fit)
  params <- as.matrix(fit)

  counterVal <- rep(0, length.out = length(goals))
  goalAchievement <- rep(0, length.out = length(goals))
  #Check each single goal
  for(gI in seq_along(goals)){
    goal <- goals[[gI]]
    ret <- checkGoal(goal, params)
    goalAchievement[gI] <- ret$goalAchievement
    counterVal[gI] <- ret$counterVal
  }

  result <- list(counterValue=as.numeric(all(counterVal==1)),
                 goalAchievement = goalAchievement)
  return(result)
}


#' Fits a model
#' @noRd
fitModel <- function(model, data, modelSeed){

  # fit model
  fit <- tryCatch({
    if ("stanmodel" %in% class(model)){
      fit <- sampling(model, data=data, seed=modelSeed, refresh=0)
    }else if ("stanreg" %in% class(model)){
      fit <- update(model, data=data, refresh = 0, seed=modelSeed)
    }else if("brmsfit" %in% class(model)){
      fit <- update(model, newdata=data, refresh = 0, seed=modelSeed)
    }else{
      stop("Unknown model")
    }
    fit
  }, error = function(i){
    stop("Unable to fit the model")
  })
  return(fit)
}


#' Stores results of goal achievements
#' @noRd
storeGoalResult <- function(ssd, resultsSimulation){
  for (value in 1:ssd$intern$furtherArgs$iParallel){
    ssd$intern$allCalc$N <- c(ssd$intern$allCalc$N, ssd$intern$N)
    ssd$intern$allCalc$success <- c(
      ssd$intern$allCalc$success,
      resultsSimulation[value,1][[1]])

    for(gI in seq_along(ssd$intern$allCalc$goalAchievement)){
      if (length(ssd$intern$allCalc$goalAchievement[[gI]]) == 1 &&
          is.na(ssd$intern$allCalc$goalAchievement[[gI]])){
        ssd$intern$allCalc$goalAchievement[[gI]] <- resultsSimulation[value,2][[1]][gI]
      }else{
        ssd$intern$allCalc$goalAchievement[[gI]] <- c(
          ssd$intern$allCalc$goalAchievement[[gI]],
          resultsSimulation[value,2][[1]][gI])
      }
    }
  }
  return(ssd)
}


#' Stores result of current N and its power
#' @noRd
storeNResult <- function(ssd){
  ## Certainty of power for N
  successes <- ssd$intern$allCalc$success[ssd$intern$allCalc$N==ssd$intern$N]
  ret <- powerEstimator(successes)

  certainty <- ret$pwrWidth <= ssd$intern$furtherArgs$acceptHDIwidth
  tendency <- 0
  #Too low
  if (ssd$intern$powerDesired >= ret$pwrMidHigh){
    tendency <- -1
  } else if (ssd$intern$powerDesired <= ret$pwrMean){#high
    tendency <- 1
  }
  ssd$intern$continueI <- tendency==0 && !certainty


  # store values for this N
  nSuccess <- ssd$intern$allCalc$success[ssd$intern$allCalc$N==ssd$intern$N]
  power <- sum(nSuccess)/length(nSuccess)
  i <- ssd$intern$furtherArgs$iParallel
  if (ssd$intern$N %in% ssd$intern$resultsSSD$N){
    i <- i + ssd$intern$resultsSSD$i[ssd$intern$resultsSSD$N==ssd$intern$N]
  }
  tR <- data.frame(N=ssd$intern$N, i=i, power=tail(power,1),
                   certainty=certainty, tendency=tendency)
  ssd$intern$resultsSSD <- ssd$intern$resultsSSD[ssd$intern$resultsSSD$N!=ssd$intern$N,]
  ssd$intern$resultsSSD <- rbind(ssd$intern$resultsSSD, tR, make.row.names=F)

  pB <- data.frame(
    N = ssd$intern$N,
    powerMean=ret$pwrMean,
    powerLow = ret$pwrLow,
    powerHigh = ret$pwrHigh)
  rPB <- ssd$intern$resultsPowerBinomial
  rPB <- rPB[rPB$N != ssd$intern$N,]
  ssd$intern$resultsPowerBinomial <- rbind(rPB, pB, make.row.names=F)

  return(ssd)
}


#' Check whether to proceed with SSD
#' @noRd
proceedSimulation <- function(ssd){
  continue <- ssd$intern$continueI

  if (!ssd$intern$continueI){
    getNextN <- nextN(ssd)

    ssd$intern$N <- getNextN[[1]]
    ssd$intern$continueN <- getNextN[[2]]

    if (ssd$intern$maxN %in% ssd$intern$resultsSSD$N[ssd$intern$resultsSSD$certainty & ssd$intern$resultsSSD$tendency < 1]) {
      ssd$intern$NMaxTooLow <- TRUE
      ssd$intern$continueN <- FALSE

      maxNTmp <- ssd$intern$maxN
      ssd$intern$maxN <- Inf
      getNextN <- nextN(ssd)
      ssd$intern$maxN <- maxNTmp
      ssd$intern$N <- getNextN[[1]]
    }
    continue <- ssd$intern$continueN

    ssd$intern$continueI <- TRUE
    ssd$intern$counter <- 0
  }

  ssd$intern$NHigh <- ssd$intern$maxN
  if (length(ssd$intern$resultsSSD$N[ssd$intern$resultsSSD$tendency > 0]) > 0){
    ssd$intern$NHigh <- min(ssd$intern$resultsSSD$N[ssd$intern$resultsSSD$tendency > 0])
  }
  ssd$extern$N <- ssd$intern$N
  ssd$extern$continue <- continue

  return(ssd)
}


#' Checks whether a single goal is fulfilled
#' @noRd
checkGoal <- function(goal, params){
  goalAchievement <- NULL
  counterVal <- NULL
  gA <- rep(0, length=dim(params)[1])
  gB <- rep(0, length=dim(params)[1])
  for(p in goal$parametersA){
    gA <- gA+params[, p]
  }
  for(p in goal$parametersB){
    gB <- gB+params[, p]
  }
  gDiff <- gA-gB
  hdiTarget <- hdi(gDiff, credMass=goal$hdi)

  if (goal$type == "rope") {
    cdfTarget <- ecdf(gDiff)

    if (goal$ropeExclude == "exclude") {
      leftFromROPE <- cdfTarget(goal$ropeLower)
      rightFromROPE <- 1-cdfTarget(goal$ropeUpper)
      if (goal$ropeExclusive){
        goalAchievement <- max(leftFromROPE, rightFromROPE)
      }else{
        goalAchievement <- abs(leftFromROPE-rightFromROPE)
      }

    } else if (goal$ropeExclude == "include") {
      integralROPE <- cdfTarget(goal$ropeUpper)-cdfTarget(goal$ropeLower)
      goalAchievement <- integralROPE
    }

    if (goalAchievement >= goal$hdi) {
      counterVal <- 1
    } else {
      counterVal <- 0
    }

  } else if (goal$type == "precision") {
    hdiWidth <- abs(hdiTarget[[2]]-hdiTarget[[1]])
    deltaWidths <- goal$precWidth-hdiWidth
    goalAchievement <- plogis(deltaWidths/goal$precWidth)

    if (goalAchievement >= 0.5) {
      counterVal <- 1
    } else {
      counterVal <- 0
    }
  }

  if (goalAchievement > 1 || goalAchievement < 0) {
    stop("goalAchievement shouldn't be <0 or >1")
  }
  return(list(goalAchievement=goalAchievement, counterVal=counterVal))
}
