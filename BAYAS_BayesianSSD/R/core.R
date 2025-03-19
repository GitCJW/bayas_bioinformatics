#' Runs a sample size determination
#'
#' This function determines the sample size required for a well-defined model based on a specific condition or 'goal'.
#'
#'@usage
#'\code{runSSD(
#'  model,
#'  dataCreationFunction,
#'  powerDesired = 0.8,
#'  minN = 2,
#'  maxN = 100,
#'  factorN = 1,
#'  goals,
#'  con = 200,
#'  iParallel = 10)}
#'
#' @param model An object of class \code{stanmodel, stanreg or brmsfit}.
#' @param dataCreationFunction A function that accepts a single parameter, \code{N}, and should generates N values in the same manner as the given \code{model}.
#' @param powerDesired The probability of achieving the goal (condition) when the experiment is repeated.
#' @param minN The minimum number of data points. If your data contains e.g. 4 groups the minimum number should be at least 4.
#' @param maxN The maximum allowable sample size. Larger values will increase the time required for the determination.
#' @param factorN Set this factor when your data includes groups that should be treated equally.
#' For example, if your \code{dataCreationFunction} generates data for a control group and two treatment groups,
#' you can use a factor of 3 to ensure that each group has the same number of samples. Alternatively, you can provide a vector of permitted N values.
#' @param goals The condition be tested. Use the function \link[bayesianssd]{createGoal} to create such goals.
#' @param con The rough number of simulations needed for each sample size. Default: 200
#' @param iParallel Number of runs in parallel, if an appropriate strategy is chosen (check \link[future]{plan}). Default: 20.

#' @returns A object containing all information about the sample size determination.
#' @export
#'
#' @examples \code{
#'dataCreationFunction <- function(N){
#'  group_effects <- c(
#'    control = 13,
#'    drug = 7
#'  )
#'  treatment <- rep(c("control", "drug"), length.out=N)
#'  y <- rpois(N, group_effects)
#'
#'  data <- data.frame(
#'    treatment = treatment,
#'    y = y
#'  )
#'  data
#'}
#'
#'model <- stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())
#'
#'goal <- createGoal(parametersA = "treatmentcontrol", parametersB = "treatmentdrug",
#'                   goalType = "rope", ropeType = "exclude", ropeLower = 0, ropeUpper = 0, ci = 0.95)
#'
#'checkSettings(model, dataCreationFunction, 2, list(goal))
#'
#'plan("multisession")
#'ssd <- runSSD(
#'  model = model,
#'  dataCreationFunction = dataCreationFunction,
#'  powerDesired = 0.8,
#'  minN = 2,
#'  maxN = 20,
#'  goals = list(goal),
#'  con = 200,
#'  iParallel = 20)
#'
#'plotResults(ssd)
#'printSSD(ssd)}
#'
runSSD <- function(model, dataCreationFunction, powerDesired=0.8, minN=2, maxN=100, factorN=1,
                    goals, con=200, iParallel=10){
  goals <- singleGoalsAsList(goals)

  # check inputs
  verifySSDInputs(model, dataCreationFunction, powerDesired, minN, maxN, factorN=1,
                 goals, con, iParallel)

  possN <- c()
  if(length(factorN) == 1){
      possN <- (1:(floor(maxN / factorN)))*factorN
      possN <- possN[possN >= minN]
  }else{
      possN <- factorN
      possN <- possN[possN >= minN & possN <= maxN]
  }


  print("Compiling your model and start sample size determination ...")

  ssd <- tryCatch({
    startSSD(model, dataCreationFunction, powerDesired, possN,
            goals, con, iParallel)
  },
  error = function(e){
    print(e)
    stop("")
  })


  plotResults(ssd)

  cat("Simulate .")
  while(ssd$extern$continue){
    cat(".")

    ssd2 <- tryCatch({
      updateSSD(ssd, dataCreationFunction)
    },
    error = function(e){
      print(e)
    })
    if(is.null(ssd2)) return(ssd)
    ssd <- ssd2
    print(plotResults(ssd))
  }

  cat("\n")
  print("Sample size determination finished.")

  printSSD(ssd)

  return(ssd)
}




#' Input validity check
#'
#' Checks if the input for a call to \code{run_sdd} is valid.
#'
#' @param model An object of class \code{stanmodel, stanreg or brmsfit}.
#' @param dataCreationFunction A function that accepts a single parameter, \code{N}, and should generates N values in the same manner as the given \code{model}.
#' @param minN The minimum number of data points. If your data contains e.g. 4 groups the minimum number should be at least 4.
#' @param goals The condition be tested. Use the function \link[bayesianssd]{createGoal}  to create such goals.
#'
#' @returns TRUE if everything seems to be well defined.
#' @export
checkSettings <- function(model, dataCreationFunction, minN, goals){
  goals <- singleGoalsAsList(goals)
  # check inputs
  verifySSDInputs.sub(model, dataCreationFunction, minN, goals)

  data <- dataCreationFunction(minN)

  fit <- NULL
  try({
    suppressWarnings({
      if ("stanmodel" %in% class(model)){
        fit <- sampling(model, data=data, refresh=0)
      }else if ("stanreg" %in% class(model)){
        fit <- update(model, data=data, refresh = 0)
      }else{
        fit <- update(model, newdata=data, refresh = 0)
      }
    })
  })

  if(is.null(fit)) stop("Unable to fit model")

  params <- as.matrix(fit)
  paraNames <- names(params[1,])

  for(goal in goals){
    parametersA <- goal$parametersA
    parametersB <- goal$parametersB
    if (!all(parametersA %in% paraNames))
      stop(paste0("Not all of the parameter names defined in 'ParametersA' exist. ",
                  "Available names: ", paste0(paraNames, collapse=", ")))
    if (!is.null(parametersB) && !all(parametersB %in% paraNames))
      stop(paste0("Not all of the parameter names defined in 'ParametersA' exist. ",
                  "Available names: ", paste0(paraNames, collapse=", ")))
  }
  print("Looks good!")
  return(T)
}



#' Prints sample size determination result
#'
#' @param ssd  Object returned by \code{runSSD()}.
#' @param summarized Boolean (default: TRUE) whether a summarized version of all steps should be printed.
#' @export
printSSD = function(ssd, summarized=T){
  if(is.null(ssd) || !"bayesianssd" %in% class(ssd))
    stop("'ssd' must be a object of class 'bayesianssd'.")

  text <- ""

  intern <- ssd$intern
  res <- intern$resultsSSD
  if (summarized){
    for(n in unique(res$N)){
      res <- res[res$N!=n | (res$N==n & res$i == max(res[res$N==n,]$i)),]
    }
    print(res)
  }else{
    print(res)
  }


  NLow <- 0
  if (length(res$N[res$tendency<=0])>0){
    NLow <- max(res$N[res$tendency<=0])
  }

  if (NLow == intern$maxN) {
    text <- paste0(text, paste0("The desired power of at least ", intern$powerDesired,
               " can't be reached with a maximum sample size N = ",
               intern$maxN, ". ",
               "\n",
               "Expected power with N = ",
               intern$maxN, ": ",
               res$power[res$N==intern$maxN],
               "\n",
               "Estimated N to reach the desired power: ", intern$N, ". "))
  } else {

    pB <- intern$resultsPowerBinomial
    pB <- pB[pB$N == intern$NHigh,]

    text <- paste0(text, paste0("The desired power of ", intern$powerDesired,
               " can be reached with N = ", pB$N,
               " with a 90% credible interval of [",
               round(pB$powerLow,2), ", ", round(pB$powerHigh,2), "]."))
  }
  print(text)
  return(text)
}



#' Plots the sample size determination
#'
#' @param ssd Object returned by \code{runSSD()}.
#' @param plotTriangles Boolean (default: TRUE) indicating whether to draw triangles representing the maximum accepted power width
#' @param theme A list given the colors for the plot. Default: \code{list(main="#56B4E9", current="black", default="darkgrey", mean="white")}.
#'
#' @returns A ggplot.
#' @export
plotResults <- function (ssd, plotTriangles = T,
                         theme=list(main="#56B4E9", current="black", default="darkgrey", mean="white")) {

  if(is.null(ssd) || !"bayesianssd" %in% class(ssd))
    stop("'ssd' must be a object of class 'bayesianssd'.")

  powerDesired <- ssd$intern$powerDesired
  resultsSSD <- ssd$intern$resultsSSD
  resultsPowerBinomial <- ssd$intern$resultsPowerBinomial
  furtherArgs <- ssd$intern$furtherArgs
  acceptedWidth <- furtherArgs$acceptHDIwidth

  NLastChecked <- tail(resultsSSD$N, 1)

  onlyCertainAndHigh <- resultsSSD[resultsSSD$certainty==T &
                                     resultsSSD$tendency == 1,]
  bestN <- NULL
  noBlackHighlight <- F
  if (dim(onlyCertainAndHigh)[1] > 0){
    bestN <- min(onlyCertainAndHigh$N)
    lowN <- bestN-1
    if (lowN %in% resultsSSD[resultsSSD$certainty==T &
                             resultsSSD$power<powerDesired,]$N)
      noBlackHighlight <- T
  }

  gg <- ggplot(resultsPowerBinomial) +
    theme_bw() +
    xlab("N") +
    ylab("Power") +
    geom_hline(yintercept=powerDesired, linetype="dashed", color = theme$main)

  for (n in unique(resultsSSD$N)) {
    subset <-  resultsSSD[resultsSSD$N==n,]
    subset <- tail(subset, 1)

    powerBinomN <- resultsPowerBinomial[resultsPowerBinomial$N==n,]
    subset$minPower <- powerBinomN$powerLow
    subset$maxPower <- powerBinomN$powerHigh

    col <- theme$default
    if (!noBlackHighlight && length(NLastChecked) > 0 && n==NLastChecked)
      col <- theme$current
    if (!is.null(bestN) && n==bestN) col <- theme$main

    gg <- gg + geom_pointrange(data=subset, mapping=aes(x=N,
                                                        y=power,
                                                        ymin=minPower,
                                                        ymax=maxPower),
                               shape = 21, size=1, fatten=3, linewidth=1,
                               color=col, fill=theme$mean)

    if (plotTriangles){
      acceptedWidthUpper <- mean(c(subset$minPower, subset$maxPower)) + acceptedWidth/2
      acceptedWidthLower <- mean(c(subset$minPower, subset$maxPower)) - acceptedWidth/2

      acceptedWidthData <- data.frame(N=n, acc=c(acceptedWidthUpper, acceptedWidthLower), type=c("upper","lower"))

      gg <- gg +
        geom_point(data=acceptedWidthData, mapping=aes(x=N, y=acc, shape=c(25,24)),
                   size=2, fill=theme$main, color=theme$main, alpha=0.5)

    }

  }
  gg <- gg + scale_shape_identity()
  gg
}


