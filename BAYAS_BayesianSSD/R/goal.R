
#' @title
#' Single goal for a sample size determination
#'
#'@description
#' Creates a single goal for a sample size determination passed to \code{runSSD}.
#'
#'@usage
#'\code{createGoal(
#'   parametersA = c(),
#'   parametersB = NULL,
#'   goalType = c("rope","precision"),
#'   ci = 0.95,
#'   ropeType = c("exclude","include"),
#'   ropeLower = NULL,
#'   ropeUpper = NULL,
#'   ropeExclusive = T,
#'   precisionWidth = NULL
#')}
#'
#' @param parametersA A vector of parameters defining the first 'group' of posteriors.
#' @param parametersB A vector of parameters defining the second 'group' of posteriors.
#' The difference of the two posterior sums defined by \code{parametersA} and \code{parametersB} (if set, otherwise 0) is the target posterior.
#' @param goalType One of the two goal types \code{"rope", "precision"}.
#' @param ci The target credible interval of the posterior.
#' @param ropeType One of the two rope types \code{"exclude", "include"}.
#' For \code{ropeType="exclude"} the credible interval must be outside the ROPE.
#' If \code{ropeType="include"}, the credible interval must be inside the ROPE.
#' Used only when \code{goalType="rope"}.
#' @param ropeLower Lower bound of the ROPE. Used only when \code{goalType="rope"}.
#' @param ropeUpper Upper bound of the ROPE. Used only when \code{goalType="rope"}.
#' @param precisionWidth The maximum allowed width of the credible interval. Used only when \code{goalType="precision"}.
#' @param ropeExclusive Used only for a ROPE and \code{'exclude'}, default TRUE.
#' If set to TRUE: When the posterior extends beyond both bounds of the ROPE, the larger fraction of the posterior outside the ROPE is used.
#' If set to FALSE: The difference between the two fractions outside the ROPE is calculated.
#'
#' @returns A goal that can be passed to \code{runSSD}.
#' @export
#'
#' @examples \code{
#' createGoal(
#' parametersA="mu[1]", parametersB="mu[2]",
#' goalType="rope", ropeType="exclude",
#' ropeLower=-0.1, ropeUpper = 0.1, ropeExclusive=T,
#' ci=0.95)}
createGoal <- function(
    parametersA, parametersB=NULL,
    goalType=c("rope","precision"), ci=0.95,
    ropeType=c("exclude","include"), ropeLower=NULL, ropeUpper=NULL,
    ropeExclusive=T,
    precisionWidth=NULL){

  if (length(goalType) != 1 || !goalType %in% c("rope","precision"))
    stop("goalType must be one of 'rope' or 'precision'")

  goal <- list(
    parametersA=c(),
    parametersB=c(),
    type = goalType,
    hdi = NULL,
    precWidth = NULL,
    ropeExclude = "",
    ropeLower = NULL,
    ropeUpper = NULL,
    ropeExclusive = NULL
  )

  if (goalType=="rope"){
    if (length(ropeType) != 1 || !ropeType %in% c("exclude","include"))
      stop("ropeType must be one of 'exclude' or 'include'")
    if (is.null(ropeLower)) stop("ropeLower must be set.")
    if (is.null(ropeUpper)) stop("ropeUpper must be set.")
    if (!is.numeric(ropeLower)) stop("ropeLower must be numeric.")
    if (!is.numeric(ropeUpper)) stop("ropeUpper must be numeric.")
    if (ropeLower > ropeUpper) stop("ropeLower must be smaller than ropeUpper.")
    if (ropeType == "exclude"){
      if (!is.logical(ropeExclusive) || is.na(ropeExclusive))
        stop("ropeExclusive must be TRUE or FALSE")
      goal$ropeExclusive <- ropeExclusive
    }
    goal$ropeExclude <- ropeType
    goal$ropeLower <- ropeLower
    goal$ropeUpper <- ropeUpper
  }else{
    if (is.null(precisionWidth)) stop("precisionWidth must be set.")
    if (!is.numeric(precisionWidth)) stop("precisionWidth must be numeric.")
    if (precisionWidth<=0) stop("precisionWidth must be positive.")
    goal$precWidth <- precisionWidth
  }

  if (is.null(ci))  stop("ci must be set.")
  if (!is.numeric(ci)) stop("ci must be numeric.")
  if (ci>1 || ci<0) stop("ci must be in the range [0,1].")
  goal$hdi <- ci

  if (is.null(parametersA)) stop("parametersA must be set.")
  goal$parametersA <- parametersA
  goal$parametersB <- parametersB

  class(goal) <- c("bayesianssdgoal",class(goal))
  return(goal)
}




#' @title
#' Plot goals for a sample size determination
#'
#'@description
#' Plots all goals created by \code{createGoal}.
#' If the additional arguments are provided, the goals are plotted alongside example data.
#'
#'@usage
#'\code{createGoal(
#'   parametersA = c(),
#'   parametersB = NULL,
#'   goalType = c("rope","precision"),
#'   ci = 0.95,
#'   ropeType = c("exclude","include"),
#'   ropeLower = NULL,
#'   ropeUpper = NULL,
#'   ropeExclusive = T,
#'   precisionWidth = NULL
#')}
#'
#' @param goals The condition be tested. Use the function \link[bayesianssd]{createGoal} to create such goals.
#' @param dataCreationFunction A function that accepts a single parameter, \code{N}, and generates N values in the same manner as the given \code{model}.
#' @param model A object of class \code{stanmodel, stanreg or brmsfit}.
#' @param N The exemplary sample size.
#'
#' @returns A gtable object.
#' @export
#'
#' @examples
#' dataCreationFunction <- function(N){
#'   group_effects <- c(
#'     control = 9,
#'     drug = 7
#'   )
#'   treatment <- rep(c("control", "drug"), length.out=N)
#'   y <- rpois(N, group_effects)
#'
#'   data <- data.frame(
#'     treatment = treatment,
#'     y = y
#'   )
#'   data
#' }
#'
#' model <- stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())
#'
#' goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
#'                    goalType="rope", ropeType="exclude", ropeLower=-0.1, ropeUpper = 0.1,
#'                    ropeExclusive=T, ci=0.95)
#' plotGoal(goal, dataCreationFunction, model, N=100)
plotGoal <- function(goals, dataCreationFunction=NULL, model=NULL, N=NULL){
  goals <- singleGoalsAsList(goals)

  verifySSDInputs.goal(goals)

  fit <- plotGoal.fit(goals, dataCreationFunction, model, N)

  ggs <- list()

  colors <- bayesianSSDColors()
  colGen <- "color4"

  ggs <- lapply(seq_along(goals), function(i){
    goal <- goals[[i]]
    genData <- NULL
    if(!is.null(fit)){
      params <- as.matrix(fit)
      gA <- rep(0, length=dim(params)[1])
      gB <- rep(0, length=dim(params)[1])
      for(p in goal$parametersA){
        gA <- gA+params[, p]
      }
      for(p in goal$parametersB){
        gB <- gB+params[, p]
      }
      genData <- gA-gB
    }

    gg <- ggplot()

    if (goal$type=="rope"){
      gg <- plotGoal.rope(colors, genData, goal)
    }else if (goal$type=="precision"){
      gg <- plotGoal.precision(colors, genData, goal)
    }
    gg
  })

  n <- length(ggs)
  nCol <- floor(sqrt(n))
  suppressWarnings(do.call("grid.arrange", c(ggs, ncol=nCol)))
}

#' Plots a single goal of type 'rope'.
#' @noRd
plotGoal.rope <- function(colors, genData, goal){
  gg <- ggplot()
  gg <- gg +
    scale_fill_manual(breaks = c("color1","color2","color3","color4"),
                      values=c(colors$goal1,
                               colors$goal2,
                               colors$goal3,
                               colors$goal4),
                      name="Model", label=c("","","","Data generation"))

  cc1 <- 1
  color <- colors$rope1

  if(!is.null(genData)){
    densGen <- density(genData)
    limits <- hdi(genData, credMass=goal$hdi)
    genData <- genData[genData>=limits[1] & genData<=limits[2]]
    bins <- min(50, length(unique(genData)))


    if(goal$ropeExclude == "include") color <- colors$rope2
    colGen <- "color4"

    gg <- gg +
      geom_histogram(data=data.frame(x=genData, col=colGen),
                     aes(x=x, fill=colGen),
                     bins=bins,
                     alpha=0.5)
    lims <- ggplot_build(gg)$layout$panel_scales_x[[1]]$range$range
    gg <- gg +
      xlim(c(min(lims[1],goal$ropeLower), max(lims[2],goal$ropeUpper)))

    ggb <- ggplot_build(gg)
    ggb_data <- ggb$data
    cc1 <- max(ggb_data[[1]]$count)
  }

  gg <- gg +
    geom_rect(aes(xmin = goal$ropeLower, xmax = goal$ropeUpper,
                  ymin = 0, ymax = cc1*1.1), alpha=0.3,
              color=color, fill=color)

  gg <- gg + xlab("") + ylab("") +
    scale_y_continuous(breaks=NULL)  +
    theme(legend.position="none")
  gg
}

#' Plots a single goal of type 'precision'.
#' @noRd
plotGoal.precision <- function(colors, genData, goal){
  if(is.null(genData))
    genData <- rnorm(1e3)

  limits <- hdi(genData, credMass=goal$hdi)

  if(is.na(limits[1])){
    if(goal$hdi < 0.5){
      limits[1] <- -0.001
    }else{
      limits[1] <- min(genData)
    }
  }
  if(is.na(limits[2])){
    if(goal$hdi < 0.5){
      limits[2] <- 0.001
    }else{
      limits[2] <- max(genData)
    }
  }

  data <- data.frame(
    x = genData,
    col="color4"
  )
  data$col[data$x <= limits[1]] <- "color1"
  data$col[data$x >= limits[2]] <- "color1"
  bins <- 50

  breaks <- seq(from=min(genData), to=max(genData), length=bins-2)
  breaks <- c(breaks, limits[1], limits[2])
  breaks <- unique(breaks)

  gg <- ggplot()
  gg <- gg +
    geom_histogram(data=data,
                   aes(x=x, fill=col, color=col),
                   breaks=breaks,
                   alpha=0.5) +
    scale_fill_manual(breaks = c("color1","color2","color3", "color4"),
                      values=c(colors$goal1,
                               colors$goal2,
                               colors$goal3,
                               colors$goal4)) +
    scale_color_manual(breaks = c("color1","color2","color3", "color4"),
                       values=c(colors$goal1,
                                colors$goal2,
                                colors$goal3,
                                colors$goal4))


  ggb <- ggplot_build(gg)
  ggb_data <- ggb$data
  cc1 <- ggb_data[[1]]$count
  height <- max(cc1) * 0.1

  gg <- gg +
    geom_segment(aes(x = limits[1], y = height,
                     xend = limits[2], yend = height),
                 linewidth=2) +
    annotate("text", x = mean(genData), y=height*0.5, size = 5,
             label=paste0("\u2264 ", goal$precWidth))

  gg <- gg + ylab("") +
    scale_y_continuous(breaks=NULL) +
    theme(legend.position = "none")

  gg
}

#' If provided, fits the model to an example dataset of length 'N' generated by 'dataCreationFunction'.
#' @noRd
plotGoal.fit <- function(goals, dataCreationFunction=NULL, model=NULL, N=NULL){
  check <- c(!is.null(dataCreationFunction),!is.null(model),!is.null(N))
  fit <- NULL
  if(any(check)){
    if(!all(check)){
      warning("If any argument of 'dataCreationFunction', 'model', or 'N' is set, all of them must be set.")
    }else{

      check <- checkSettings(model, dataCreationFunction, N, goals)
      if(!check){
        stop("Invalid inputs, check with 'checkSettings'.")
      }

      try({
        data <- dataCreationFunction(N)
      })
      if(is.null(data)){
        stop("Couldn't generate data with the provided 'dataCreationFunction' and 'N'")
      }
      fit <- NULL
      try({
        if ("stanmodel" %in% class(model)){
          fit <- sampling(model, data=data)
        }else if ("stanreg" %in% class(model)){
          fit <- update(model, data=data)
        }else{
          fit <- update(model, newdata=data)
        }
      })
      if(is.null(data)){
        stop("Couldn't fit the provided model.")
      }
    }
  }
  return(fit)
}
