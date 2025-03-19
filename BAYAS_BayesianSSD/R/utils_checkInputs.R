
#' Checks all input arguments for the function 'runSSD'.
#' @noRd
verifySSDInputs <- function(model, dataCreationFunction, powerDesired, minN,
                            maxN, factorN=1, goals, con, iParallel){

  verifySSDInputs.sub(model, dataCreationFunction, minN, goals)

  if(!is.numeric(powerDesired) || powerDesired <= 0 || powerDesired >= 1)
    stop("Please provide 'powerDesired' as a numeric between (0,1).")

  if(!is.numeric(maxN) || maxN <= 0)
    stop("Please provide 'maxN' as a natural number.")

  if(maxN < minN)
    stop("Please provide 'maxN' as a natural number greater 'minN'.")

  if(length(factorN) == 1){
    if(!is.numeric(factorN) ||
       factorN != round(factorN) ||
       factorN <=0)
      stop("The argument 'factorN' has to be a single natural number or a vector of numbers.")

  }else if(any(!is.numeric(factorN)) ||
           any(factorN != round(factorN)) ||
           any(factorN <0)){
    stop("The argument 'factorN' has to be a single natural number or a vector of numbers.")
  }

  if(!is.numeric(con) || con <= 0 )
    stop("Please provide 'con' as a natural number.")

  if(con < 10 )
    warning("'con' should be at least 10, better >100.")

  if(!is.numeric(con) || con <= 0 )
    stop("Please provide 'con' as a natural number.")

  if(!is.numeric(iParallel) || iParallel <= 0 )
    stop("Please provide 'iParallel' as a natural number.")

  return(T)
}

#' Checks a subset of input arguments for the function 'runSSD'.
#' @noRd
verifySSDInputs.sub <- function(model, dataCreationFunction, minN, goals){

  if(!any(c("stanmodel","stanreg","brmsfit") %in% class(model)))
    stop("Please provide a 'model' of class 'stanmodel', 'stanreg' or 'brmsfit'")

  if(!is.function(dataCreationFunction))
    stop("Please provide 'dataCreationFunction' as a function.")

  if(!"N" %in% names(formals(dataCreationFunction)))
    stop("Please provide 'dataCreationFunction' as a function with a single argument 'N'.")

  if(!is.numeric(minN) || minN <= 0 )
    stop("Please provide 'minN' as a natural number.")

  verifySSDInputs.goal(goals)
  return(T)
}

#' Checks the 'goals' argument for e.g. the function 'runSSD'.
#' @noRd
verifySSDInputs.goal <- function(goals){
  if(is.null(goals) || !"list" %in% class(goals))
    stop("Please provide 'goals' as a list of goals created by 'createGoal'.")

  for(goal in goals){
    if(!"bayesianssdgoal" %in% class(goal))
      stop("Please provide 'goals' as a list of goals created by 'createGoal'.")
  }
  return(T)
}

#' Converts an object of class 'bayesianssdgoal' into a list of that object.
#' @noRd
singleGoalsAsList <- function(goal){
  if(!is.list(goal) || !"bayesianssdgoal" %in% class(goal[[1]])){
    goal <- list(goal)
  }
  return(goal)
}
