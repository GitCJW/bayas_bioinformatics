

#Init workers
initSSD_NGoals = function(strategy="multisession", maxCores=NULL){

  if(is.null(maxCores)) maxCores <- detectCores()-1
  cluster <- future::plan(strategy=strategy, workers = min(maxCores,detectCores()-1))

  return(cluster)
}


#startSSD_NGoals
startSSD_NGoals <- function (model, data, power_desired, maxN, minN, goals, cluster,
                             fit_binom, furtherArgs) {

  warning <- ""

  ## conditions for N
  N_max <- maxN
  N_min <- minN
  N <- N_max
  N_high <- N_max

  continue_N <- TRUE
  NMaxTooLow <- FALSE

  ## conditions for i
  i_parallel <- furtherArgs$i_parallel
  continue_i <- TRUE
  counter <- 0

  model_seed <- furtherArgs$model_seed


  ## store results
  resultsSSD <- data.frame(N=numeric(0), i=numeric(0), power=numeric(0),
                           certainty=logical(0), tendency=numeric(0))

  allCalc <- list(N=numeric(0),
                  success=numeric(0),
                  goalAchievement=as.list(rep(numeric(0),length=length(goals))))

  resultsPowerBinomial <- data.frame(N=numeric(0), power=numeric(0))


  # ret <- doSimulation_NGoals(model=model, data=data[[1]],
  #                            goals=goals, model_seed = model_seed[[1]])

  results_simulation <- foreach(
    sim = 1:i_parallel,
    .options.future = list(seed = TRUE),
    .combine = 'rbind'#,
    # .packages = c("bayestestR", "rstan")
  ) %dofuture% {
    mem <- sum(c(object.size(model),
                 object.size(data),
                 object.size(goals),
                 object.size(model_seed)))
    print(paste0("size: ", mem))
    ret <- doSimulation_NGoals(model=model, data=data[[sim]],
                        goals=goals, model_seed = model_seed[[sim]])
    ret
  }


  for (value in 1:i_parallel){
    allCalc$N <- c(allCalc$N, N)
    allCalc$success <- c(allCalc$success, results_simulation[value,1][[1]])
    for(g_i in seq_len(length(allCalc$goalAchievement))){
      if(length(allCalc$goalAchievement[[g_i]]) == 1 && is.na(allCalc$goalAchievement[[g_i]])){
        allCalc$goalAchievement[[g_i]] <- results_simulation[value,2][[1]][g_i]
      }else{
        allCalc$goalAchievement[[g_i]] <- c(allCalc$goalAchievement[[g_i]],
                                            results_simulation[value,2][[1]][g_i])
      }
    }
  }


  ######################################
  ## Binomial model instead of a beta ##
  ######################################

  successes <- allCalc$success[allCalc$N==N]
  data <- list(y = successes,
               N = length(successes))
  fit_binom_t <- sampling(fit_binom, data=data, refresh=0, seed=model_seed[1])
  ex <- rstan::extract(fit_binom_t)
  power_estimated <- ex$p

  beta_power_90hdi <- hdi(ex$p, ci=0.90)
  beta_power_width <- beta_power_90hdi$CI_high-beta_power_90hdi$CI_low
  beta_power_low <- beta_power_90hdi$CI_low
  beta_power <- mean(ex$p)
  beta_power_high <- beta_power_90hdi$CI_high
  power_beta <- ex$p


  tendency <- 0
  certainty <- beta_power_width <= furtherArgs$accept_HDIwidth

  #Too low
  if(power_desired >= beta_power_high){
    tendency <- -1
  }
  #high
  else if(power_desired <= beta_power){
    tendency <- 1
  }
  continue_i <- tendency==0 && !certainty



  # store values for this N
  N_success <- allCalc$success[allCalc$N==N]
  power <- sum(N_success)/length(N_success)
  i <- i_parallel
  if(N %in% resultsSSD$N){
    i <- i + resultsSSD$i[resultsSSD$N==N]
  }

  tR <- data.frame(N=N, i=i, power=tail(power,1),
                   certainty=certainty, tendency=tendency)
  resultsSSD <- resultsSSD[resultsSSD$N!=N,]
  resultsSSD <- rbind(resultsSSD, tR)



  pB <- data.frame(N=N, power=power_beta)
  resultsPowerBinomial <- resultsPowerBinomial[resultsPowerBinomial$N!=N,]
  resultsPowerBinomial <- rbind(resultsPowerBinomial, pB)

  continue <- continue_i

  if(!continue_i){
    get_next_N <- nextN_logisticApprox_linearOnError_NGoals(power_desired,
                                                     N, N_max, N_min,
                                                     resultsSSD, seed=model_seed[1])

    N <- get_next_N[[1]]
    continue_N <- get_next_N[[2]]

    if (N_max %in% resultsSSD$N[resultsSSD$certainty & resultsSSD$tendency < 1]) {
      NMaxTooLow <- TRUE
      continue_N <- FALSE
    }
    continue <- continue_N

    continue_i <- TRUE
    counter <- 0
  }

  N_high <- N_max
  if(length(resultsSSD$N[resultsSSD$tendency>0])>0){
    N_high <- min(resultsSSD$N[resultsSSD$tendency>0])
  }

  intern <- list(
    model = model,
    data = data,
    power_desired = power_desired,
    maxN = maxN,
    minN = minN,
    goals = goals,
    cluster = cluster,
    fit_binom = fit_binom,
    furtherArgs = furtherArgs,

    allCalc = allCalc,
    resultsSSD = resultsSSD,
    resultsPowerBinomial = resultsPowerBinomial,

    continue_i = continue_i,
    counter=counter,
    continue_N = continue_N,
    N = N,
    N_high = N_high,

    NMaxTooLow = NMaxTooLow
  )

  extern <- list(
    continue = continue,
    N = N,
    reps = i_parallel,
    warning = warning
  )

  ret <- list(
    intern=intern,
    extern=extern
  )
  return(ret)
}

#updateSSD_NGoals
updateSSD_NGoals <- function(ssd, new_data, print=F){
  warning <- ""

  ssd_intern <- ssd$intern

  model <- ssd_intern$model
  data <- new_data
  power_desired <- ssd_intern$power_desired
  maxN <- ssd_intern$maxN
  goals <- ssd_intern$goals
  cluster <- ssd_intern$cluster
  fit_binom <- ssd_intern$fit_binom
  furtherArgs <- ssd_intern$furtherArgs


  ## conditions for N
  N_max <- ssd_intern$maxN
  N_min <- ssd_intern$minN
  N <- ssd_intern$N
  N_high <- ssd_intern$N_high

  continue_N <- ssd_intern$continue_N
  NMaxTooLow <- ssd_intern$NMaxTooLow

  ## conditions for i
  i_parallel <- ssd_intern$furtherArgs$i_parallel
  continue_i <- ssd_intern$continue_i
  counter <- ssd_intern$counter

  model_seed <- furtherArgs$model_seed


  ## store results
  resultsSSD <- ssd_intern$resultsSSD

  allCalc <- ssd_intern$allCalc

  resultsPowerBinomial <- ssd_intern$resultsPowerBinomial

  if(print) print("Starting inference...")
  #start continue_i
  results_simulation <- foreach(
    sim = 1:i_parallel,
    .combine = 'rbind',
    .options.future = list(seed = TRUE)
    # .packages = c("bayestestR", "rstan")
  ) %dofuture% {
    mem <- sum(c(object.size(model),
                 object.size(data),
                 object.size(goals),
                 object.size(model_seed)))
    print(paste0("size: ", mem))
    ret <- doSimulation_NGoals(model=model, data=data[[sim]], goals=goals,
                               model_seed = model_seed[[sim]])
    ret
  }
  if(print) print("Inference done")

  for (value in 1:i_parallel){
    allCalc$N <- c(allCalc$N, N)
    allCalc$success <- c(allCalc$success, results_simulation[value,1][[1]])

    for(g_i in seq_len(length(allCalc$goalAchievement))){
      if(length(allCalc$goalAchievement[[g_i]]) == 1 && is.na(allCalc$goalAchievement[[g_i]])){
        allCalc$goalAchievement[[g_i]] <- results_simulation[value,2][[1]][g_i]
      }else{
        allCalc$goalAchievement[[g_i]] <- c(allCalc$goalAchievement[[g_i]],
                                            results_simulation[value,2][[1]][g_i])
      }
    }
  }

  #######################################
  ## Bernoulli model instead of a beta ##
  #######################################

  successes <- allCalc$success[allCalc$N==N]
  data <- list(y = successes,
               N = length(successes))
  if(print) print("Starting bernoulli...")
  fit_binom_t <- sampling(fit_binom, data=data, refresh=0, seed=model_seed[1])
  if(print) print("Bernoulli done.")
  ex <- rstan::extract(fit_binom_t)
  power_estimated <- ex$p

  beta_power_90hdi <- hdi(ex$p, ci=0.90)
  beta_power_width <- beta_power_90hdi$CI_high-beta_power_90hdi$CI_low
  beta_power_low <- beta_power_90hdi$CI_low
  beta_power <- mean(ex$p)
  beta_power_high <- beta_power_90hdi$CI_high
  power_beta <- ex$p


  tendency <- 0
  certainty <- beta_power_width <= furtherArgs$accept_HDIwidth

  #Too low
  if(power_desired >= beta_power_high){
    tendency <- -1
  } else if(power_desired <= beta_power){   #high
    tendency <- 1
  }
  continue_i <- tendency==0 && !certainty


  # store values for this N
  N_success <- allCalc$success[allCalc$N==N]
  power <- sum(N_success)/length(N_success)
  i <- i_parallel
  if(N %in% ssd_intern$resultsSSD$N){
    i <- i + ssd_intern$resultsSSD$i[ssd_intern$resultsSSD$N==N]
  }
  tR <- data.frame(N=N, i=i, power=tail(power,1),
                   certainty=certainty, tendency=tendency)
  resultsSSD <- resultsSSD[resultsSSD$N!=N,]
  resultsSSD <- rbind(resultsSSD, tR)


  pB <- data.frame(N=N, power=power_beta)
  resultsPowerBinomial <- resultsPowerBinomial[resultsPowerBinomial$N!=N,]
  resultsPowerBinomial <- rbind(resultsPowerBinomial, pB)

  continue <- continue_i


  if(!continue_i){
    if(print) print("Getting next N ...")
    get_next_N <- nextN_logisticApprox_linearOnError_NGoals(power_desired,
                                                     N, N_max, N_min,
                                                     resultsSSD, seed=model_seed[1])
    if(print) print("Got next N.")

    N <- get_next_N[[1]]
    continue_N <- get_next_N[[2]]
    if(get_next_N$warning) warning <- "Stuck in loop"

    if (N_max %in% resultsSSD$N[resultsSSD$certainty & resultsSSD$tendency < 1]) {
      NMaxTooLow <- TRUE
      continue_N <- FALSE

      get_next_N <- nextN_logisticApprox_linearOnError_NGoals(power_desired,
                                                              N, Inf, N_min,
                                                              resultsSSD, seed=model_seed[1])
      N <- get_next_N[[1]]
    }
    continue <- continue_N

    continue_i <- TRUE
    counter <- 0
  }


  N_high <- N_max
  if(length(resultsSSD$N[resultsSSD$tendency>0])>0){
    N_high <- min(resultsSSD$N[resultsSSD$tendency>0])
  }

  intern <- list(
    model = model,
    data = data,
    power_desired = power_desired,
    maxN = maxN,
    minN = N_min,
    goals = goals,
    cluster = cluster,
    fit_binom = fit_binom,
    furtherArgs = furtherArgs,

    allCalc = allCalc,
    resultsSSD = resultsSSD,
    resultsPowerBinomial = resultsPowerBinomial,

    continue_i = continue_i,
    counter=counter,
    continue_N = continue_N,
    N = N,
    N_high = N_high,

    NMaxTooLow = NMaxTooLow
  )

  extern <- list(
    continue = continue,
    N = N,
    reps = i_parallel,
    warning = warning
  )

  ret <- list(
    intern=intern,
    extern=extern
  )
  return(ret)
}

#Close workers
closeCluster_NGoals = function(cluster){
  try({
    stopCluster(cluster)
  })
}

#Print ssd result
printSSD_NGoals = function(ssd, summarized=T){
  intern <- ssd$intern
  res <- intern$resultsSSD
  if(summarized){
    for(n in unique(res$N)){
      res <- res[res$N!=n | (res$N==n & res$i == max(res[res$N==n,]$i)),]
    }
    print(res)
  }else{
    print(res)
  }


  N_low <- 0
  if(length(res$N[res$tendency<=0])>0){
    N_low <- max(res$N[res$tendency<=0])
  }

  if (N_low == intern$maxN) {
    cat(paste0("The desired power of at least ", intern$power_desired,
                 " can't be reached with a maximum sample size N = ",
               intern$maxN, ". ",
                 "\n",
                 "Highest possible power with N = ",
               intern$maxN, ": ",
               res$power[res$N==N],
                 "\n",
                 "Estimated N to reach the desired power: ", intern$N, ". "))
  } else {

    pB <- intern$resultsPowerBinomial
    pB <- pB[pB$N == intern$N_high,]
    certainty <- sum(pB$power>=intern$power_desired)/length(pB$power)

    cat(paste0("The desired power of ", intern$power_desired,
               " can be reached with a probability of ",
               certainty*100, "% with N = ",
               intern$N_high, ". "))
  }
}



#Plot results
plotResults <- function (ssd, plotTriangles = T,
                         theme=list(main="#56B4E9", current="black", default="darkgrey", mean="white")) {

  ssd_intern <- ssd$intern

  power_desired <- ssd_intern$power_desired
  resultsSSD <- ssd_intern$resultsSSD
  resultsPowerBinomial <- ssd_intern$resultsPowerBinomial
  furtherArgs <- ssd_intern$furtherArgs
  acceptedWidth <- furtherArgs$accept_HDIwidth

  N_lastChecked <- tail(resultsSSD$N, 1)

  onlyCertainAndHigh <- resultsSSD[resultsSSD$certainty==T &
                                     resultsSSD$tendency == 1,]
  best_N <- NULL
  noBlackHighlight <- F
  if(dim(onlyCertainAndHigh)[1] > 0){
    best_N <- min(onlyCertainAndHigh$N)
    low_N <- best_N-1
    if(low_N %in% resultsSSD[resultsSSD$certainty==T &
                             resultsSSD$power<power_desired,]$N)
      noBlackHighlight <- T
  }

  gg <-  ggplot(resultsPowerBinomial) +
    theme_bw() +
    xlab("N") +
    ylab("Power") +
    geom_hline(yintercept=power_desired, linetype="dashed", color = theme$main)

  for (n in unique(resultsSSD$N)) {
    subset <-  resultsSSD[resultsSSD$N==n,]
    subset <- tail(subset, 1)

    powerBinom_N <- resultsPowerBinomial$power[resultsPowerBinomial$N==n]
    hdi_powerBinom <- hdi(powerBinom_N, ci=0.9)
    subset$min_power <- hdi_powerBinom$CI_low
    subset$max_power <- hdi_powerBinom$CI_high

    col <- theme$default
    if(!noBlackHighlight && length(N_lastChecked) > 0 && n==N_lastChecked) col <- theme$current
    if(!is.null(best_N) && n==best_N) col <- theme$main

    gg <- gg + geom_pointrange(data=subset, mapping=aes(x=N,
                                                        y=power,
                                                        ymin=min_power,
                                                        ymax=max_power),
                               shape = 21, size=1, fatten=3, linewidth=1,
                               color=col, fill=theme$mean)

    if(plotTriangles){
      acceptedWidth_upper <- mean(c(subset$min_power, subset$max_power)) + acceptedWidth/2
      acceptedWidth_lower <- mean(c(subset$min_power, subset$max_power)) - acceptedWidth/2

      acceptedWidth_data <- data.frame(N=n, acc=c(acceptedWidth_upper, acceptedWidth_lower), type=c("upper","lower"))

      gg <- gg +
        geom_point(data=acceptedWidth_data, mapping=aes(x=N, y=acc, shape=c(25,24)),
                   size=2, fill=theme$main, color=theme$main, alpha=0.5)

    }

  }
  gg <- gg + scale_shape_identity()
  gg
}


doSimulation_NGoals <- function(model, data, goals, model_seed) {

  # fit model
  fit <- update(model, newdata=data, refresh = 0, seed=model_seed)
  summ <- summary(fit)

  params <- as.matrix(fit)


  counterVal <- rep(0, length=length(goals))
  goalAchievement <- rep(0, length=length(goals))
  #check single goals
  for(g_i in seq_along(goals)){
    goal <- goals[[g_i]]
    gA <- rep(0, length=dim(params)[1])
    gB <- rep(0, length=dim(params)[1])
    for(p in goal$parametersA){
      gA <- gA+params[, p]
    }
    for(p in goal$parametersB){
      gB <- gB+params[, p]
    }
    gDiff <- gA-gB
    hdiTarget <- hdi(gDiff, ci=goal$hdi)

    if (goal$type == "rope") {
      cdfTarget <- ecdf(gDiff)

      if (goal$ropeExclude == "exclude") {
        leftFromROPE <- cdfTarget(goal$ropeLower)
        rightFromROPE <- 1-cdfTarget(goal$ropeUpper)
        goalAchievement[g_i] <- abs(leftFromROPE-rightFromROPE)

      } else if (goal$ropeExclude == "include") {
        integralROPE <- cdfTarget(goal$ropeUpper)-cdfTarget(goal$ropeLower)
        goalAchievement[g_i] <- integralROPE
      }

      if (goalAchievement[g_i] >= goal$hdi) {
        counterVal[g_i] <- 1
      } else {
        counterVal[g_i] <- 0
      }

    } else if (goal$type == "precision") {
      hdi_width <- abs(hdiTarget$CI_high-hdiTarget$CI_low)
      deltaWidths <- goal$precWidth-hdi_width
      goalAchievement[g_i] <- invlogit(deltaWidths/goal$precWidth)

      if (goalAchievement[g_i] >= 0.5) {
        counterVal[g_i] <- 1
      } else {
        counterVal[g_i] <- 0
      }
    }

    if (goalAchievement[g_i] > 1) {
      print(goalAchievement[g_i])
      stop("should not happen")
    } else if (goalAchievement[g_i] < 0) {
      print(goalAchievement[g_i])
      stop("should not happen")
    }
  }

  result <- list(counterValue=as.numeric(all(counterVal==1)),
                 goalAchievement = goalAchievement,
                 rhat = summ$spec_pars$Rhat,
                 Neff = summ$spec_pars$Bulk_ESS)
  return(result)
}




# if next_N is invalid according to BAYAS, decide which proposed sample size from BAYAS to choose
# - choose the sample size with the lower difference between approximated power and desired power
# - if difference for both sample sizes is equal:
#       - check if lower sample size can reach desired power and take this one
#       - if approximated power for lower sample size is less than power desired: take larger sample size
getValidNext_N <- function (N_BAYAS_low, N_BAYAS_high, power_desired, resultsSSD) {
  data <- data.frame()
  if (nrow(resultsSSD) < 2) {
    data <- resultsSSD
    data <- rbind(data, c(2, 1, 0))
  } else if (nrow(resultsSSD) >= 2) {
    data <- resultsSSD
  }

  fit <- optim(par=c(1.01, 0.01),
               lower=c(1.001, 0.001),
               method = "L-BFGS-B",
               fn=squared_powerDiff,
               N=data$N,
               power=data$power,
               weights=data$i)

  params <- fit$par
  a_approx <- params[1]
  b_approx <- params[2]

  power_N_BAYAS_low <- power_fct(N_BAYAS_low, a_approx, b_approx)
  power_N_BAYAS_high <- power_fct(N_BAYAS_high, a_approx, b_approx)

  powerDiff_N_BAYAS_low <- abs(power_N_BAYAS_low-power_desired)
  powerDiff_N_BAYAS_high <- abs(power_N_BAYAS_high-power_desired)

  if (powerDiff_N_BAYAS_low < powerDiff_N_BAYAS_high) {
    return(N_BAYAS_low)
  } else if (powerDiff_N_BAYAS_high < powerDiff_N_BAYAS_low) {
    return(N_BAYAS_high)
  } else if (power_N_BAYAS_low >= power_desired) {
    return(N_BAYAS_low)
  } else {
    return(N_BAYAS_high)
  }
}

nextN_logisticApprox_linearOnError_NGoals <- function (power_desired, N, N_max, N_min,
                                                       resultsSSD, seed){
  warning <- F
  N_high <- N_max
  N_low <- 0
  if(length(resultsSSD$N[resultsSSD$tendency>0])>0){
    potN <- resultsSSD$N[resultsSSD$tendency>0]
    if(length(resultsSSD$N[resultsSSD$certainty & resultsSSD$tendency == -1]) > 0){
      potN <- potN[potN > max(resultsSSD$N[resultsSSD$certainty & resultsSSD$tendency == -1])]
    }
    N_high <- min(potN)
  }
  if(length(resultsSSD$N[resultsSSD$tendency<=0])>0){
    N_low <- max(resultsSSD$N[resultsSSD$tendency<=0])
  }

  onlyCertain <- resultsSSD[resultsSSD$certainty==T,]

  possible_Ns <- N_high - N_low - 1

  next_N <- NULL
  continue_N <- TRUE
  if (N == 2 && N_high == 2 && N_max != 2 && resultsSSD$certainty[resultsSSD$N == N]) {
    next_N <- N_high
    continue_N <- FALSE
  }else if (possible_Ns == 0 && all(c(N_low, N_high) %in% onlyCertain$N)) {
    next_N <- N_high
    continue_N <- FALSE
  } else {
    data <- data.frame()
    if (nrow(resultsSSD) < 2) {
      data <- resultsSSD
      data <- rbind(data, c(2, 1, 0, 0, 0))
    }else if (nrow(resultsSSD) >= 2) {
      data <- resultsSSD
    }
    tryCatch({
      fit <- optim(par = c(1.01, 0.01), lower = c(1.001, 0.001),
                   method = "L-BFGS-B", fn = squared_powerDiff,
                   N = data$N, power = data$power, weights = data$i)
      params <- fit$par
      a_approx <- params[1]
      b_approx <- params[2]
      next_N <- round(N_fct(power_desired, a_approx, b_approx))
      continue_N <- TRUE
    }, error = function(e) {
      fit <- rstanarm::stan_glm(N ~ power, family = poisson(),
                                data = data, weights = data$i, refresh = 0,
                                seed=seed)
      params <- as.matrix(fit)
      fit_intercept <- exp(params[, "(Intercept)"])
      fit_power <- exp(params[, "power"])
      densityN <- density(fit_intercept + fit_power *
                            power_desired)
      next_N <- round(densityN$x[which.max(densityN$y)])
      # print(paste0("next_N berechnet mit stan_glm statt optim: ", next_N))
    })

    if(next_N < N_min){
      next_N <- N_min
    }else if (next_N < 2) {
      next_N <- 2
    }else if (next_N > N_max) {
      next_N <- N_max
    }

    continue_N <- T
    onlyCertain <- resultsSSD[resultsSSD$certainty==T,]
    if(possible_Ns == 0 && all(c(N_low, N_high) %in% onlyCertain$N)){
      continue_N <- F
    }else{
      Ns <- c()
      count <- 0
      while(length(resultsSSD$certainty[resultsSSD$N == next_N])>0 &&
            resultsSSD$certainty[resultsSSD$N == next_N]){
        count <- count +1
        if(resultsSSD$tendency[resultsSSD$N == next_N] == 0){
          if(next_N %in% Ns) break
          next_N <- next_N+1
        }else if(resultsSSD$tendency[resultsSSD$N == next_N] < 0){
          next_N <- next_N+1
        }else if(resultsSSD$tendency[resultsSSD$N == next_N] > 0){
          next_N <- next_N-1
        }
        Ns <- c(Ns, next_N)

        if(sum(next_N==Ns) > 2){
          warning <- T
        }
      }
    }
  }
  if(next_N < N_min){
    continue_N <- F
    next_N <- N_min
  }
  result <- list(next_N, continue_N, warning=warning)
  return(result)
}
