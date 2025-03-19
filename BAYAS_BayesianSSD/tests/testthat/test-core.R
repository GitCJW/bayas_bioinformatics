### Test 'runSSD'

test_that("test runSSD", {
  testthat::skip()
})



### Test 'checkSettings'

test_that("test checkSettings", {
  dataCreationFunction <- function(N){
    group_effects <- c(
      control = 9,
      drug = 7
    )
    treatment <- rep(c("control", "drug"), length.out=N)
    y <- rpois(N, group_effects)

    data <- data.frame(
      treatment = treatment,
      y = y
    )
    data
  }

  dataCreationFunctionWrong <- function(N){
    group_effects <- c(
      control = 9,
      drug = 7
    )
    treatment <- rep(c("control", "drug"), length.out=N)
    y <- rpois(N, group_effects)

    data <- data.frame(
      treatment = treatment,
      x = y
    )
    data
  }

  model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson(), refresh=0)
  modelWrong <- glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())
  class(modelWrong) <- c("stanmodel", class(modelWrong))

  goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                     goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                     ropeExclusive=T, ci=0.95)

  goalWrong <- goal
  goalWrong$parametersA <- "test"

  expect_true(checkSettings(model, dataCreationFunction, 2, list(goal)))
  expect_error(checkSettings(model, dataCreationFunction, 2, list(goalWrong)))
  expect_error(checkSettings(model, dataCreationFunctionWrong, 2, list(goal)))
  expect_error(checkSettings(modelWrong, dataCreationFunction, 2, list(goal)))
})



### Test 'printSSD'

test_that("test printSSD", {
  #Create an exemplary ssd
  powerDesired <- 0.8
  N <- 48
  NMax <- 99
  NMin <- 3
  resultsSSD <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,100,80),
    power = c(1, 0.7, 0.81, 0.8125),
    certainty = c(F,F,F,F),
    tendency = c(1, -1, 0, 1)
  )
  resultsSSD2 <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,100,80),
    power = c(0.79, 0.6, 0.7, 0.71),
    certainty = c(F,F,F,F),
    tendency = c(-1, -1, -1, -1)
  )
  resultsPowerBinomial <- data.frame(
    N = c(99, 39, 48, 51),
    powerMean = c(0.9166667, 0.6875000, 0.7934783, 0.8048780),
    powerLow = c(0.8389251, 0.5937902, 0.7450150, 0.7547323),
    powerHigh = c(0.9955778, 0.7824342, 0.8425751, 0.8557615)
  )
  possN <- seq(1:33)*3

  ssd <- list(
    intern = list(
      powerDesired = powerDesired,
      N = N,
      maxN = NMax,
      minN = NMin,
      resultsSSD = resultsSSD,
      possN = possN,
      NHigh = 51,
      resultsPowerBinomial = resultsPowerBinomial
    )
  )
  ssd2 <- list(
    intern = list(
      powerDesired = powerDesired,
      N = 150,
      maxN = NMax,
      minN = NMin,
      resultsSSD = resultsSSD2,
      possN = possN,
      NHigh = 150,
      resultsPowerBinomial = resultsPowerBinomial
    )
  )
  ssdNotClass <- ssd
  class(ssd) <- c("bayesianssd",class(ssd))
  class(ssd2) <- c("bayesianssd",class(ssd2))


  expect_error(printSSD(ssdNotClass))
  expect_true(printSSD(ssd))
  expect_true(printSSD(ssd2))
})



### Test 'plotResults'

test_that("test plotResults", {

  powerDesired <- 0.8
  resultsSSD <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,100,80),
    power = c(1, 0.7, 0.81, 0.8125),
    certainty = c(F,F,F,F),
    tendency = c(1, -1, 0, 1)
  )

  resultsPowerBinomial <- data.frame(
    N = c(99, 39, 48, 51),
    powerMean = c(0.9166667, 0.6875000, 0.7934783, 0.8048780),
    powerLow = c(0.8389251, 0.5937902, 0.7450150, 0.7547323),
    powerHigh = c(0.9955778, 0.7824342, 0.8425751, 0.8557615)
  )

  furtherArgs <- list(
    acceptHDIwidth = c(upper = 0.13)
  )

  ssd <- list(
    intern = list(
      powerDesired = powerDesired,
      resultsSSD = resultsSSD,
      resultsPowerBinomial = resultsPowerBinomial,
      furtherArgs = furtherArgs
    )
  )
  class(ssd) <- c("bayesianssd", class(ssd))
  gg <- plotResults(ssd)
  expect_true("ggplot" %in% class(gg))
})
