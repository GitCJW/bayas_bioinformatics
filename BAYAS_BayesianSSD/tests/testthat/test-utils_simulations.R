# testthat::test_file("tests/testthat/test-utils_simulations.R")


### Test 'startSSD'

test_that("test startSSD", {

  skip()

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

  model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())

  goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                     goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                     ropeExclusive=T, ci=0.95)


  expect_success(startSSD(
    model = model,
    dataCreationFunction = dataCreationFunction,
    powerDesired = 0.8,
    possN = c(1:50)*2,
    goals = list(goal),
    con = 100,
    iParallel = 2))
})


### Test 'updateSSD'

test_that("test updateSSD", {

  skip()

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

  model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())

  goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                     goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                     ropeExclusive=T, ci=0.95)


  ssd <- startSSD(
    model = model,
    dataCreationFunction = dataCreationFunction,
    powerDesired = 0.8,
    possN = c(1:50)*2,
    goals = list(goal),
    con = 100,
    iParallel = 2)

  expect_success(updateSSD(ssd, dataCreationFunction))
})


### Test 'initSSD'

test_that("test initSSD", {

  skip()

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

  data <- list(
    dataCreationFunction(20),
    dataCreationFunction(20))
  model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())

  goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                     goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                     ropeExclusive=T, ci=0.95)

  powerDesired <- 0.8
  possN <- c(1:50)*2
  goals <- list(goal)

  furtherArgs <- list(
    iParallel=2,
    modelSeed = c(1,2),
    acceptHDIwidth = 0.13,
    acceptHDIConcentration = 100
  )

  expect_success(initSSD(model, data, powerDesired, possN, goals,
          furtherArgs))

})


### Test 'continueSSD'

test_that("test continueSSD", {

  skip()

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

  model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())

  goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                     goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                     ropeExclusive=T, ci=0.95)


  ssd <- startSSD(
    model = model,
    dataCreationFunction = dataCreationFunction,
    powerDesired = 0.8,
    possN = c(1:50)*2,
    goals = list(goal),
    con = 100,
    iParallel = 2)

  expect_success(continueSSD(ssd, list(dataCreationFunction(20), dataCreationFunction(20))))
})


### Test 'doSSD'

test_that("test doSSD", {

  skip()

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

  model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())

  goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                     goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                     ropeExclusive=T, ci=0.95)


  ssd <- startSSD(
    model = model,
    dataCreationFunction = dataCreationFunction,
    powerDesired = 0.8,
    possN = c(1:50)*2,
    goals = list(goal),
    con = 100,
    iParallel = 2)

  expect_success(doSSD(ssd, list(dataCreationFunction(20), dataCreationFunction(20))))
})


### Test 'doMultSimulation'

test_that("test doMultSimulation", {

  skip()

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

  model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())

  goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                     goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                     ropeExclusive=T, ci=0.95)


  ssd <- startSSD(
    model = model,
    dataCreationFunction = dataCreationFunction,
    powerDesired = 0.8,
    possN = c(1:50)*2,
    goals = list(goal),
    con = 100,
    iParallel = 2)

  expect_success(doMultSimulation(ssd, list(dataCreationFunction(20), dataCreationFunction(20))))
})


### Test 'doSimulation'

test_that("test doSimulation", {

  skip()

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

  model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())

  goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                     goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                     ropeExclusive=T, ci=0.95)

  expect_success(doSimulation(model, dataCreationFunction(20), list(goal), 123))
})


### Test 'fitModel'

test_that("test fitModel", {

  skip()

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
  data <- dataCreationFunction(20)

  modelRstanarm <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())
  modelBrms <- brms::brm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())
  modelGlm <- glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())


  expect_success(fitModel(modelRstanarm, data, 123))
  expect_success(fitModel(modelBrms, data, 123))
  expect_failure(fitModel(modelGlm, data, 123))
})




### Test 'checkGoal'

test_that("test checkGoal", {


  goal1 <- createGoal(parametersA="aa", parametersB="bb",
                      goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                      ropeExclusive=T, ci=0.95)
  goal2 <- createGoal(parametersA="aa", parametersB="bb",
                      goalType="rope", ropeType="exclude", ropeLower=-1, ropeUpper = 1,
                      ropeExclusive=T, ci=0.95)
  goal3 <- createGoal(parametersA="aa", parametersB="bb",
                      goalType="rope", ropeType="include", ropeLower=-0.5, ropeUpper = 0.5,
                      ropeExclusive=T, ci=0.95)
  goal4 <- createGoal(parametersA="aa", parametersB="bb",
                      goalType="precision", precisionWidth =1,
                      ci=0.95)
  goal5 <- createGoal(parametersA="aa",
                      goalType="precision", precisionWidth =0.5,
                      ropeExclusive=T, ci=0.6)

  set.seed(1234)
  params <- matrix(c(rnorm(4000, 0, 0.1), rnorm(4000, 0.5, 0.1)), 4000, 2)
  dimnames(params) <- list(iterations = NULL,
                           parameters = c("aa","bb"))

  c1 <- checkGoal(goal1, params)
  c2 <- checkGoal(goal2, params)
  c3 <- checkGoal(goal3, params)
  c4 <- checkGoal(goal4, params)
  c5 <- checkGoal(goal5, params)

  expect_equal(c1$goalAchievement, 0.99975, tolerance = 1e-4)
  expect_equal(c1$counterVal, 1)

  expect_equal(c2$goalAchievement, 0.00025, tolerance = 1e-4)
  expect_equal(c2$counterVal, 0)

  expect_equal(c3$goalAchievement, 0.498, tolerance = 1e-4)
  expect_equal(c3$counterVal, 0)

  expect_equal(c4$goalAchievement, 0.6126774, tolerance = 1e-4)
  expect_equal(c4$counterVal, 1)

  expect_equal(c5$goalAchievement, 0.6606021, tolerance = 1e-4)
  expect_equal(c5$counterVal, 1)
})
