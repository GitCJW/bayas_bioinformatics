
### Test 'verifySSDInputs'

test_that("valid inputs", {
  suppressWarnings({
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

    model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20),
                                family=poisson(), iter=10, chains=1)

    goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                       goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                       ropeExclusive=T, ci=0.95)
  })

  expect_equal(TRUE, verifySSDInputs(model=model, dataCreationFunction=dataCreationFunction,
                                     powerDesired=0.8, minN=2, maxN=100, factorN=2, goals=list(goal),
                                     con=200, iParallel=20))
})

test_that("invalid inputs", {
  suppressWarnings({
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

    dataCreationFunction2 <- function(){
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

    model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20),
                                family=poisson(), iter=10, chains=1)

    model2 <- glm("y~-1+treatment", data=dataCreationFunction(20),
                                family=poisson())

    goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                       goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                       ropeExclusive=T, ci=0.95)
  })

  expect_error(
    verifySSDInputs(model=model, dataCreationFunction=dataCreationFunction,
                    powerDesired=0.8, minN=2, maxN=100, factorN=2, goals=goal,
                    con=200, iParallel=20))

  expect_error(
    verifySSDInputs(model=model, dataCreationFunction=dataCreationFunction2,
                    powerDesired=0.8, minN=2, maxN=100, factorN=2, goals=list(goal),
                    con=200, iParallel=20))

  expect_error(
    verifySSDInputs(model=model2, dataCreationFunction=dataCreationFunction,
                    powerDesired=0.8, minN=2, maxN=100, factorN=2, goals=list(goal),
                    con=200, iParallel=20))

  expect_error(
    verifySSDInputs(model=model, dataCreationFunction=dataCreationFunction,
                    powerDesired=1, minN=2, maxN=100, factorN=2, goals=list(goal),
                    con=200, iParallel=20))

  expect_error(
    verifySSDInputs(model=model, dataCreationFunction=dataCreationFunction,
                    powerDesired=1, minN=101, maxN=100, factorN=2, goals=list(goal),
                    con=200, iParallel=20))

  expect_error(
    verifySSDInputs(model=model, dataCreationFunction=dataCreationFunction,
                    powerDesired=1, minN=2, maxN=100, factorN=0.5, goals=list(goal),
                    con=200, iParallel=20))

  expect_error(
    verifySSDInputs(model=model, dataCreationFunction=dataCreationFunction,
                    powerDesired=1, minN=2, maxN=100, factorN=2, goals=list(goal),
                    con=200, iParallel=0))

  expect_error(
    verifySSDInputs(model=model, dataCreationFunction=dataCreationFunction,
                    powerDesired=1, minN=2, maxN=100, factorN=2, goals=list(goal),
                    con=5, iParallel=20))
})



### Test 'singleGoalsAsList'

test_that("check inputs", {
  suppressWarnings({
    goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                       goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                       ropeExclusive=T, ci=0.95)

    goalList <- list(goal)
  })

  expect_equal(list(goal), singleGoalsAsList(goal), ignore_attr=T)
  expect_equal(list(goal), singleGoalsAsList(goalList), ignore_attr=T)
})
