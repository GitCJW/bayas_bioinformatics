### Test 'createGoal'

test_that("test createGoal", {

  goal1 <- createGoal(
    parametersA = "a",
    goalType = "rope", ci=0.95,
    ropeType = "exclude", ropeLower = 0, ropeUpper = 1,
    ropeExclusive = T)

  goal2 <- createGoal(
    parametersA = "a",
    goalType = "rope", ci=0.9,
    ropeType = "include", ropeLower = 0, ropeUpper = 1,
    ropeExclusive = T)

  goal3 <- createGoal(
    parametersA = "a",
    goalType = "rope", ci=0.85,
    ropeType = "include", ropeLower = 0, ropeUpper = 1,
    ropeExclusive = F)

  goal3 <- createGoal(
    parametersA = "a",
    goalType = "precision", ci=0.8,
    precisionWidth=1)

  goal4 <- createGoal(
    parametersA = "a", parametersB="b",
    goalType = "rope", ci=0.75,
    ropeType = "exclude", ropeLower = 0, ropeUpper = 1,
    ropeExclusive = T)

  goal5 <- createGoal(
    parametersA = "a", parametersB="b",
    goalType = "precision", ci=0.7,
    precisionWidth=1)


  expect_true("bayesianssdgoal" %in% class(goal1))
  expect_true("bayesianssdgoal" %in% class(goal2))
  expect_true("bayesianssdgoal" %in% class(goal3))
  expect_true("bayesianssdgoal" %in% class(goal4))
  expect_true("bayesianssdgoal" %in% class(goal5))
})



### Test 'plotGoal'

test_that("test plotGoal", {

  goal1 <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                     goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                     ropeExclusive=T, ci=0.95)
  goal2 <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                      goalType="rope", ropeType="exclude", ropeLower=-1, ropeUpper = 1,
                      ropeExclusive=T, ci=0.95)
  goal3 <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                      goalType="rope", ropeType="include", ropeLower=-0.5, ropeUpper = 0.5,
                      ropeExclusive=T, ci=0.95)
  goal4 <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                     goalType="precision", precisionWidth =1,
                     ropeExclusive=T, ci=0.95)
  goal5 <- createGoal(parametersA="treatmentcontrol",
                      goalType="precision", precisionWidth =0.5,
                      ropeExclusive=T, ci=0.6)

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

  suppressWarnings({
    model <- stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson(),
                      iter=1000, chains=2, refresh=0)
  })


  goals <- list(goal1, goal2, goal3, goal4, goal5)
  N <- 100
  ggs <- plotGoal(goals, dataCreationFunction, model, N)
  ggWoModel <- plotGoal(goals)
  gg <- plotGoal(goal2)

  expect_true("gtable" %in% class(ggs))
  expect_true("gtable" %in% class(ggWoModel))
  expect_true("gtable" %in% class(gg))
})

