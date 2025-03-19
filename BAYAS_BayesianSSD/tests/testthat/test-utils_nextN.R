### Test 'nextN'

test_that("test nextN", {

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
  possN <- seq(1:33)*3

  ssd <- list(
    intern = list(
      powerDesired = powerDesired,
      N = N,
      maxN = NMax,
      minN = NMin,
      resultsSSD = resultsSSD,
      possN = possN
    )
  )

  nextN <- nextN(ssd)

  expect_equal(nextN[[1]], 48)
})



### Test 'nextN.reviseCertainty'
test_that("test nextN.reviseCertainty", {

  #Create an exemplary ssd
  resultsSSD <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,100,110),
    power = c(1, 0.7, 0.81,0.79),
    certainty = c(F,F,T,T),
    tendency = c(1, -1, 1, -1)
  )
  resultsSSDRet <- nextN.reviseCertainty(resultsSSD)

  resultsSSD2 <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,110,100),
    power = c(1, 0.7, 0.81,0.79),
    certainty = c(F,F,T,T),
    tendency = c(1, -1, 1, -1)
  )
  resultsSSD2Ret <- nextN.reviseCertainty(resultsSSD2)

  expect_true(resultsSSDRet$certainty[4])
  expect_false(resultsSSDRet$certainty[3])

  expect_true(resultsSSD2Ret$certainty[3])
  expect_false(resultsSSD2Ret$certainty[4])
})




### Test 'nextN.lowAndHigh'
test_that("test nextN.lowAndHigh", {

  #Create an exemplary resultsSSD
  resultsSSD <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,100,110),
    power = c(1, 0.7, 0.81,0.79),
    certainty = c(F,F,F,T),
    tendency = c(1, -1, 1, -1)
  )
  limits <- nextN.lowAndHigh(resultsSSD, 3, 99)

  resultsSSD2 <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,110,100),
    power = c(1, 0.7, 0.81,0.79),
    certainty = c(F,F,T,F),
    tendency = c(1, -1, 1, -1)
  )
  limits2 <- nextN.lowAndHigh(resultsSSD2, 3, 99)

  resultsSSD3 <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,110,100),
    power = c(1, 0.7, 0.79,0.81),
    certainty = c(F,F,T,T),
    tendency = c(1, -1, 0, 1)
  )
  limits3 <- nextN.lowAndHigh(resultsSSD3, 3, 99)

  expect_equal(limits[1], 51)
  expect_equal(limits[2], 99)

  expect_equal(limits2[1], 3)
  expect_equal(limits2[2], 48)

  expect_equal(limits3[1], 48)
  expect_equal(limits3[2], 51)
})



### Test 'nextN.approx'
test_that("test nextN.approx", {

  #Create an exemplary resultsSSD
  resultsSSD <- data.frame(
    N = c(39,99),
    i = c(60,20),
    power = c(0.7, 1)
  )

  expect_equal(nextN.approx(resultsSSD, 0, 99), 0)
  expect_equal(nextN.approx(resultsSSD, 0.8, 99), 49)
  expect_equal(nextN.approx(resultsSSD, 0.9, 99), 66)
  expect_equal(nextN.approx(resultsSSD, 0.99, 99), 119)
  expect_equal(nextN.approx(resultsSSD, 1, 99), Inf)
})



### Test 'nextN.verifyNCandidate'
test_that("test nextN.verifyNCandidate", {

  #Create an exemplary resultsSSD
  resultsSSD <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,100,110),
    power = c(1, 0.7, 0.81,0.79),
    certainty = c(F,F,T,T),
    tendency = c(1, -1, 1, -1)
  )

  expect_equal(nextN.verifyNCandidate(resultsSSD, 48), 47)
  expect_equal(nextN.verifyNCandidate(resultsSSD, 54), 54)
  expect_equal(nextN.verifyNCandidate(resultsSSD, 51), 52)
  expect_equal(nextN.verifyNCandidate(resultsSSD, 39), 39)
})



### Test 'getValidNextN'
test_that("test getValidNextN", {

  #Create an exemplary resultsSSD
  resultsSSD <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,100,110),
    power = c(1, 0.7, 0.81,0.79),
    certainty = c(F,F,T,T),
    tendency = c(1, -1, 1, -1)
  )
  resultsSSD2 <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,100,110),
    power = c(1, 0.7, 0.81,0.79),
    certainty = c(F,F,F,F),
    tendency = c(1, -1, 1, -1)
  )

  expect_equal(getValidNextN(45, 48, resultsSSD, 47), 45)
  expect_equal(getValidNextN(51, 54, resultsSSD, 52), 54)
  expect_equal(getValidNextN(45, 48, resultsSSD2, 47), 48)
  expect_equal(getValidNextN(51, 54, resultsSSD2, 52), 51)
})



