### Test 'powerFct'

test_that("test powerFct", {
  expect_true(powerFct(N=1e6,a=1.001,b=0.001) > 0)
  expect_true(powerFct(N=1e6,a=1.0,b=0.001) == 0)
  expect_true(powerFct(N=1e6,a=1.001,b=0) == 0)
  expect_true(powerFct(N=-1,a=2,b=1) < 0)
  expect_true(powerFct(N=10,a=1.001,b=0.001) > 0)
  expect_true(powerFct(N=11,a=1.001,b=0.001) > powerFct(N=10,a=1.001,b=0.001))
  expect_true(powerFct(N=10,a=10,b=10) > 0)
})



### Test 'NFct'

test_that("test NFct", {
  a <- 1.5
  b <- 0.5
  expect_equal(10, NFct(powerFct(N=10,a=a,b=b),a=a,b=b), tolerance = testthat_tolerance())
  expect_equal(100, NFct(powerFct(N=100,a=a,b=b),a=a,b=b), tolerance = testthat_tolerance())
})


### Test 'squaredPowerDiff'

test_that("test squaredPowerDiff", {
  expect_true(squaredPowerDiff(params = c(1.5,0.5),
                               N = c(1,3,10,30),
                               power = c(0,0.1,0.7,0.85),
                               weights = c(20,20,50,40)) > 0)
})
