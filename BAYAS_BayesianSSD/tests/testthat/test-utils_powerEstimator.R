## Test 'powerEstimator'

test_that("test powerEstimator", {

  ret1 <- powerEstimator(rep(0,10))
  ret2 <- powerEstimator(rep(1,10))
  ret3 <- powerEstimator(rep(c(0,1), length.out=10))
  ret4 <- powerEstimator(c(rep(0,99),1))
  ret5 <- powerEstimator(c(rep(1,99),0))

  expect_equal(length(ret1), 4)
  expect_equal(length(ret2), 4)
  expect_equal(length(ret3), 4)
  expect_equal(length(ret4), 4)
  expect_equal(length(ret5), 4)

  expect_true(all(ret1 > 0))
  expect_true(all(ret2 > 0))
  expect_true(all(ret3 > 0))
  expect_true(all(ret4 > 0))
  expect_true(all(ret5 > 0))
})
