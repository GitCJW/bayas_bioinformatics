test_that("match names", {
  expect_contains(c("goal1","goal2","goal3","goal4","rope1","rope2"), names(bayesianSSDColors()))
})
