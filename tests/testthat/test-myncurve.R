test_that("Three tests for each list component", {
  list = myncurve(5, 6, 5.5)
  expect_equal(list[["mean"]], 5)
  expect_equal(list[["sd"]], 6)
  expect_equal(list[["probability"]], round(pnorm(5.5, mean = 5, sd = 6), 4))
})
