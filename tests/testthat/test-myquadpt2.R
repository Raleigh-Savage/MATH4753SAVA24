test_that("Description of what's happening", {
  v <- myquadpt2(1:10)
  expect_length(v, 10)
  expect_equal(2 * 2, 4)
})
