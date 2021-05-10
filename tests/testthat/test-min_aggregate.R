test_that("min works", {
  values <- c(8, 9, 5, 7)
  result <- min(values)

  expect_equal(result, 5)
})
