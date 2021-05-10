test_that("multiplication works", {
  values <- c(8, 9, 5, 7)
  result <- mean(values)

  expect_equal(result, 7.25)
})
