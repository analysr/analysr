test_that("max_aggregate works", {
  values <- c(8, 9, 5, 7)
  result <- max(values)

  expect_equal(result, 9)
})
