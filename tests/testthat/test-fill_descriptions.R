test_that("fill_description works", {
  # reset env
  setup_new_env()

  hash <- c(1, 2, 3)
  type <- "location"
  data <- data.frame(
    first_column = c("value_1", "value_2", "value_3"),
    second_column = c("value_1", "value_2", "value_3"),
    location = c("Home", "Hospital", "Home")
  )

  expected <- data.frame(
    hash = c(1, 2, 3),
    type = c("location", "location", "location"),
    value = c("Home", "Hospital", "Home")
  )

  fill_descriptions(hash, type, data)

  expect_equal(
    dplyr::all_equal(
      analysr_env$descriptions,
      expected
    ),
    TRUE
  )
})
