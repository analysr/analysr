test_that("fill_description works", {
  # reset env
  setup_new_env()

  hash <- c(1, 2, 3)
  types <- c("location", "during")
  data <- data.frame(
    first_column = c("value_1", "value_2", "value_3"),
    second_column = c("value_1", "value_2", "value_3"),
    location = c("Home", "Hospital", "Home"),
    during = c("Diner", "Sport", "/")
  )

  expected <- data.frame(
    hash = c(1, 2, 3, 1, 2, 3),
    type = c("location", "location", "location", "during", "during", "during"),
    value = c("Home", "Hospital", "Home", "Diner", "Sport", "/")
  )

  fill_descriptions(hash, types, data)

  expect_equal(
    dplyr::all_equal(
      analysr_env$descriptions,
      expected
    ),
    TRUE
  )
})
