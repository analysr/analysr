test_that("vector_calc works like create_feature", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/vector_calc/before1")

  # vector_calc
  model <- vector_calc(analysr_env,
                          "Temp_average_2006",
                          mean,
                          "Temperature",
                          lubridate::parse_date_time("2006/01/01 01:00:00", "ymd-HMS"),
                          lubridate::parse_date_time("2006/12/31 23:59:00", "ymd-HMS"))

  # check model
  expect_equal(model_state_equal("./csv/vector_calc/after1", model), TRUE)
})
test_that("vector_calc works on request", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/vector_calc/before1")

  # vector_calc
  model <- (
    analysr_env
    %>% observed(Temperature > 35)
    %>% vector_calc("Temp_average_2006", mean, "Temperature")
  )

  # check model
  expect_equal(model_state_equal("./csv/vector_calc/after1", model), TRUE)
})
test_that("vector_calc works on request #2", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/vector_calc/before1")

  # vector_calc
  model <- (
    analysr_env
    %>% observed(Temperature > 35)
    %>% vector_calc("Temp_average_2006", mean)
  )

  # check model
  expect_equal(model_state_equal("./csv/vector_calc/after1", model), TRUE)
})
