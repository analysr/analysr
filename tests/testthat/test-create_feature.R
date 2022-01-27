test_that("create_feature works", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/create_feature/before1")

  # restrict
  model <- create_feature(analysr_env,
                          "Temp_mooy_2006",
                          "Temperature",
                          lubridate::parse_date_time("2006/01/01 01:00:00", "ymd-HMS"),
                          lubridate::parse_date_time("2006/12/31 23:59:00", "ymd-HMS"))

  # check model
  save_env_csv("./csv/create_feature/after1")
  expect_equal(model_state_equal("./csv/create_feature/after1", model), TRUE)
})
