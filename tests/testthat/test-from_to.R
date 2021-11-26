test_that("from works with date", {
  setup_new_env()

  # import model
  load_env_csv("./csv/from_to/test1")
  model <- observed(analysr_env, Temperature > 38.5)

  # from
  model <- from(model, as.Date("2006-10-22"))

  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature",
                from_date = as.Date("2006-10-22"))

  # check model (model should not have changed, only query has)

  expect_equal(model_state_equal("./csv/from_to/test1", model, query), TRUE)
})
test_that("from works with date representing date", {
  setup_new_env()

  # import model
  load_env_csv("./csv/from_to/test1")
  model <- observed(analysr_env, Temperature > 38.5)

  # from
  model <- from(model, "2006-10-22", date_format_func = as.Date)

  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature",
                from_date = as.Date("2006-10-22", tz = "UTC"))

  # check model (model should not have changed, only query has)
  expect_equal(model_state_equal("./csv/from_to/test1", model, query), TRUE)
})
test_that("from to works on a request", {
  setup_new_env()

  # import model
  load_env_csv("./csv/from_to/test2")
  model <- (
    analysr_env
    %>% observed(Temperature > 38.5)
    %>% at_most(15 * days)
    %>% from("2006/11/09 08:00:00")
    %>% to("2006/11/11 09:00:00")
    %>% before("Surgery")
  )

  #expected_result
  exp_result <- c(7)


  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature",
                duration_type = "at_most",
                duration = lubridate::duration(15, "days"),
                from_date = lubridate::parse_date_time("2006/11/09 08:00:00", "ymd-HMS"),
                to_date = lubridate::parse_date_time("2006/11/11 09:00:00", "ymd-HMS"))

  # check model (model should not have changed)
  expect_equal(model_state_equal("./csv/from_to/test2", model, query), TRUE)
})
