test_that("observed works with simple condition", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before1")

  # observed
  model <- observed(analysr_env, Temperature > 38.5)

  stat_unit <- c(101929077)
  date <- c(lubridate::as_date("2006-11-10 12:00:00"))

  df <- data.frame(stat_unit, date)

  query <- list(condition=rlang::expr(Temperature > 38.5), tag="Temperature",
                selection = df)

  # check model
  expect_equal(model_state_equal("./csv/observed/before1", model, query), TRUE)
})
test_that("observed works  with simple reverse condition", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before1")

  # observed
  model <- observed(analysr_env, 38.5 < Temperature)
  stat_unit <- c(101929077)
  date <- c("2006-11-10 12:00:00")

  df <- data.frame(stat_unit, date)

  query <- list(condition=rlang::expr(38.5 < Temperature), tag="Temperature",
                selection = df)


  # check model
  expect_equal(model_state_equal("./csv/observed/before1", model, query), TRUE)
})



