test_that("before works", {
  setup_new_env()

  # import model
  load_env_csv("./csv/before/before")
  model <- observed(analysr_env, Temperature > 38.5)

  # at_most
  model <- at_most(model, 15 * days)

  # before
  result <- before(model, "Surgery")

  stat_unit <- c(101929077)
  date <- c("2006-11-10 12:00:00")

  df <- data.frame(stat_unit, date)

  query <- list(condition=rlang::expr(Temperature > 38.5),
                tag="Temperature",
                selection = df,
                duration_type = "at_most",
                duration=lubridate::duration(15, "days"))

  #expected_result
  exp_result <- c(101929077)

  #check result
  expect_equal(result, exp_result)

  # check model (model should have changed, query also)
  expect_equal(model_state_equal("./csv/before/after", model, query), TRUE)




})
