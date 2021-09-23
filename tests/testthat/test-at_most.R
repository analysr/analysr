test_that("at_most works", {
  setup_new_env()

  # import model
  load_env_csv("./csv/at_most/model")
  model <- observed(analysr_env, Temperature > 38.5)

  # at_most
  model <- at_most(model, 15*days)



  stat_unit <- c(101929077)
  date <- c("2006-11-10 12:00:00")

  df <- data.frame(stat_unit, date)

  query <- list(condition=rlang::expr(Temperature > 38.5),
                tag="Temperature",
                selection = df,
                duration_type = "at_most",
                duration=lubridate::duration(15, "days"))

  # check model (model should not have changed, only query has)

  expect_equal(model_state_equal("./csv/at_most/model", model, query), TRUE)
})
