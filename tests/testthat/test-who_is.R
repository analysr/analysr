test_that("who_is works on a request", {
  setup_new_env()

  # import model
  load_env_csv("./csv/who_is/test")
  model <- (
    analysr_env
    %>% observed(Temperature > 38.5)
    %>% at_most(15 * days)
    %>% who_is(Gender == "Male")
    %>% before("Surgery")
  )

  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature",
                duration_type = "at_most",
                duration = lubridate::duration(15, "days"))

  #check result
  expect_equal(model_state_equal("./csv/who_is/test", model, query), TRUE)
})
