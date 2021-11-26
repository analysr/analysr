test_that("inside works on a request", {
  setup_new_env()

  # import model
  load_env_csv("./csv/inside_outside/test1")
  model <- (
    analysr_env
    %>% observed(Temperature > 38.5)
    %>% at_most(15 * days)
    %>% inside("Hospitalization")
    %>% before("Surgery")
  )

  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature",
                duration_type = "at_most",
                duration = lubridate::duration(15, "days"))

  #check result
  expect_equal(model_state_equal("./csv/inside_outside/test1", model, query),
               TRUE)
})
test_that("outside works on a request", {
  setup_new_env()

  # import model
  load_env_csv("./csv/inside_outside/test2")
  model <- (
    analysr_env
    %>% observed(Temperature > 38.5)
    %>% at_most(15 * days)
    %>% outside("Hospitalization")
    %>% before("Surgery")
  )

  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature",
                duration_type = "at_most",
                duration = lubridate::duration(15, "days"))


  #check result
  expect_equal(model_state_equal("./csv/inside_outside/test2", model, query),
               TRUE)

})
