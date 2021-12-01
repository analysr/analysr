test_that("after works", {
  setup_new_env()

  # import model
  load_env_csv("./csv/after/test1")
  model <- (
    analysr_env
    %>% observed(Temperature > 38.5)
    %>% at_most(15 * days)
    %>% after(Surgery)
  )
  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature",
                duration_type = "at_most",
                duration = lubridate::duration(15, "days"))

  # check model (model should have changed, query also)
  expect_equal(model_state_equal("./csv/after/test1", model, query),
               TRUE)


})
test_that("after works with at_most", {
  setup_new_env()

  # import model
  load_env_csv("./csv/after/test2")
  model <- (
    analysr_env
    %>% observed(Temperature > 38.5)
    %>% at_most(15 * days)
    %>% after(Surgery)
  )

  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature",
                duration_type = "at_most",
                duration = lubridate::duration(15, "days"))

  #check result
  expect_equal(model_state_equal("./csv/after/test2", model, query),
               TRUE)

})


test_that("after works with at_least", {
  setup_new_env()

  # import model
  load_env_csv("./csv/after/test3")
  model <- (
    analysr_env
    %>% observed(Temperature > 38.5)
    %>% at_least(15 * days)
    %>% after(Surgery)
  )

  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature",
                duration_type = "at_least",
                duration = lubridate::duration(15, "days"))


  #check result
  expect_equal(model_state_equal("./csv/after/test3", model, query),
               TRUE)

})
