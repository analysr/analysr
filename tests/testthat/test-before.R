test_that("before works", {
  setup_new_env()

  # import model
  load_env_csv("./csv/before/before1")
  result <- (
  analysr_env
    %>% observed(Temperature > 38.5)
    %>% at_most(15 * days)
    %>% before("Surgery")
  )
  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature",
                duration_type = "at_most",
                duration = lubridate::duration(15, "days"))

  #expected_result
  exp_result <- c(101929077)

  #check result
  expect_equal(result, exp_result)

  # check model (model should have changed, query also)
  expect_equal(model_state_equal("./csv/before/after1", analysr_env, query),
              TRUE)




})
test_that("before works with at_most", {
  setup_new_env()

  # import model
  load_env_csv("./csv/before/test2")
  result <- (
    analysr_env
    %>% observed(Temperature > 38.5)
    %>% at_most(15 * days)
    %>% before("Surgery")
  )

  #expected_result
  exp_result <- c(1,7)

  #check result
  expect_equal(result, exp_result)

})
test_that("before works with at_least", {
  setup_new_env()

  # import model
  load_env_csv("./csv/before/test3")
  result <- (
    analysr_env
    %>% observed(Temperature > 38.5)
    %>% at_least(15 * days)
    %>% before("Surgery")
  )

  #expected_result
  exp_result <- c(5,1,6)

  #check result
  expect_equal(result, exp_result)

})
