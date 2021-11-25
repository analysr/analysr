# those tests can de facto test prepare_query function
test_that("observed works with simple condition", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before1")

  # observed
  model <- observed(analysr_env, Temperature > 38.5)

  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature")

  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before1", model, query), TRUE)
})
test_that("observed works with simple condition (import)", {
  setup_new_env()

  # import measures
  import_measures_csv("./csv/observed/before7/measures.csv")

  # observed
  model <- observed(analysr_env, Temperature > 38.5)

  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature")

  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before7", model, query), TRUE)
})
test_that("observed works  with simple reverse condition", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before1")

  # observed
  model <- observed(analysr_env, 38.5 < Temperature)


  query <- list(condition = rlang::expr(38.5 < Temperature),
                tag = "Temperature")


  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before1", model, query), TRUE)
})
test_that("observed works on events", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before2")

  # observed
  model <- observed(analysr_env, Hemorrhage)


  query <- list(condition = rlang::expr(Hemorrhage), tag = "Hemorrhage")


  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before2", model, query), TRUE)
})
test_that("observed works on desciption without value", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before3")

  # observed
  model <- observed(analysr_env, Covid_Positive)


  query <- list(condition = rlang::expr(Covid_Positive), tag = "Covid_Positive")


  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before3", model, query), TRUE)
})
test_that("observed works on desciption with value", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before4")

  # observed
  model <- observed(analysr_env, Age > 5)

  query <- list(condition = rlang::expr(Age > 5), tag = "Age")


  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before4", model, query), TRUE)
})
test_that("observed works on desciption with value reversed", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before4")

  # observed
  model <- observed(analysr_env,  5 < Age)

  query <- list(condition = rlang::expr(5 < Age), tag = "Age")


  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before4", model, query), TRUE)
})
test_that("observed works on desciption with value", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before5")

  # observed
  model <- observed(analysr_env, Covid_Positive == TRUE)

  query <- list(condition = rlang::expr(Covid_Positive == TRUE),
                tag = "Covid_Positive")


  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before5", model, query), TRUE)
})
test_that("observed works on periods", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before6")

  # observed
  model <- observed(analysr_env, Hospitalization)


  query <- list(condition = rlang::expr(Hospitalization),
                tag = "Hospitalization")


  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before6", model, query), TRUE)
})
test_that(
  "observed works on measures described (request on description table)", {

  setup_new_env()

  # load measures
  load_env_csv("./csv/observed/before8")

  # observed
  model <- observed(analysr_env, Location == "Austria")

  query <- list(condition = rlang::expr(Location == "Austria"),
                tag = "Location")

  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before8", model, query), TRUE)
})
test_that(
  "observed works on periods described (request on description table)", {

  setup_new_env()

  # load measures
  load_env_csv("./csv/observed/before9")

  # observed
  model <- observed(analysr_env, Location == "Paris")

  query <- list(condition = rlang::expr(Location == "Paris"),
                tag = "Location")

  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before9", model, query), TRUE)
})
test_that(
  "observed works on periods described (request on description table) 2", {

  setup_new_env()

  # load measures
  load_env_csv("./csv/observed/before10")

  # observed
  model <- observed(analysr_env, Location)

  query <- list(condition = rlang::expr(Location),
                tag = "Location")

  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before10", model, query), TRUE)
})

test_that(
  "observed works on events described (request on description table)", {

  setup_new_env()

  # load measures
  load_env_csv("./csv/observed/before11")

  # observed
  model <- observed(analysr_env, Location == "Paris")

  query <- list(condition = rlang::expr(Location == "Paris"),
                tag = "Location")

  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before11", model, query), TRUE)
})
