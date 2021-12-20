test_that("get_hash works", {

  setup_new_env()

  expect_equal(
    get_hash(1), 1
  )

  setup_new_env()

  expect_equal(
    get_hash(15), 1:15
  )

})
test_that("hash_from_stat_unit works", {

  setup_new_env()

  # import model
  load_env_csv("./csv/hash_from_stat_unit/env")

  res <- hash_from_stat_unit(analysr_env,
                                    c("0447625b-b860-483c-9f30-17ed375b1493",
                                     "f186b9f3-8c70-4bbc-8335-4bb2ccca0a3c",
                                     "0447625b-b860-483c-9f30-17ed375b1493"))

  expect_equal(res, c(18, 27, 18))


  # check model
  expect_equal(model_state_equal("./csv/hash_from_stat_unit/env", analysr_env), TRUE)


})
