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
