test_that("get_hash works for one element", {
  setup_new_env()
  expect_equal(get_hash(1), 0)
})

test_that("get_hash works for a vector", {
  setup_new_env()
  expect_equal(get_hash(5), 0:4)
})