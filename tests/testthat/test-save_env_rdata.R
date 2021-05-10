test_that("save_env_rdata works", {
  # reset env
  setup_new_env()

  save_env_rdata("./tmp/save_env_rdata/")

  # check if file exist
  expect_equal(length(
    Sys.glob(file.path("./tmp/save_env_rdata/", "*.RData"))), 1)
})
