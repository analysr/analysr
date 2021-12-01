test_that("import measures CSV  works", {
  # reset env
  setup_new_env()
  import_stat_units_csv(
    "./csv/import_stat_units_csv/to_import_1.csv",
    "UserId",
    c("BIRTHDATE","DEATHDATE","FIRST","LAST")
  )
  expect_equal(model_state_equal("./csv/import_stat_units_csv/after1"), TRUE)
})
