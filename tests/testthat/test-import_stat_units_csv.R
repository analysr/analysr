test_that("import stat_units CSV  works", {
  # reset env
  setup_new_env()
  import_stat_units_csv(
    "./csv/import_stat_units_csv/to_import_1.csv",
    "UserId",
    c("BIRTHDATE", "DEATHDATE", "FIRST", "LAST", "GENDER")
  )
  expect_equal(model_state_equal("./csv/import_stat_units_csv/after1"), TRUE)
})
test_that("import stat_units works when a stat_unit already exists in table", {
  # reset env
  setup_new_env()
  import_stat_units_csv(
    "./csv/import_stat_units_csv/to_import_1.csv",
    "UserId",
    c("BIRTHDATE", "DEATHDATE", "FIRST", "LAST", "GENDER")
  )
  expect_equal(model_state_equal("./csv/import_stat_units_csv/after2"), TRUE)
})
