# to compare dataframes :
# https://community.rstudio.com/t/all-equal-on-tibbles-ignores-attributes/4299/2

test_that("import measures CSV  works", {
  # reset env
  setup_new_env()
  import_measures_csv(
    "./csv/import_measures_csv/before-advanced.csv",
    "patient",
    "date_prlvt",
    "type_examen",
    "valeur"
  )

  quiet_read_csv <- purrr::quietly(readr::read_csv)
  expected <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_measures_csv/after.csv")$result
    )

  # to check dataframes without hash
  expect_equal(
    dplyr::all_equal(
      analysr_env$measures[c("stat_unit", "date", "tag", "value")],
      expected), TRUE)

  # check that stat units have been added
  expect_equal(nrow(analysr_env$stat_units), 2)

  # check that tables are consistent
  expect_equal(check_tables_integrity(), TRUE)

  # check if current hash has changed in env
  expect_equal(analysr_env$current_hash, 5)
})

test_that("import measures CSV works when import twice", {
  # reset env
  setup_new_env()

  # import twice
  import_measures_csv(
    "./csv/import_measures_csv/before.csv",
    "patient",
    "date_prlvt",
    "type_examen",
    "valeur"
  )
  import_measures_csv(
    "./csv/import_measures_csv/before.csv",
    "patient",
    "date_prlvt",
    "type_examen",
    "valeur"
  )

  quiet_read_csv <- purrr::quietly(readr::read_csv)

  expected <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_measures_csv/after2.csv")$result)

  # to check dataframes without hash
  expect_equal(
    dplyr::all_equal(expected,
      analysr_env$measures[c("stat_unit", "date", "tag", "value")]), TRUE)
  # check that stat units have been added
  expect_equal(nrow(analysr_env$stat_units), 2)

  # check if current hash has changed in env
  expect_equal(analysr_env$current_hash, 7)

  # check that tables are consistent
  expect_equal(check_tables_integrity(), TRUE)
})

test_that("import measures CSV  works and fill descriptions", {
  # reset env
  setup_new_env()
  import_measures_csv(
    "./csv/import_measures_csv/before-advanced.csv",
    "patient",
    "date_prlvt",
    "type_examen",
    "valeur",
    c("effectue_par")
  )

  quiet_read_csv <- purrr::quietly(readr::read_csv)
  expected <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_measures_csv/after.csv")$result
    )

  expect_equal(
    dplyr::all_equal(
      analysr_env$measures[c("stat_unit", "date", "tag", "value")],
      expected), TRUE)



  expected_descriptions <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_measures_csv/after-descriptions.csv")$result
    )
  expected_descriptions <-
                      transform(expected_descriptions, hash = as.integer(hash))
  # conflict when importing hash have to be an integer

  expect_equal(dplyr::all_equal(
      analysr_env$descriptions, expected_descriptions
  ), TRUE)

  # check that tables are consistent
  expect_equal(check_tables_integrity(), TRUE)
})