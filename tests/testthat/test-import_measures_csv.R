# to compare dataframes :
# https://community.rstudio.com/t/all-equal-on-tibbles-ignores-attributes/4299/2

quiet_read_csv <- purrr::quietly(readr::read_csv)

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

  expected <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_measures_csv/after.csv",
      col_types = readr::cols("hash" = "i")
    )$result)

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

  expected <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_measures_csv/after2.csv",
      col_types = readr::cols("hash" = "i")
    )$result)

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

  expected <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_measures_csv/after.csv",
      col_types = readr::cols("hash" = "i")
    )$result)

  expect_equal(
    dplyr::all_equal(
      analysr_env$measures[c("stat_unit", "date", "tag", "value")],
      expected), TRUE)



  expected_descriptions <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_measures_csv/after-descriptions.csv",
      col_types = readr::cols("hash" = "i")
  )$result)

  # conflict when importing hash have to be an integer

  expect_equal(dplyr::all_equal(
      analysr_env$descriptions, expected_descriptions
  ), TRUE)

  # check that tables are consistent
  expect_equal(check_tables_integrity(), TRUE)
})
test_that("import measures CSV works when importing different date formats", {

  # expected
  expected <- as.data.frame(quiet_read_csv(
      file = "./csv/import_measures_csv/date/after.csv",
      col_types = readr::cols("hash" = "i")
  )$result)

  # import ymd-HMS
  setup_new_env()
  import_measures_csv(
    "./csv/import_measures_csv/date/before-ymd-HMS.csv"
  )

  expect_equal(
    dplyr::all_equal(expected,
      analysr_env$measures), TRUE)

  # import dmy-HM
  setup_new_env()
  import_measures_csv(
    "./csv/import_measures_csv/date/before-dmy-HM.csv",
    date_format_reg = "dmy-HM"
  )

  expect_equal(
    dplyr::all_equal(expected,
      analysr_env$measures), TRUE)


  # import ymd-HM
  setup_new_env()
  import_measures_csv(
    "./csv/import_measures_csv/date/before-ymd-HM.csv",
    date_format_reg = "ymd-HM"
  )

  expect_equal(
    dplyr::all_equal(expected,
      analysr_env$measures), TRUE)

})
