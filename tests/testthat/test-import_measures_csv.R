test_that("import measures CSV  works", {
  # reset env
  setup_new_env()
  import_measures_csv(
    "./csv/import_measures_csv/to_import_1.csv",
    "patient",
    "date_prlvt",
    "type_examen",
    "valeur"
  )
  expect_equal(model_state_equal("./csv/import_measures_csv/after1"), TRUE)
})
test_that("import measures CSV works when import twice", {
  # reset env
  setup_new_env()

  # import twice
  import_measures_csv(
    "./csv/import_measures_csv/to_import_2.csv",
    "patient",
    "date_prlvt",
    "type_examen",
    "valeur"
  )
  import_measures_csv(
    "./csv/import_measures_csv/to_import_2.csv",
    "patient",
    "date_prlvt",
    "type_examen",
    "valeur"
  )

  expect_equal(model_state_equal("./csv/import_measures_csv/after2"), TRUE)
})

test_that("import measures CSV  works and fill descriptions", {
  # reset env
  setup_new_env()
  import_measures_csv(
    "./csv/import_measures_csv/to_import_1.csv",
    "patient",
    "date_prlvt",
    "type_examen",
    "valeur",
    c("effectue_par")
  )

  expect_equal(model_state_equal("./csv/import_measures_csv/after3"), TRUE)
})
test_that("import measures CSV  works and fill descriptions with ';' separator", {
  # reset env
  setup_new_env()
  import_measures_csv(
    "./csv/import_measures_csv/to_import_4.csv",
    "patient",
    "date_prlvt",
    "type_examen",
    "valeur",
    c("effectue_par"),
    delim = ";"
  )

  # should be the same as the 3rd
  expect_equal(model_state_equal("./csv/import_measures_csv/after3"), TRUE)
})
test_that("import measures CSV works when importing different date formats", {

  # expected
  expected <- quiet_read_csv(
      file = "./csv/import_measures_csv/date/after.csv",
      col_types = readr::cols("hash" = "i")
  )$result

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

  expect_equal(dplyr::all_equal(expected,analysr_env$measures), TRUE)

})
