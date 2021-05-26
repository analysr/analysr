
quiet_read_csv <- purrr::quietly(readr::read_csv)

test_that("import events CSV  works", {
  # reset env
  setup_new_env()

  import_events_csv("./csv/import_events_csv/to_import_1.csv",
                    "PERSON",
                    "TIMESTAMP",
                    "TITLE")

  expect_equal(model_state_equal("./csv/import_events_csv/after1"), TRUE)

})

test_that("import events CSV works when import twice", {
  # reset env
  setup_new_env()

  # import twice
  import_events_csv("./csv/import_events_csv/to_import_2.csv",
                    "PERSON",
                    "TIMESTAMP",
                    "TITLE")
  import_events_csv("./csv/import_events_csv/to_import_2.csv",
                    "PERSON",
                    "TIMESTAMP",
                    "TITLE")

  expect_equal(model_state_equal("./csv/import_events_csv/after2"), TRUE)
})

test_that("import events CSV works and fill descriptions", {
  # reset env
  setup_new_env()

  # import
  import_events_csv(
    "./csv/import_events_csv/to_import_3.csv",
    "stat_unit",
    "date",
    "tag",
    c("context", "location"),
    date_format_reg = "ymd-HM"
  )

  expect_equal(model_state_equal("./csv/import_events_csv/after3"), TRUE)
})
test_that("import events CSV works when importing different date formats", {

  # expected
  expected <- as.data.frame(quiet_read_csv(
      file = "./csv/import_events_csv/date/after.csv",
      col_types = readr::cols("hash" = "i")
  )$result)

  # import ymd-HM
  setup_new_env()
  import_events_csv(
    "./csv/import_events_csv/date/before-ymd-HM.csv",
    date_format_reg = "ymd-HM"
  )

  expect_equal(
    dplyr::all_equal(expected,
      analysr_env$events), TRUE)

  # import dmy-HMS
  setup_new_env()
  import_events_csv(
    "./csv/import_events_csv/date/before-dmy-HMS.csv",
    date_format_reg = "dmy-HMS"
  )

  expect_equal(
    dplyr::all_equal(expected,
      analysr_env$events), TRUE)
})
