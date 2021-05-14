# to compare data frames:
# https://community.rstudio.com/t/all-equal-on-tibbles-ignores-attributes/4299/2

test_that("import events CSV  works", {
  # reset env
  setup_new_env()

  import_events_csv("./csv/import_events_csv/before-advanced.csv",
                    "PERSON",
                    "TIMESTAMP",
                    "TITLE")

  quiet_read_csv <- purrr::quietly(readr::read_csv)
  expected <-
    as.data.frame(
      quiet_read_csv(file = "./csv/import_events_csv/after.csv")$result)
  # to check dataframes without hash
  expect_equal(
    dplyr::all_equal(analysr_env$events[c("stat_unit", "date", "tag")],
                     expected), TRUE)

  # check that stat units have been added
  expect_equal(nrow(analysr_env$stat_units), 2)

  # check that tables are consistent
  expect_equal(check_tables_integrity(), TRUE)

  # check if current hash has changed in env
  expect_equal(analysr_env$current_hash, 5)
})

test_that("import events CSV works when import twice", {
  # reset env
  setup_new_env()

  # import twice
  import_events_csv("./csv/import_events_csv/before.csv",
                    "PERSON",
                    "TIMESTAMP",
                    "TITLE")
  import_events_csv("./csv/import_events_csv/before.csv",
                    "PERSON",
                    "TIMESTAMP",
                    "TITLE")

  quiet_read_csv <- purrr::quietly(readr::read_csv)
  expected <-
    as.data.frame(
      quiet_read_csv(file = "./csv/import_events_csv/after2.csv")$result)

  # to check dataframes without hash
  expect_equal(
    dplyr::all_equal(analysr_env$events[c("stat_unit", "date", "tag")],
                                expected), TRUE)

  # check that stat units have been added
  expect_equal(nrow(analysr_env$stat_units), 2)

  # check if current hash has changed in env
  expect_equal(analysr_env$current_hash, 7)

  # check that tables are consistent
  expect_equal(check_tables_integrity(), TRUE)
})

test_that("import events CSV works and fill descriptions", {
  # reset env
  setup_new_env()

  quiet_read_csv <- purrr::quietly(readr::read_csv)

  # import
  import_events_csv(
    "./csv/import_events_csv/before-optional-data.csv",
    "stat_unit",
    "date",
    "tag",
    c("context", "location"),
    date_format_reg = "ymd-HM"
  )

  expected <-
    as.data.frame(
      quiet_read_csv(file = "./csv/import_events_csv/after.csv")$result)
  # to check dataframes without hash
  expect_equal(dplyr::all_equal(
                  analysr_env$events[c("stat_unit", "date", "tag")], expected),
               TRUE)

  expected_descriptions <- as.data.frame(
    quiet_read_csv(
      file = "./csv/import_events_csv/after-descriptions.csv")$result
    )
  expected_descriptions <- transform(expected_descriptions,
                                     hash = as.integer(hash))
  # conflict when importing hash have to be an integer

  expect_equal(
            dplyr::all_equal(analysr_env$descriptions, expected_descriptions),
          TRUE)

  # check that tables are consistent
  expect_equal(check_tables_integrity(), TRUE)
})
