# pour réaliser le test nécessaire: 
# https://community.rstudio.com/t/all-equal-on-tibbles-ignores-attributes/4299/2

test_that("import events CSV  works", {
  # reset env
  setup_new_env()

  import_events_csv("./csv/import_events_csv/before-advanced.csv",
                     "PERSON",
                     "TIMESTAMP",
                     "TITLE")

  quiet_read_csv <- purrr::quietly(readr::read_csv)
  excepted <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_events_csv/after.csv")$result
    )
  expect_equal(dplyr::all_equal(analysr_env$events, excepted), TRUE)
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
  excepted <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_events_csv/after2.csv"
    )$result)


  expect_equal(dplyr::all_equal(analysr_env$events, excepted), TRUE)
})
