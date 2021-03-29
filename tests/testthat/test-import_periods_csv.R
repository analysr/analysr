# pour réaliser le test nécessaire: https://community.rstudio.com/t/all-equal-on-tibbles-ignores-attributes/4299/2

test_that("import periods CSV  works", {
  # reset dataframe
  analysr_env$periods <- analysr_env$periods[0, ]

  import_periods_csv("./csv/import_periods_csv/before.csv",
                     "PERSON",
                     "BEGIN",
                     "END",
                     "DESCRIPTION")

  quiet_read_csv <- purrr::quietly(readr::read_csv)
  excepted <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_periods_csv/after.csv")$result
    )

  expect_equal(dplyr::all_equal(analysr_env$periods, excepted), TRUE)
})

test_that("import periods CSV works when import twice", {
  # reset dataframe
  analysr_env$periods <- analysr_env$periods[0, ]

  # import twice
  import_periods_csv("./csv/import_periods_csv/before.csv",
                     "PERSON",
                     "BEGIN",
                     "END",
                     "DESCRIPTION")
  import_periods_csv("./csv/import_periods_csv/before.csv",
                     "PERSON",
                     "BEGIN",
                     "END",
                     "DESCRIPTION")

  quiet_read_csv <- purrr::quietly(readr::read_csv)
  excepted <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_periods_csv/after2.csv"
    )$result)


  expect_equal(dplyr::all_equal(analysr_env$periods, excepted), TRUE)
})
