library(dplyr) # pour réaliser le test nécessaire: https://community.rstudio.com/t/all-equal-on-tibbles-ignores-attributes/4299/2

test_that("import periods CSV  works", {
  result <- import_periods_csv("./csv/import_periods_csv/before.csv", "PERSON","BEGIN","END","DESCRIPTION")

  excepted <- as.data.frame(readr::read_csv(file="./csv/import_periods_csv/after.csv"))

  expect_equal(all_equal(excepted, result), TRUE)
})

test_that("import periods CSV works in global variable", {

  # reset dataframe
  analysr_env$periods <- analysr_env$periods[0,]

  # import twice
  import_periods_csv("./csv/import_periods_csv/before.csv", "PERSON","BEGIN","END","DESCRIPTION")
  import_periods_csv("./csv/import_periods_csv/before.csv", "PERSON","BEGIN","END","DESCRIPTION")

  excepted <- as.data.frame(readr::read_csv(file="./csv/import_periods_csv/after2.csv"))

  expect_equal(all_equal(analysr_env$periods, excepted), TRUE)
})


