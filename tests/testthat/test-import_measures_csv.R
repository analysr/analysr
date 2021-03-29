library(dplyr) # pour réaliser le test nécessaire: https://community.rstudio.com/t/all-equal-on-tibbles-ignores-attributes/4299/2

test_that("import measures CSV  works", {
  result <- import_measures_csv("./csv/import_measures_csv/before.csv", "patient", "date_prlvt", "type_examen", "valeur")

  stat_unit <- c(101929077, 101929076)
  date <- c(lubridate::ymd_hm("2006-10-23 12:00"), lubridate::ymd_hm("2006-11-10 11:00"))
  tag <- c("Cholesterol","Kaliemie")
  value <- c(0.4,4.7)
  excepted <- data.frame(stat_unit,date,tag,value)
  colnames(excepted) <- c("stat_unit", "date", "tag", "value")

  expect_equal(all_equal(excepted, result), TRUE)
})

test_that("import measures CSV works in global variable", {

  # reset dataframe
  analysr_env$measures <- analysr_env$measures[0,]

  # import twice
  import_measures_csv("./csv/import_measures_csv/before.csv", "patient", "date_prlvt", "type_examen", "valeur")
  import_measures_csv("./csv/import_measures_csv/before.csv", "patient", "date_prlvt", "type_examen", "valeur")

  excepted <- as.data.frame(readr::read_csv(file="./csv/import_measures_csv/after2.csv"))

  expect_equal(all_equal(analysr_env$measures, excepted), TRUE)
})


