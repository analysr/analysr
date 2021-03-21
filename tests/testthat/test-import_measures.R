library(dplyr) # pour réaliser le test nécessaire: https://community.rstudio.com/t/all-equal-on-tibbles-ignores-attributes/4299/2



test_that("import measures CSV  works", {
  result <- import_measures_csv("./set-import-measures.csv")

  id <- c(101929077, 101929076)
  date <- c(lubridate::ymd_hm("2006-10-23 12:00"), lubridate::ymd_hm("2006-11-10 11:00"))
  tag <- c("Cholesterol","Kaliemie")
  value <- c(0.4,4.7)


  excepted <- data.frame(id,date,tag,value)
  colnames(excepted) <- c("id", "date", "tag", "value")
  print(result)
  print(excepted)
  expect_equal(all_equal(excepted, result), TRUE)
})
