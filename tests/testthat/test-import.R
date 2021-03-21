library(dplyr) # pour réaliser le test nécessaire: https://community.rstudio.com/t/all-equal-on-tibbles-ignores-attributes/4299/2

test_that("import CSV works", {
  result <- import_CSV("./set.csv")

  stat_unit <- c(101929077, 101929077, 101929077, 101929077, 101929076, 101929077, 101929077, 101929077)
  date <- c(lubridate::ymd_hm("2003-10-23 12:00"), lubridate::ymd_hm("2006-11-23 12:00"), lubridate::ymd_hm("2006-11-25 12:00"), lubridate::ymd_hm("2006-11-25 17:00"), lubridate::ymd_hm("2006-11-27 12:00"), lubridate::ymd_hm("2006-11-27 12:00"), lubridate::ymd_hm("2006-12-18 12:00"), lubridate::ymd_hm("2016-11-25 17:00"))
  tag <- c("Kaliemie", "Kaliemie","Kaliemie","Cholesterol","Kaliemie","Kaliemie","Kaliemie", "Kaliemie")
  value <- c(4.7,3.7,6.3,0.9,5.4,3.0,2.1,4)


  excepted <- data.frame(stat_unit,date,tag,value)
  colnames(excepted) <- c("STAT_UNIT", "DATE", "TAG", "VALUE")

  expect_equal(all_equal(excepted, result), TRUE)
})
