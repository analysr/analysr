library(dplyr) # pour réaliser le test nécessaire: https://community.rstudio.com/t/all-equal-on-tibbles-ignores-attributes/4299/2

test_that("import period CSV works", {
  result <- import_period_CSV("./set-import-period.csv")

  stat_unit <- c(101929077, 101929076)
  begin <- c(lubridate::ymd_hm("2006-10-23 12:00"), lubridate::ymd_hm("2006-11-10 11:00"))
  end <- c(lubridate::ymd_hm("2006-11-23 18:00"), lubridate::ymd_hm("2006-11-15 09:00"))


  excepted <- data.frame(stat_unit,begin,end)
  colnames(excepted) <- c("STAT_UNIT", "BEGIN", "END")
  #print(result)
  #print(excepted)
  expect_equal(all_equal(excepted, result), TRUE)
})


test_that("import period CSV 2 works", {
  result <- import_period_CSV_2("./set-import-period-2.csv", "PERSON", "START", "END")

  stat_unit <- c(101929077, 101929076)
  begin <- c(lubridate::ymd_hm("2006-10-23 12:00"), lubridate::ymd_hm("2006-11-10 11:00"))
  end <- c(lubridate::ymd_hm("2006-11-23 18:00"), lubridate::ymd_hm("2006-11-15 09:00"))


  excepted <- data.frame(stat_unit,begin,end)
  colnames(excepted) <- c("STAT_UNIT", "BEGIN", "END")
  #print(result)
  #print(excepted)
  expect_equal(all_equal(excepted, result), TRUE)
})
