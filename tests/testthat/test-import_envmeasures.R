library(dplyr) # pour réaliser le test nécessaire: https://community.rstudio.com/t/all-equal-on-tibbles-ignores-attributes/4299/2

elocal <- new.env()

test_that("import measures CSV  works", {
  import_envmeasures_csv("./set-import-measures.csv")
  import_envmeasures_csv("./set-import-measures2.csv")
  id <- c(101929077,101929076,101929082,101929081,101929080) 
  date <- c(lubridate::ymd_hm("2006-10-23 12:00"), lubridate::ymd_hm("2006-11-10 11:00"), lubridate::ymd_hm("2006-11-30 12:00"), lubridate::ymd_hm("2006-11-01 14:00"),lubridate::ymd_hm("2006-12-12 12:00"))
  tag <- c("Cholesterol","Kaliemie", "Annemie", "Cholesterol", "Annemie")
  value <- c(0.4,4.7,1.3,4.1,0.8)


  excepted <- data.frame(id,date,tag,value)
  colnames(excepted) <- c("id", "date", "tag", "value")
  print(elocal$measures)
  print(excepted)
  expect_equal(all_equal(excepted, elocal$measures), TRUE)
})

