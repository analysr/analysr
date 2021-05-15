test_that("fix_granularity works", {

  stat_unit <- c(101929076, 101929077, 101929077, rep(101929076, 9))
  date <- lubridate::ymd_hm(c("06-11-10 11:00", "06-10-23 12:00",
                              "06-11-10 12:00", "06-11-11 12:00",
                              "06-11-11 20:00", "06-11-13 11:00",
                              "06-11-16 11:00", "06-11-16 13:00",
                              "06-11-16 15:00", "06-11-16 16:00",
                              "06-11-17 11:00", "06-11-17 13:00"))
  tag <- c("Kaliemie", "Cholesterol", rep("Kaliemie", 10))
  value <- c(4.7, 0.4, 5.1, 7.4, 6.8, 5.3, 8.2, 7.9, 8.3, 8.5, 7.4, 5.8)
  d <- data.frame(stat_unit, date, tag, value)

  result <- fix_granularity(data = d,
                   tag_wanted = "Kaliemie",
                   period_start = lubridate::ymd_hm("06-11-10 10:00"),
                   period_end = lubridate::ymd_hm("06-11-17 10:00"),
                   temporal_granularity = lubridate::days(),
                   stat_unit_wanted = 101929076)

  stat_unit <- rep(101929076, 8)
  date <- lubridate::ymd_hm("06-11-10 10:00") + lubridate::days() * c(0:7)
  tag <- rep("Kaliemie", 8)
  value <- c(4.7, 7.1, 6.2, 5.3, 6.275, 7.250, 8.225, 6.6)
  status <- c("AGGREGATED", "AGGREGATED", "IMPUTED", "AGGREGATED", "IMPUTED",
              "IMPUTED", "AGGREGATED", "AGGREGATED")

  expected_result <- data.frame(stat_unit, date, tag, value, status)


  expect_equal(result, expected_result)



})
