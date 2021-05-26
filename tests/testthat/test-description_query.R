# TODO: remove when merging
quiet_read_csv <- purrr::quietly(readr::read_csv)


test_that("description_query works", {

  load_env_csv(save_path = "./csv/description_query/before1")


  # TODO: do a function to reduce size of it
  expected <- as.data.frame(quiet_read_csv(
    file = "./csv/description_query/after1.csv",
    col_types = readr::cols("hash" = "i","stat_unit" = "c"))$result)


  expect_equal(dplyr::all_equal(description_query("Germany", "Location"),
                                expected, convert = TRUE), TRUE)
})
