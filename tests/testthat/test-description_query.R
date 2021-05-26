test_that("description_query works", {

  load_env_csv(save_path = "./csv/load_env_csv/save")

  print(description_query("Germany", "Location"))
  print(read.csv("./csv/description_query/after1.csv"))

  expect_equal(
    dplyr::all_equal(
      description_query("Germany", "Location"),
      read.csv("./csv/description_query/after1.csv"), convert = TRUE), TRUE)
})
