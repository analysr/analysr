test_that("induce_measure works", {

  setup_new_env()
})
test_that("replace_values works", {
  expect_equal("80/(1.82*1.82)", replace_values("Weight/(Length*Length)", c("Weight", "Length"), c(80,1.82)))
})
test_that("replace_values works", {
  test_func <- function(calcul) {
    calcul <- rlang::enexpr(calcul)
    calcul_as_string <- toString(calcul)
    calcul <- get_AST(calcul)
    tags <- get_tags(calcul)
    tags
  }
  expect_equal("Weight",test_func(add(8,9,Weight)))
  expect_equal(c("Length","Weight"),test_func(Length+add(Weight*divide(7),8)))
})
