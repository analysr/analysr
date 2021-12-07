test_that("induce_measure works", {

  setup_new_env()

  load_env_csv("./csv/induce_measure/before1")

  induce_measure(analysr_env, "BMI", Weight / (Size * Size))

  expect_equal(model_state_equal("./csv/induce_measure/after1", analysr_env),
              TRUE)
})
test_that("replace_values works", {
  expect_equal("80/(1.82*1.82)", replace_values("Weight/(Size*Size)",
                                            c("Weight", "Size"), c(80,1.82)))
})
test_that("replace_values works", {
  test_func <- function(calcul) {
    calcul <- rlang::enexpr(calcul)
    calcul_as_string <- toString(calcul)
    calcul <- get_AST(calcul)
    tags <- get_tags(calcul)
    tags
  }
  expect_equal("Weight", test_func(add(8, 9, Weight)))
  expect_equal(c("Size", "Weight"),test_func(Size + add(Weight * divide(7), 8)))
})
