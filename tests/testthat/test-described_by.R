test_that("described_by works", {

    setup_new_env()

    # import model
    load_env_csv("./csv/described_by/test1")
      (analysr_env
      %>% observed(Temperature > 38)
      %>% described_by(Gender == "Male")
      )
    query <- list(condition = rlang::expr(Temperature > 38),
                  tag = "Temperature")


    # check model (model should have changed, query also)
    expect_equal(model_state_equal("./csv/described_by/test1", analysr_env, query),
                 TRUE)
})
