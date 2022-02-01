test_that("import_FHIR works", {
  setup_new_env()
  import_fhir("./fhir/import_FHIR/",
              c("onsetDateTime"),
              c("beginDate"),
              c("endDate"),
              c("Condition"),
              c("value"))
  show_env()
})
