test_that("alch_create_df construye tibble con columnas var* y metadatos", {
  resp <- desuctools::alch_get_survey_responses(survey_id = 8453877, page = 1)
  df <- desuctools::alch_create_df(resp)
  expect_s3_class(df, "tbl_df")
  expect_true(any(grepl("^var", names(df))))
  expect_true("id" %in% names(df)) # columna de metadatos
  expect_equal(nrow(df), length(resp$data))
})


test_that("alch_create_df retorna NA si una pregunta no tiene respuesta", {
  resp <- desuctools::alch_get_survey_responses(survey_id = 8453877, page = 1)
  df <- desuctools::alch_create_df(resp)
  # Busca una columna var* y verifica si hay al menos un NA
  var_cols <- grep("^var", names(df), value = TRUE)
  expect_true(any(is.na(df[[var_cols[1]]])))
})


test_that("alch_create_df asigna etiquetas de variable (label)", {
  resp <- desuctools::alch_get_survey_responses(survey_id = 8453877, page = 1)
  df <- desuctools::alch_create_df(resp)
  var_cols <- grep("^var", names(df), value = TRUE)
  expect_true(!is.null(attr(df[[var_cols[1]]], "label")))
})
