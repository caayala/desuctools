testthat::skip_on_ci()
testthat::skip_if_offline()

resp <- desuctools::alch_get_survey_responses(survey_id = 8453877, page = 1)
df <- desuctools::alch_create_df(resp)

test_that("alch_create_df construye tibble con columnas var* y metadatos", {
  expect_s3_class(df, "tbl_df")
  expect_true(any(grepl("^var", names(df))))
  expect_true("id" %in% names(df)) # columna de metadatos
  expect_equal(nrow(df), length(resp$data))
})

test_that("alch_create_df retorna NA si una pregunta no tiene respuesta", {
  # Busca una columna var* y verifica si hay al menos un NA
  var_cols <- grep("^var", names(df), value = TRUE)
  skip_if(length(var_cols) == 0, "No hay columnas var*")
  # Busca en todas las var* en vez de solo la primera
  tiene_na <- any(vapply(
    df[var_cols],
    function(x) any(is.na(x)),
    logical(1)
  ))
  testthat::expect_true(tiene_na)
})

test_that("alch_create_df asigna etiquetas de variable (label)", {
  var_cols <- grep("^var", names(df), value = TRUE)
  testthat::skip_if(length(var_cols) == 0, "No hay columnas var*")
  # Verifica que al menos una var* tenga etiqueta
  tiene_label <- any(vapply(
    df[var_cols],
    function(x) !is.null(attr(x, "label")),
    logical(1)
  ))
  testthat::expect_true(tiene_label)
})
