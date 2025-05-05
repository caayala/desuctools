df_test <- tibble::tibble(
  sexo = haven::labelled(
    c(1, 2, 2, 2),
    labels = c("M" = 1, "W" = 2),
    label = "Sex"
  ),
  edad = haven::labelled(
    c(1, 1, 2, 2),
    labels = c("young" = 1, "old" = 2),
    label = "Age"
  ),
  c_int = c(1, 1, 1, 1),
  cat = c(1, 1, 2, 2),
  cat_na = c(1, 1, 2, NA),
  wt = c(2, 1, 1, 0)
)

# tabla_categoria ---------------------------------------------------------

test_that("tabla_categorias proporcion de categoría labelled", {
  expect_identical(tabla_categorias(df_test, sexo)[["prop"]], c(0.25, 0.75))
})

test_that("tabla_categorias proporcion de dos variables labelled", {
  expect_identical(
    tabla_categorias(df_test, sexo, edad)[["prop"]],
    c(0.25, 0.75, 0.50, 0.50)
  )
})

test_that("tabla_categoria agrega en pregunta_lab la etiqueta de categoria", {
  expect_equal(
    tabla_categorias(df_test, sexo, edad)[["pregunta_lab"]] %>%
      as.character(),
    c("Sex", "Sex", "Age", "Age")
  )
})

test_that("tabla_categoria proporcion de categoría numerica", {
  expect_identical(tabla_categorias(df_test, cat)[["prop"]], c(0.5, 0.5))
})

test_that("tabla_categoria proporcion de categoría numerica con missing", {
  expect_identical(
    tabla_categorias(df_test, cat_na)[["prop"]],
    c(0.5, 0.25, 0.25)
  )
})

test_that("tabla_categoria proporcion de categoría labelled con peso", {
  expect_equal(tabla_categorias(df_test, sexo, .wt = wt)[["prop"]], c(0.5, 0.5))
})

test_that("tabla_categoria proporcion de categoría numerica con missing con peso", {
  expect_equal(
    tabla_categorias(df_test, cat_na, .wt = wt)[["prop"]],
    c(0.75, 0.25, 0)
  )
})


# tabla_vars_segmentos ----------------------------------------------------

test_that("tabla_var_segmento proporcion de categoría y total", {
  expect_equal(
    desuctools:::tabla_var_segmento(
      df_test,
      .var = "sexo",
      .segmento = "cat",
      total = TRUE
    )[["prop"]],
    c(0.5, 0.5, 0, 1, 0.25, 0.75)
  )
})


test_that("tabla_vars_segmentos proporcion de categoría y total", {
  expect_equal(
    tabla_vars_segmentos(
      df_test,
      .vars = sexo,
      .segmentos = cat,
      total = TRUE
    )[["prop"]],
    c(0.5, 0.5, 0, 1, 0.25, 0.75)
  )
})

test_that("tabla_categoria proporcion de categoría y total y missing factor", {
  expect_equal(
    tabla_vars_segmentos(
      df_test,
      .vars = sexo,
      .segmentos = cat,
      miss = "M",
      total = TRUE
    )[["prop_val"]],
    c(NA, 1, NA, 1, NA, 1)
  )
})

test_that("tabla_categoria proporcion de categoría y total y missing numérico", {
  expect_equal(
    tabla_vars_segmentos(
      df_test,
      .vars = cat_na,
      .segmentos = sexo,
      miss = c(2, NA),
      total = FALSE
    )[["prop_val"]],
    c(1, NA, NA, 1, NA, NA)
  )
})

test_that("tabla_categoria proporcion de varias categorías", {
  expect_equal(
    tabla_vars_segmentos(
      df_test,
      .vars = cat,
      .segmentos = c(sexo, edad),
      total = FALSE
    )[["casos"]],
    c(1, 0, 1, 2, 2, 0, 0, 2)
  )
})

test_that("tabla_categoria proporcion de categoría con segmento constante", {
  expect_equal(
    tabla_vars_segmentos(
      df_test,
      .vars = cat_na,
      .segmentos = NULL,
      miss = c(NA),
      total = FALSE
    )[["prop_val"]],
    c(2 / 3, 1 / 3, NA)
  )
})

# tabla_vars_segmentos Generic S3 Method ----------------------------------

# Test that S3 dispatch works with different input classes
test_that("tabla_vars_segmentos S3 generic dispatches correctly for data.frame", {
  result <- tabla_vars_segmentos(
    df_test,
    .vars = sexo,
    .segmentos = cat
  )

  # Check that the result has expected columns
  expect_true(all(
    c(
      "segmento_var",
      "segmento_cat",
      "pregunta_var",
      "pregunta_cat",
      "prop"
    ) %in%
      names(result)
  ))
  expect_equal(nrow(result), 4) # 2 categories in sexo × 2 categories in cat
})

test_that("tabla_vars_segmentos S3 generic works with tibble", {
  # Convert to tibble explicitly
  df_tibble <- tibble::as_tibble(df_test)

  result <- tabla_vars_segmentos(
    df_tibble,
    .vars = sexo,
    .segmentos = cat
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4) # 2 categories in sexo × 2 categories in cat
})

test_that("tabla_vars_segmentos.data.frame handles multiple variables", {
  result <- tabla_vars_segmentos(
    df_test,
    .vars = c(sexo, edad),
    .segmentos = cat
  )

  # Should have 8 rows: (2 categories in sexo + 2 categories in edad) × 2 categories in cat
  expect_equal(nrow(result), 8)
  expect_equal(
    sort(unique(result$pregunta_var)),
    sort(c("sexo", "edad"))
  )
})

test_that("tabla_vars_segmentos.data.frame handles weighted data correctly", {
  result_unweighted <- tabla_vars_segmentos(
    df_test,
    .vars = sexo,
    .segmentos = cat
  )

  result_weighted <- tabla_vars_segmentos(
    df_test,
    .vars = sexo,
    .segmentos = cat,
    .wt = wt
  )

  # The proportions should be different when weighted
  expect_false(identical(result_unweighted$prop, result_weighted$prop))
})

test_that("tabla_vars_segmentos.data.frame handles missings with prop_val", {
  result <- tabla_vars_segmentos(
    df_test,
    .vars = cat_na,
    .segmentos = sexo,
    miss = c(2, NA)
  )

  # Check that prop_val is calculated
  expect_true("prop_val" %in% names(result))

  # Categories marked as missing should have NA in prop_val
  missing_rows <- result$pregunta_cat %in% c("2", "(Missing)")
  expect_true(all(is.na(result$prop_val[missing_rows])))
})

#  rec_cat_5a3 ------------------------------------------------------------

test_that("rec_cat_5a3 mantiene la etiqueta de la variable", {
  expect_equal(attr(rec_cat_5a3(df_test$sexo), "label", exact = TRUE), "Sex")
})
