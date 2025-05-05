d <- data.frame(
  x = factor(c(rep(letters[1:3], 6), letters[1:2]), levels = letters[1:4]),
  fct = factor(c(rep(1, 12), rep(0, 8)), labels = c("No", "Si")),
  lab = haven::labelled(
    c(rep(1, 12), rep(0, 8)),
    labels = c("Si" = 1, "No" = 0)
  ),
  lab_na = haven::labelled(
    c(rep(1, 12), rep(0, 7), NA),
    labels = c("Sí" = 1, "No" = 0, "NS/NR" = 9)
  ),
  chr = c(rep("Si", 12), rep("No", 7), NA_character_),
  num = c(rep(1, 12), rep(0, 8)),
  esc = 1:20,
  esc_na = c(NA_integer_, 2:20),
  wgt = c(rep(.5, 10), rep(1.5, 10))
)

s <- srvyr::as_survey_design(d, weights = wgt)

# svy_tabla_var_segmento ---------------------------------------------------------

# Resultados sin segmentos

test_that("svy_tabla_var_segmento proporcion de categoría factor", {
  expect_identical(
    svy_tabla_var_segmento(s, .var = "fct")$pregunta_cat,
    factor(c("No", "Si"))
  )
})

test_that("svy_tabla_var_segmento proporcion de categoría labelled", {
  expect_identical(
    svy_tabla_var_segmento(s, .var = "lab")$pregunta_cat,
    factor(c("No", "Si"))
  )
})

test_that("svy_tabla_var_segmento mismos resultados entre factor y labelled", {
  expect_identical(
    svy_tabla_var_segmento(s, .var = "fct")$prop,
    svy_tabla_var_segmento(s, .var = "lab")$prop
  )
})

test_that("svy_tabla_var_segmento media de escalar", {
  expect_s3_class(svy_tabla_var_segmento(s, .var = "esc"), "tbl_df")
})

test_that("svy_tabla_var_segmento proporcion de categoría labelled", {
  expect_s3_class(svy_tabla_var_segmento(s, .var = "lab_na"), "tbl_df")
})

# Resultados con segmentos

test_that("svy_tabla_var_segmento proporcion de categoría factor con segmento", {
  expect_identical(
    svy_tabla_var_segmento(s, .var = "fct", .segmento = "x")$casos,
    c(4.5, 2, 4.5, 3, 3, 3, 0, 0)
  )
})

test_that("svy_tabla_var_segmento proporcion de categoría labelled con segmento", {
  expect_identical(
    svy_tabla_var_segmento(s, .var = "lab", .segmento = "x")$casos,
    c(4.5, 2, 4.5, 3, 3, 3, 0, 0)
  )
})

test_that("svy_tabla_var_segmento mismos resultados entre factor y labelled", {
  expect_identical(
    svy_tabla_var_segmento(s, .var = "fct", .segmento = "x")$prop,
    svy_tabla_var_segmento(s, .var = "lab", .segmento = "x")$prop
  )
})

test_that("svy_tabla_var_segmento proporcion labelled con segmento", {
  expect_s3_class(
    svy_tabla_var_segmento(s, .var = "esc", .segmento = "x"),
    "tbl_df"
  )
})

test_that("svy_tabla_var_segmento media escalar con segmento", {
  expect_s3_class(
    svy_tabla_var_segmento(s, .var = "esc", .segmento = "lab_na"),
    "tbl_df"
  )
})

test_that("svy_tabla_var_segmento proporcion de categoría labelled", {
  expect_s3_class(
    svy_tabla_var_segmento(s, .var = "lab_na", .segmento = "x"),
    "tbl_df"
  )
})

# svy_tabla_vars_segmentos ---------------------------------------------------------

# Resultados sin segmentos

test_that("svy_tabla_vars_segmento proporcion de categoría factor", {
  expect_identical(
    svy_tabla_vars_segmentos(s, .vars = fct)$pregunta_cat,
    factor(c("No", "Si"))
  )
})

test_that("svy_tabla_vars_segmentos proporcion labelled con segmento", {
  expect_s3_class(
    svy_tabla_vars_segmentos(s, .vars = esc, .segmentos = c(x)),
    "tbl_df"
  )
})

test_that("svy_tabla_vars_segmentos proporcion labelled con segmento", {
  expect_s3_class(
    svy_tabla_vars_segmentos(
      s,
      .vars = fct,
      .segmentos = c(x, lab)
    )$segmento_cat,
    "factor"
  )
})

# tabla_vars_segmentos S3 Method for tbl_svy --------------------------------

test_that("tabla_vars_segmentos S3 generic dispatches correctly for tbl_svy", {
  # Test that the S3 dispatch works properly with survey objects
  result <- tabla_vars_segmentos(
    s,
    .vars = fct,
    .segmentos = NULL
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
  expect_equal(result$segmento_cat, factor("Total"))
})

test_that("tabla_vars_segmentos and svy_tabla_vars_segmentos produce identical results", {
  # The new S3 method should produce the same results as the original function
  result_s3 <- tabla_vars_segmentos(
    s,
    .vars = fct,
    .segmentos = x
  )

  result_original <- svy_tabla_vars_segmentos(
    s,
    .vars = fct,
    .segmentos = x
  )

  # Compare key columns
  expect_equal(result_s3$segmento_cat, result_original$segmento_cat)
  expect_equal(result_s3$pregunta_cat, result_original$pregunta_cat)
  expect_equal(result_s3$prop, result_original$prop)
})

test_that("tabla_vars_segmentos.tbl_svy handles multiple variables", {
  result <- tabla_vars_segmentos(
    s,
    .vars = c(fct, lab),
    .segmentos = x
  )

  # Should include both variables
  expect_equal(
    sort(unique(result$pregunta_var)),
    sort(c("fct", "lab"))
  )
})

test_that("tabla_vars_segmentos.tbl_svy handles missings with prop_val", {
  result <- tabla_vars_segmentos(
    s,
    .vars = lab_na,
    .segmentos = x,
    miss = c(9, NA)
  )

  # Check that prop_val is calculated
  expect_true("prop_val" %in% names(result))
})

test_that("tabla_vars_segmentos.tbl_svy includes confidence intervals", {
  result <- tabla_vars_segmentos(
    s,
    .vars = fct,
    .segmentos = x,
    vartype = c("ci")
  )

  # Check that confidence interval columns are included
  expect_true(all(c("prop_low", "prop_upp") %in% names(result)))
})
