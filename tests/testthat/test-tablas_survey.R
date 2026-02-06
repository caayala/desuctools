d <- data.frame(
  x = factor(c(rep(letters[1:3], 6), letters[1:2]), levels = letters[1:4]),
  fct = factor(c(rep(1, 12), rep(0, 8)), labels = c("No", "Si")),
  lab = haven::labelled(
    c(rep(1, 12), rep(0, 8)),
    labels = c("Si" = 1, "No" = 0),
    label = "Variable labelled"
  ),
  lab_na = haven::labelled(
    c(rep(1, 12), rep(0, 7), NA),
    labels = c("Sí" = 1, "No" = 0, "NS/NR" = 9)
  ),
  chr = c(rep("Si", 12), rep("No", 7), NA_character_),
  num = c(rep(1, 12), rep(0, 8)),
  esc = 1:20,
  esc_na = c(NA_integer_, 2:20),
  wgt = c(rep(0.5, 10), rep(1.5, 10))
)

s <- srvyr::as_survey_design(d, weights = wgt)
# svy_tabla_var_segmento ---------------------------------------------------------

# Resultados sin segmentos

test_that("svy_tabla_var_segmento proporcion de categoría factor", {
  result <- svy_tabla_var_segmento(s, .var = "fct")

  expect_identical(
    result$pregunta_cat,
    factor(c("No", "Si"))
  )
})

test_that("svy_tabla_var_segmento proporcion de categoría labelled", {
  result <- svy_tabla_var_segmento(s, .var = "lab")

  expect_identical(
    result$pregunta_cat,
    factor(c("No", "Si"))
  )
  expect_identical(
    result$pregunta_lab,
    c("Variable labelled", "Variable labelled")
  )
})

test_that("svy_tabla_var_segmento mismos resultados entre factor y labelled", {
  result_fct <- svy_tabla_var_segmento(s, .var = "fct")
  result_lab <- svy_tabla_var_segmento(s, .var = "lab")

  expect_identical(
    result_fct$prop,
    result_lab$prop
  )
})

test_that("svy_tabla_var_segmento media de escalar", {
  result <- svy_tabla_var_segmento(s, .var = "esc")

  expect_s3_class(result, "tbl_df")
  expect_equal(result$mean, weighted.mean(d$esc, d$wgt))
})

test_that("svy_tabla_var_segmento proporcion de categoría labelled", {
  result <- svy_tabla_var_segmento(s, .var = "lab_na")

  expect_s3_class(result, "tbl_df")
  expect_equal(result$prop, c(0.525, 0.4, 0, 0.075))

  result <- svy_tabla_var_segmento(s, .var = "lab_na", miss = c(9, NA))
  expect_equal(result$prop_val, c(10.5 / 18.5, 8 / 18.5, 0, NA))
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
  expect_true(all(as.character(result$segmento_cat) == "Total"))
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

test_that("tabla_vars_segmentos.survey.design2 despacha via metodo S3", {
  s_design <- survey::svydesign(ids = ~1, weights = ~wgt, data = d)

  result <- tabla_vars_segmentos(
    s_design,
    .vars = fct,
    .segmentos = x
  )

  expect_true(all(c("prop", "pregunta_cat", "segmento_cat") %in% names(result)))
  expect_s3_class(result, "tbl_df")
})

test_that("tabla_vars_segmentos.tbl_svy numerica incluye mean y errores", {
  result <- tabla_vars_segmentos(
    s,
    .vars = esc,
    .segmentos = x,
    miss = 20,
    vartype = c("ci", "se")
  )

  expect_equal(unique(result$pregunta_cat), "mean")
  expect_true(all(
    c("mean", "mean_low", "mean_upp", "mean_se", "casos_val") %in% names(result)
  ))
})

test_that("tabla_vars_segmentos.tbl_svy soporta tidyselect en vars y segmentos", {
  result <- tabla_vars_segmentos(
    s,
    .vars = starts_with("f"),
    .segmentos = all_of(c("x"))
  )

  expect_equal(unique(result$pregunta_var), "fct")
  expect_equal(unique(result$segmento_var), "x")
})
