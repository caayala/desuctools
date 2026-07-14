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

  # NS/NR (código 9) queda excluido de prop_val por miss = 9, aunque el
  # factor muestre su etiqueta y no el código.
  result <- svy_tabla_var_segmento(s, .var = "lab_na", miss = c(9, NA))
  expect_equal(result$prop_val, c(10.5 / 18.5, 8 / 18.5, NA, NA))
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

# Robustez y regresiones -----------------------------------------------------

test_that("svy_tabla_var_segmento entrega error claro si variable no existe", {
  expect_error(
    svy_tabla_var_segmento(s, .var = "no_existe"),
    "no existe en los datos"
  )
  expect_error(
    svy_tabla_var_segmento(s, .var = "fct", .segmento = "no_existe"),
    "no existe en los datos"
  )
})

test_that("svy_tabla_vars_segmentos permite que .vars y .segmentos coincidan", {
  # Con prop = 1 el método xlogit para el CI genera warnings esperables.
  result <- suppressWarnings(
    svy_tabla_vars_segmentos(s, .vars = x, .segmentos = x)
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("segmento_cat", "pregunta_cat") %in% names(result)))
  # La diagonal concentra toda la proporción del segmento.
  diag_prop <- result$prop[
    as.character(result$segmento_cat) == as.character(result$pregunta_cat) &
      result$casos > 0
  ]
  expect_equal(diag_prop, rep(1, length(diag_prop)))
})

test_that("una columna llamada 'total' no interfiere con el total interno", {
  d_tot <- d
  d_tot$total <- d_tot$esc
  s_tot <- srvyr::as_survey_design(d_tot, weights = wgt)

  result <- svy_tabla_var_segmento(s_tot, .var = "total")

  expect_identical(unique(as.character(result$segmento_cat)), "Total")
  expect_equal(result$mean, weighted.mean(d_tot$total, d_tot$wgt))
})

test_that("svy_tabla_vars_segmentos acepta survey.design2", {
  s_design <- survey::svydesign(ids = ~1, weights = ~wgt, data = d)

  result <- svy_tabla_vars_segmentos(s_design, .vars = fct)

  expect_s3_class(result, "tbl_df")
  expect_true("prop" %in% names(result))
})

test_that("svy_tabla_vars_segmentos rechaza data.frame sin diseño", {
  expect_error(
    svy_tabla_vars_segmentos(d, .vars = fct),
    "diseno complejo"
  )
})

test_that("prop coincide con survey::svymean", {
  result <- svy_tabla_var_segmento(s, .var = "fct")

  esperado <- unname(coef(survey::svymean(~fct, s)))

  expect_equal(result$prop, esperado)
})

test_that("casos_val con miss es la suma de pesos de casos válidos", {
  result <- svy_tabla_var_segmento(s, .var = "lab_na", miss = c(9, NA))

  # 19 casos válidos: 10 con peso 0.5 y 9 con peso 1.5 menos el NA (peso 1.5)
  expect_equal(unique(result$casos_val), sum(d$wgt[!is.na(d$lab_na)]))
})

test_that("prop_val es NA si el segmento no tiene casos válidos", {
  d_na <- d
  d_na$lab_na_x <- haven::labelled(
    ifelse(d_na$x == "a", 9, unclass(d_na$lab_na)),
    labels = c("Sí" = 1, "No" = 0, "NS/NR" = 9)
  )
  s_na <- srvyr::as_survey_design(d_na, weights = wgt)

  # Con prop = 1 el método xlogit para el CI genera warnings esperables.
  result <- suppressWarnings(
    svy_tabla_var_segmento(
      s_na,
      .var = "lab_na_x",
      .segmento = "x",
      miss = c(9, NA)
    )
  )

  seg_a <- result[as.character(result$segmento_cat) == "a", ]
  expect_true(all(is.na(seg_a$prop_val)))
})

test_that("diff_sig se calcula cuando vartype incluye ci", {
  result <- svy_tabla_var_segmento(s, .var = "fct", .segmento = "x")

  expect_true("diff_sig" %in% names(result))

  result_se <- svy_tabla_var_segmento(s, .var = "fct", vartype = "se")
  expect_false("diff_sig" %in% names(result_se))
})
