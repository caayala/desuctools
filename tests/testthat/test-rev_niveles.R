var_labelled <- structure(
  c(1, 2, 2, 3, 9),
  labels = c("a" = 1, "b" = 2, "c" = 3, "nr" = 9),
  label = "this is labelled",
  class = "haven_labelled"
)

test_that("rev_niveles haven_labelled", {
  result_sin_rango <- rev_niveles(var_labelled)
  result_con_rango <- rev_niveles(var_labelled, rango_inv = c(1, 2))

  result_con_niveles <- rev_niveles(var_labelled, niveles_inv = c(1, 2))

  # Valores sin rango
  expect_equal(
    as.numeric(result_sin_rango),
    c(9, 8, 8, 7, 1)
  )
  # Etiquetas sin rango
  expect_equal(
    attr(result_sin_rango),
    c("a" = 9, "b" = 8, "c" = 7, "nr" = 1)
  )

  # Valores con rango
  expect_equal(
    as.numeric(result_con_rango),
    c(2, 1, 1, 3, 9)
  )
  # Etiquetas con rango
  expect_equal(
    attr(result_con_rango, "labels"),
    c("a" = 2, "b" = 1, "c" = 3, "nr" = 9)
  )

  # Igualdad entre rango == NULL y todo el rango
  expect_equal(
    rev_niveles(var_labelled, rango_inv = NULL),
    rev_niveles(var_labelled, rango_inv = c(1, 9))
  )
})

test_that("rev_niveles factor", {
  var_factor <- factor(c("a", "b", "b", "c", "nr")) |>
    `attr<-`("label", "factor variable")

  result_sin_niveles <- rev_niveles(var_factor)
  result_con_niveles <- rev_niveles(var_factor, niveles_inv = c("a", "b"))

  expect_equal(
    as.character(result_con_niveles),
    c("a", "b", "b", "c", "nr"),
    ignore_attr = TRUE
  )
  expect_equal(levels(result_con_niveles), c("b", "a", "c", "nr"))

  expect_equal(
    as.character(result_sin_niveles),
    c("a", "b", "b", "c", "nr"),
    ignore_attr = TRUE
  )
  expect_equal(levels(result_sin_niveles), c("nr", "c", "b", "a"))

  # Igualdad entre rango == NULL y todo el rango
  expect_equal(
    rev_niveles(var_factor, niveles_inv = NULL),
    rev_niveles(var_factor, niveles_inv = levels(var_factor))
  )
})

test_that("rev_niveles numeric", {
  var_numeric <- c(1, 2, 2, 9) |>
    `attr<-`("label", "numeric variable")

  result_sin_rango <- rev_niveles(var_numeric)
  result_con_rango <- rev_niveles(var_numeric, rango_inv = c(1, 2))

  expect_equal(result_sin_rango, c(9, 8, 8, 1), ignore_attr = TRUE)
  expect_equal(attr(result_sin_rango, "label"), "numeric variable")

  expect_equal(result_con_rango, c(2, 1, 1, 9), ignore_attr = TRUE)
  expect_equal(attr(result_con_rango, "label"), "numeric variable")

  # Igualdad entre rango == NULL y todo el rango
  expect_equal(
    rev_niveles(var_numeric, rango_inv = NULL),
    rev_niveles(var_numeric, rango_inv = c(1, 9))
  )
})
