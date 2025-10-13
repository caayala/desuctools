# Test de función edad_rut ----

test_that("edad_rut returns correct age for single RUT", {
  # Rut de ejemplo
  x <- 20117419
  fecha <- as.Date("2024-01-31")

  # Ejecutar la función
  edad_calculada <- edad_rut(.rut = x, fecha_referencia = fecha)

  # Test de clase
  expect_type(edad_calculada, "integer")
  # Test de valor
  expect_equal(edad_calculada, 24L)
})

test_that("edad_rut handles different reference dates", {
  x <- 20117419

  # Diferentes fechas de referencia
  expect_equal(edad_rut(x, as.Date("2023-01-31")), 23L)
  expect_equal(edad_rut(x, as.Date("2025-01-31")), 25L)
  expect_equal(edad_rut(x, as.Date("2000-01-31")), 0L) # Fecha antes del nacimiento estimado
})

test_that("edad_rut works with vector inputs", {
  # Vector de RUTs
  ruts <- c(9000000, 12345678, 20117419, 25000000)
  fecha <- as.Date("2024-01-31")

  edades <- edad_rut(.rut = ruts, fecha_referencia = fecha)

  expect_type(edades, "integer")
  expect_length(edades, 4)
  # Las edades específicas dependen de la fórmula de la función
  # pero verificamos que sean valores plausibles
  expect_true(all(edades >= 0))
  expect_true(all(edades <= 100))

  # Comprobamos algunos valores específicos
  expect_equal(edades[3], 24L) # Ya probado anteriormente
})

test_that("edad_rut handles edge cases", {
  fecha <- as.Date("2024-01-31")

  # RUTs muy pequeños (personas de edad avanzada)
  expect_true(edad_rut(1000000, fecha) > 80)

  # RUTs grandes (personas jóvenes)
  expect_true(edad_rut(26000000, fecha) < 10)
})

test_that("edad_rut works with different reference date vectors", {
  ruts <- c(9000000, 12345678, 20117419)
  fechas <- as.Date(c("2020-01-31", "2022-01-31", "2024-01-31"))

  # Misma longitud de vectores
  edades <- edad_rut(.rut = ruts, fecha_referencia = fechas)
  expect_length(edades, 3)

  # Una fecha para múltiples RUTs
  edades_misma_fecha <- edad_rut(
    .rut = ruts,
    fecha_referencia = as.Date("2024-01-31")
  )
  expect_length(edades_misma_fecha, 3)

  # Comprobamos que las edades de la tercera persona sean consistentes
  # Debe ser 2 años menor en 2022 comparado con 2024
  edad_2024 <- edad_rut(20117419, as.Date("2024-01-31"))
  edad_2022 <- edad_rut(20117419, as.Date("2022-01-31"))
  expect_equal(edad_2024 - edad_2022, 2L)
})

test_that("edad_rut returns NA for NA inputs", {
  fecha <- as.Date("2024-01-31")

  expect_equal(edad_rut(NA_integer_, fecha), NA_integer_)
  expect_equal(edad_rut(c(20117419, NA_integer_), fecha), c(24L, NA_integer_))
})
