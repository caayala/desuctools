#' @title edad_rut
#'
#' @description
#' Función para calcular una edad estimada según el rut de la persona.
#' Es solo una aproximación. Puede ser inexacta para personas migrantes.
#'
#' @importFrom lubridate date_decimal year interval
#'
#' @param .rut `ìnt`: Vector numérico con el rut (sin dígito verificador).
#' @param fecha_referencia `date`: Vector que contiene la fecha de referencia que determina la edad.
#'  Pueden ser una fecha pasada, actual o futura.
#'
#' @source
#' https://rutificador-chile.com/wp-content/uploads/2022/06/rut-a-edad.html
#'
#' @return
#' integer
#'
#' @examples
#'
#' # Importante: el rut no debe contar con el dígito verificador
#' x <- 20117419
#' fecha <- as.Date("2024-01-31")
#'
#' edad_rut(.rut = x,
#'          fecha_referencia = fecha)
#'
#' @export
edad_rut <- function(.rut, fecha_referencia) {
  # Early return for NA inputs
  if (all(is.na(.rut))) {
    return(rep(NA_integer_, length(.rut)))
  }

  # Coeficientes de la fórmula de conversión
  slope <- 3.3363697569700348e-06
  intercept <- 1932.2573852507373

  # Vectorized calculation
  birth_decimal <- .rut * slope + intercept

  # Crear fechas de nacimiento estimadas
  birth_dates <- lubridate::date_decimal(birth_decimal) |>
    as.Date()

  # Prepare reference dates vector - recycle if needed
  if (length(fecha_referencia) == 1) {
    ref_dates <- rep(fecha_referencia, length(.rut))
  } else if (length(fecha_referencia) == length(.rut)) {
    ref_dates <- fecha_referencia
  } else {
    # Recycle with warning if lengths don't match
    warning(
      "Length of fecha_referencia doesn't match length of .rut. Recycling fecha_referencia."
    )
    ref_dates <- rep_len(fecha_referencia, length(.rut))
  }

  # Define helper function for calculating age
  calc_age <- function(birth_date, ref_date, rut_value) {
    if (is.na(rut_value)) {
      return(NA_integer_)
    }

    if (birth_date > ref_date) {
      return(0L)
    } else {
      year_diff <- as.numeric(ref_date - birth_date) / 365.25
      return(floor(year_diff))
    }
  }

  # Apply the function to each pair of birth_date and ref_date
  ages <- mapply(calc_age, birth_dates, ref_dates, .rut)

  # Ensure integer output
  return(as.integer(ages))
}
