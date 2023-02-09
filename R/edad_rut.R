#' Estimación de edad a partir de RUT
#'
#' @param .rut integer. RUT sin dígito verificador.
#'
#' @return integer. Edad estimada de la persona.
#'
#' @importfrom lubridate date_decimal
#' @examples
#' edad_rut(13456789)
#'
#' @export
edad_rut <- function(.rut){
  # Ruts sin dígito verificador.

  # A partir de código de página:
  # https://rutificador-chile.com/wp-content/uploads/2022/06/rut-a-edad.html

  slope <- 3.3363697569700348e-06
  intercept <- 1932.2573852507373

  birth_date <- .rut * slope + intercept

  birth_date_year <- floor(birth_date)
  birth_date_month <- ceiling((birth_date - birth_date_year) * 12)

  age <- difftime(Sys.Date(),
                  lubridate::date_decimal(birth_date))

  age <- age / 365.25
  age <- floor(age)
  age <- as.integer(age)

  return(age)
}
