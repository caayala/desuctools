#' Factor de niveles de enseñanza
#'
#' @description
#' Recodificación para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' Niveles de enseñanza
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x Un vector de códigos, típicamente variable `COD_ENSE` o `COD_ENS_X`.
#'
#' @returns
#' A factor with 44 levels, with an attached `"label"` attribute.
#'
#' @export
edu_fct_cod_ense <- function(x) {
  cod_ense <- c(
    "No Aplica" = 0,
    "Educación Parvularia" = 10,
    "Enseñanza Básica" = 110,
    "Educación Básica Común Adultos (Decreto 584/2007)" = 160,
    "Educación Básica Especial Adultos" = 161,
    "Escuelas Cárceles (Básica Adultos)" = 163,
    "Educación Básica Adultos Sin Oficios (Decreto 584/2007)" = 165,
    "Educación Básica Adultos Con Oficios (Decreto 584/2007 y 999/2009)" = 167,
    "Educación Especial Discapacidad Auditiva" = 211,
    "Educación Especial Discapacidad Intelectual" = 212,
    "Educación Especial Discapacidad Visual" = 213,
    "Educación Especial Trastornos Específicos del Lenguaje" = 214,
    "Educación Especial Trastornos Motores" = 215,
    "Educación Especial Autismo" = 216,
    "Educación Especial Discapacidad Graves Alteraciones en la Capacidad de Relación y Comunicación" = 217,
    "Educación Especial Discapacidad Múltiple" = 218,
    "Educación Especial Sordoceguera" = 219,
    "Opción 4 Programa Integración Escolar" = 299,
    "Enseñanza Media H-C niños y jóvenes" = 310,
    "Educación Media H-C adultos vespertino y nocturno (Decreto N° 190/1975)" = 360,
    "Educación Media H-C adultos (Decreto N° 12/1987)" = 361,
    "Escuelas Cárceles (Media Adultos)" = 362,
    "Educación Media H-C Adultos (Decreto N°1000/2009)" = 363,
    "Enseñanza Media T-P Comercial Niños y Jóvenes" = 410,
    "Educación Media T-P Comercial Adultos (Decreto N° 152/1989)" = 460,
    "Educación Media T-P Comercial Adultos (Decreto N° 152/1989)" = 461,
    "Educación Media T-P Comercial Adultos (Decreto N° 1000/2009)" = 463,
    "Enseñanza Media T-P Industrial Niños y Jóvenes" = 510,
    "Educación Media T-P Industrial Adultos (Decreto N° 152/1989)" = 560,
    "Educación Media T-P Industrial Adultos (Decreto N° 152/1989)" = 561,
    "Educación Media T-P Industrial Adultos (Decreto N° 1000/2009)" = 563,
    "Enseñanza Media T-P Técnica Niños y Jóvenes" = 610,
    "Educación Media T-P Técnica Adultos (Decreto N° 152/1989)" = 660,
    "Educación Media T-P Técnica Adultos (Decreto N° 152/1989)" = 661,
    "Educación Media T-P Técnica Adultos (Decreto N° 1000/2009)" = 663,
    "Enseñanza Media T-P Agrícola Niños y Jóvenes" = 710,
    "Educación Media T-P Agrícola Adultos (Decreto N° 152/1989)" = 760,
    "Educación Media T-P Agrícola Adultos (Decreto N° 152/1989)" = 761,
    "Educación Media T-P Agrícola Adultos (Decreto N° 1000/2009)" = 763,
    "Enseñanza Media T-P Marítima Niños y Jóvenes" = 810,
    "Enseñanza Media T-P Marítima Adultos (Decreto N° 152/1989)" = 860,
    "Enseñanza Media T-P Marítima Adultos (Decreto N° 1000/2009)" = 863,
    "Enseñanza Media Artística Niños y Jóvenes" = 910,
    "Enseñanza Media Artística Adultos" = 963
  )

  factor(
    x,
    levels = unname(cod_ense),
    labels = names(cod_ense)
  ) |>
    structure(
      label = "Niveles de enseñanza"
    )
}

#' Factor para distingir Educación Especial
#'
#' @description
#' Recodificación para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' Diferencia Educación Especial en tres niveles.
#' es igual al dígito `6`.
#'
#' Los cod_educ relacioandos son:
#'
#' - 211 Educación Especial Discapacidad Auditiva
#' - 212 Educación Especial Discapacidad Intelectual
#' - 213 Educación Especial Discapacidad Visual
#' - 214 Educación Especial Trastornos Específicos del Lenguaje
#' - 215 Educación Especial Trastornos Motores
#' - 216 Educación Especial Autismo
#' - 217 Educación Especial Discapacidad Graves Alteraciones en la Capacidad de Relación y Comunicación
#' - 218 Educación Especial Discapacidad Múltiple
#' - 219 Educación Especial Sordoceguera
#' - 299 Opción 4 Programa Integración Escolar
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x A numeric vector.
#'
#' @returns
#' A factor with 3 levels, with an attached `"label"` attribute.
#'
#' @examples
#' edu_fct_educacion_especial(c(211, 211, 216, 214, 110)) |>
#' table()
#'
#' @export
edu_fct_educacion_especial <- function(x) {
  dplyr::case_match(
    x,
    214 ~ 2L,
    211:299 ~ 1L,
    .default = 3L
  ) |>
    factor(
      levels = 1:3,
      labels = c(
        'Educación especial',
        'Educación especial Trastornos del Lenguaje',
        'No'
      )
    ) |>
    structure(
      label = "Tipo de Educación Especial"
    )
}


#' Factor de niveles de enseñanza agrupado
#'
#' @description
#' Recodificación para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' Niveles de enseñanza agrupados
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x Un vector de códigos, típicamente variable `COD_ENSE2`.
#'
#' @returns
#' A factor with 8 levels, with an attached `"label"` attribute.
#'
#' @export
edu_fct_cod_ense2 <- function(x) {
  factor(
    x,
    levels = 1:8,
    labels = c(
      'Educación Parvularia',
      'Enseñanza Básica Niños',
      'Educación Básica Adultos',
      'Educación Especial',
      'Enseñanza Media Humanístico-Científica Jóvenes',
      'Educación Media Humanístico-Científica Adultos',
      'Enseñanza Media Técnico Profesional y Artística, Jóvenes',
      'Educación Media Técnico Profesional y Artística, Adultos'
    )
  ) |>
    structure(
      label = "Niveles de enseñanza agrupados"
    )
}


#' Factor de niveles de enseñanza agrupado
#'
#' @description
#' Recodificación para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' Niveles de enseñanza con educación especial reasignado.
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x Un vector de códigos, típicamente variable `COD_ENSE3`.
#'
#' @returns
#' A factor with 7 levels, with an attached `"label"` attribute.
#'
#' @export
edu_fct_cod_ense3 <- function(x) {
  factor(
    x,
    levels = 1:7,
    labels = c(
      "Educación Parvularia",
      "Enseñanza Básica Niños",
      "Educación Básica Adultos",
      "Enseñanza Media Humanístico-Científica Jóvenes",
      "Educación Media Humanístico-Científica Adultos",
      "Enseñanza Media Técnico Profesional y Artística, Jóvenes",
      "Educación Media Técnico Profesional y Artística, Adultos"
    )
  ) |>
    structure(
      label = "Niveles de enseñanza con educación especial reasignado"
    )
}

#' Factor de código de dependencia del establecimiento agrupado
#'
#' @description
#' Recodificación para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' Código de Dependencia del Establecimiento (agrupado).
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x Un vector de códigos, típicamente variable `COD_DEPE2`.
#'
#' @returns
#' A factor with 5 levels, with an attached `"label"` attribute.
#'
#' @export
edu_fct_cod_depe2 <- function(x) {
  factor(
    x,
    levels = 1:5,
    labels = c(
      "Municipal",
      "Subvencionado",
      "Part. Pagado",
      "Adm. Delegada",
      "SLE"
    )
  ) |>
    structure(
      label = "Código de Dependencia del Establecimiento"
    )
}


#' Factor de código de enseñanza en 13 niveles
#'
#' @description
#' Recodificación para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' Recodificación del Código de Enseñanza en 13 niveles.
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x Un vector de códigos, típicamente variable `COD_DEPE2`.
#'
#' @returns
#' A factor with 13 levels, with an attached `"label"` attribute.
#'
#' @export
edu_fct_ens <- function(x) {
  factor(
    x,
    levels = 1:13,
    labels = c(
      "Educación Parvularia Regular",
      "Educación Parvularia Especial",
      "Enseñanza Básica Niños Regular",
      "Enseñanza Básica Niños Especial",
      "Enseñanza Media HC Jóvenes ciclo general",
      "Enseñanza Media TP Jóvenes ciclo general",
      "Enseñanza Media HC Jóvenes ciclo diferenciado",
      "Enseñanza Media TP Jóvenes ciclo diferenciado",
      "Educación Básica Adultos",
      "Educación Media HC Adultos ciclo general",
      "Educación Media TP Adultos ciclo general",
      "Educación Media HC Adultos ciclo diferenciado",
      "Educación Media TP Adultos ciclo diferenciado"
    )
  ) |>
    structure(
      label = "Recodificación del Código de Enseñanza en 13 niveles"
    )
}


#' Factor de ruralidad
#'
#' @description
#' Recodificación para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' Índice de ruralidad del establecimiento.
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x Un vector de códigos, típicamente variable `RURAL_RBD`.
#'
#' @returns
#' A factor with 2 levels, with an attached `"label"` attribute.
#'
#' @export
edu_fct_rural_rbd <- function(x) {
  factor(
    x,
    levels = 0:1,
    labels = c(
      "Urbano",
      "Rural"
    )
  ) |>
    structure(
      label = "Índice de ruralidad del establecimiento"
    )
}

#' Detecta códigos de enseñanza para adultos
#'
#' @description
#' Recodificación para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' Detecta niveles de enseñanza para adultos ya que su segunda cifra significativa
#' es igual al dígito `6`.
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x Un vector numérico o de cadenas.
#'
#' @returns
#' Un vector lógico indicando si es un código de enseñanza para adultos.
#'
#' @export
edu_es_educacion_adulto <- function(x) {
  x <- unlist(x)

  # Convertimos a caracteres de un largo determinado.
  x_str <- sprintf("%04d", x) # Asegura tener al menos 4 dígitos para todos

  # Vector lógico detectando los codigos de enseñanza para adultos.
  grepl(".*6.$", x_str)
}
