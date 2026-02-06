#' Factor de niveles de ense\u00f1anza
#'
#' @description
#' Recodificaci\u00f3n para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' Niveles de ense\u00f1anza
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x Un vector de c\u00f3digos, t\u00edpicamente variable `COD_ENSE` o `COD_ENS_X`.
#'
#' @returns
#' A factor with 44 levels, with an attached `"label"` attribute.
#'
#' @export
edu_fct_cod_ense <- function(x) {
  cod_ense <- c(
    "No Aplica" = 0,
    "Educaci\u00f3n Parvularia" = 10,
    "Ense\u00f1anza B\u00e1sica" = 110,
    "Educaci\u00f3n B\u00e1sica Com\u00fan Adultos (Decreto 584/2007)" = 160,
    "Educaci\u00f3n B\u00e1sica Especial Adultos" = 161,
    "Escuelas C\u00e1rceles (B\u00e1sica Adultos)" = 163,
    "Educaci\u00f3n B\u00e1sica Adultos Sin Oficios (Decreto 584/2007)" = 165,
    "Educaci\u00f3n B\u00e1sica Adultos Con Oficios (Decreto 584/2007 y 999/2009)" = 167,
    "Educaci\u00f3n Especial Discapacidad Auditiva" = 211,
    "Educaci\u00f3n Especial Discapacidad Intelectual" = 212,
    "Educaci\u00f3n Especial Discapacidad Visual" = 213,
    "Educaci\u00f3n Especial Trastornos Espec\u00edficos del Lenguaje" = 214,
    "Educaci\u00f3n Especial Trastornos Motores" = 215,
    "Educaci\u00f3n Especial Autismo" = 216,
    "Educaci\u00f3n Especial Discapacidad Graves Alteraciones en la Capacidad de Relaci\u00f3n y Comunicaci\u00f3n" = 217,
    "Educaci\u00f3n Especial Discapacidad M\u00faltiple" = 218,
    "Educaci\u00f3n Especial Sordoceguera" = 219,
    "Opci\u00f3n 4 Programa Integraci\u00f3n Escolar" = 299,
    "Ense\u00f1anza Media H-C ni\u00f1os y j\u00f3venes" = 310,
    "Educaci\u00f3n Media H-C adultos vespertino y nocturno (Decreto N\u00b0 190/1975)" = 360,
    "Educaci\u00f3n Media H-C adultos (Decreto N\u00b0 12/1987)" = 361,
    "Escuelas C\u00e1rceles (Media Adultos)" = 362,
    "Educaci\u00f3n Media H-C Adultos (Decreto N\u00b01000/2009)" = 363,
    "Ense\u00f1anza Media T-P Comercial Ni\u00f1os y J\u00f3venes" = 410,
    "Educaci\u00f3n Media T-P Comercial Adultos (Decreto N\u00b0 152/1989)" = 460,
    "Educaci\u00f3n Media T-P Comercial Adultos (Decreto N\u00b0 152/1989)" = 461,
    "Educaci\u00f3n Media T-P Comercial Adultos (Decreto N\u00b0 1000/2009)" = 463,
    "Ense\u00f1anza Media T-P Industrial Ni\u00f1os y J\u00f3venes" = 510,
    "Educaci\u00f3n Media T-P Industrial Adultos (Decreto N\u00b0 152/1989)" = 560,
    "Educaci\u00f3n Media T-P Industrial Adultos (Decreto N\u00b0 152/1989)" = 561,
    "Educaci\u00f3n Media T-P Industrial Adultos (Decreto N\u00b0 1000/2009)" = 563,
    "Ense\u00f1anza Media T-P T\u00e9cnica Ni\u00f1os y J\u00f3venes" = 610,
    "Educaci\u00f3n Media T-P T\u00e9cnica Adultos (Decreto N\u00b0 152/1989)" = 660,
    "Educaci\u00f3n Media T-P T\u00e9cnica Adultos (Decreto N\u00b0 152/1989)" = 661,
    "Educaci\u00f3n Media T-P T\u00e9cnica Adultos (Decreto N\u00b0 1000/2009)" = 663,
    "Ense\u00f1anza Media T-P Agr\u00edcola Ni\u00f1os y J\u00f3venes" = 710,
    "Educaci\u00f3n Media T-P Agr\u00edcola Adultos (Decreto N\u00b0 152/1989)" = 760,
    "Educaci\u00f3n Media T-P Agr\u00edcola Adultos (Decreto N\u00b0 152/1989)" = 761,
    "Educaci\u00f3n Media T-P Agr\u00edcola Adultos (Decreto N\u00b0 1000/2009)" = 763,
    "Ense\u00f1anza Media T-P Mar\u00edtima Ni\u00f1os y J\u00f3venes" = 810,
    "Ense\u00f1anza Media T-P Mar\u00edtima Adultos (Decreto N\u00b0 152/1989)" = 860,
    "Ense\u00f1anza Media T-P Mar\u00edtima Adultos (Decreto N\u00b0 1000/2009)" = 863,
    "Ense\u00f1anza Media Art\u00edstica Ni\u00f1os y J\u00f3venes" = 910,
    "Ense\u00f1anza Media Art\u00edstica Adultos" = 963
  )

  factor(
    x,
    levels = unname(cod_ense),
    labels = names(cod_ense)
  ) |>
    structure(
      label = "Niveles de ense\u00f1anza"
    )
}

#' Factor para distingir Educaci\u00f3n Especial
#'
#' @description
#' Recodificaci\u00f3n para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' Diferencia Educaci\u00f3n Especial en tres niveles.
#' es igual al d\u00edgito `6`.
#'
#' Los cod_educ relacioandos son:
#'
#' - 211 Educaci\u00f3n Especial Discapacidad Auditiva
#' - 212 Educaci\u00f3n Especial Discapacidad Intelectual
#' - 213 Educaci\u00f3n Especial Discapacidad Visual
#' - 214 Educaci\u00f3n Especial Trastornos Espec\u00edficos del Lenguaje
#' - 215 Educaci\u00f3n Especial Trastornos Motores
#' - 216 Educaci\u00f3n Especial Autismo
#' - 217 Educaci\u00f3n Especial Discapacidad Graves Alteraciones en la Capacidad de Relaci\u00f3n y Comunicaci\u00f3n
#' - 218 Educaci\u00f3n Especial Discapacidad M\u00faltiple
#' - 219 Educaci\u00f3n Especial Sordoceguera
#' - 299 Opci\u00f3n 4 Programa Integraci\u00f3n Escolar
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
        'Educaci\u00f3n especial',
        'Educaci\u00f3n especial Trastornos del Lenguaje',
        'No'
      )
    ) |>
    structure(
      label = "Tipo de Educaci\u00f3n Especial"
    )
}


#' Factor de niveles de ense\u00f1anza agrupado
#'
#' @description
#' Recodificaci\u00f3n para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' Niveles de ense\u00f1anza agrupados
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x Un vector de c\u00f3digos, t\u00edpicamente variable `COD_ENSE2`.
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
      'Educaci\u00f3n Parvularia',
      'Ense\u00f1anza B\u00e1sica Ni\u00f1os',
      'Educaci\u00f3n B\u00e1sica Adultos',
      'Educaci\u00f3n Especial',
      'Ense\u00f1anza Media Human\u00edstico-Cient\u00edfica J\u00f3venes',
      'Educaci\u00f3n Media Human\u00edstico-Cient\u00edfica Adultos',
      'Ense\u00f1anza Media T\u00e9cnico Profesional y Art\u00edstica, J\u00f3venes',
      'Educaci\u00f3n Media T\u00e9cnico Profesional y Art\u00edstica, Adultos'
    )
  ) |>
    structure(
      label = "Niveles de ense\u00f1anza agrupados"
    )
}


#' Factor de niveles de ense\u00f1anza agrupado
#'
#' @description
#' Recodificaci\u00f3n para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' Niveles de ense\u00f1anza con educaci\u00f3n especial reasignado.
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x Un vector de c\u00f3digos, t\u00edpicamente variable `COD_ENSE3`.
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
      "Educaci\u00f3n Parvularia",
      "Ense\u00f1anza B\u00e1sica Ni\u00f1os",
      "Educaci\u00f3n B\u00e1sica Adultos",
      "Ense\u00f1anza Media Human\u00edstico-Cient\u00edfica J\u00f3venes",
      "Educaci\u00f3n Media Human\u00edstico-Cient\u00edfica Adultos",
      "Ense\u00f1anza Media T\u00e9cnico Profesional y Art\u00edstica, J\u00f3venes",
      "Educaci\u00f3n Media T\u00e9cnico Profesional y Art\u00edstica, Adultos"
    )
  ) |>
    structure(
      label = "Niveles de ense\u00f1anza con educaci\u00f3n especial reasignado"
    )
}

#' Factor de c\u00f3digo de dependencia del establecimiento agrupado
#'
#' @description
#' Recodificaci\u00f3n para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' C\u00f3digo de Dependencia del Establecimiento (agrupado).
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x Un vector de c\u00f3digos, t\u00edpicamente variable `COD_DEPE2`.
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
      label = "C\u00f3digo de Dependencia del Establecimiento"
    )
}


#' Factor de c\u00f3digo de ense\u00f1anza en 13 niveles
#'
#' @description
#' Recodificaci\u00f3n para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' Recodificaci\u00f3n del C\u00f3digo de Ense\u00f1anza en 13 niveles.
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x Un vector de c\u00f3digos, t\u00edpicamente variable `COD_DEPE2`.
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
      "Educaci\u00f3n Parvularia Regular",
      "Educaci\u00f3n Parvularia Especial",
      "Ense\u00f1anza B\u00e1sica Ni\u00f1os Regular",
      "Ense\u00f1anza B\u00e1sica Ni\u00f1os Especial",
      "Ense\u00f1anza Media HC J\u00f3venes ciclo general",
      "Ense\u00f1anza Media TP J\u00f3venes ciclo general",
      "Ense\u00f1anza Media HC J\u00f3venes ciclo diferenciado",
      "Ense\u00f1anza Media TP J\u00f3venes ciclo diferenciado",
      "Educaci\u00f3n B\u00e1sica Adultos",
      "Educaci\u00f3n Media HC Adultos ciclo general",
      "Educaci\u00f3n Media TP Adultos ciclo general",
      "Educaci\u00f3n Media HC Adultos ciclo diferenciado",
      "Educaci\u00f3n Media TP Adultos ciclo diferenciado"
    )
  ) |>
    structure(
      label = "Recodificaci\u00f3n del C\u00f3digo de Ense\u00f1anza en 13 niveles"
    )
}


#' Factor de ruralidad
#'
#' @description
#' Recodificaci\u00f3n para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' \u00cdndice de ruralidad del establecimiento.
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x Un vector de c\u00f3digos, t\u00edpicamente variable `RURAL_RBD`.
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
      label = "\u00cdndice de ruralidad del establecimiento"
    )
}

#' Detecta c\u00f3digos de ense\u00f1anza para adultos
#'
#' @description
#' Recodificaci\u00f3n para bases de datos disponibles en el Centro de Estudios MINEDUC, Chile
#' Detecta niveles de ense\u00f1anza para adultos ya que su segunda cifra significativa
#' es igual al d\u00edgito `6`.
#'
#' @seealso [https://centroestudios.mineduc.cl/datos-abiertos/]
#'
#' @param x Un vector num\u00e9rico o de cadenas.
#'
#' @returns
#' Un vector l\u00f3gico indicando si es un c\u00f3digo de ense\u00f1anza para adultos.
#'
#' @export
edu_es_educacion_adulto <- function(x) {
  x <- unlist(x)

  # Convertimos a caracteres de un largo determinado.
  x_str <- sprintf("%04d", x) # Asegura tener al menos 4 d\u00edgitos para todos

  # Vector l\u00f3gico detectando los codigos de ense\u00f1anza para adultos.
  grepl(".*6.$", x_str)
}
