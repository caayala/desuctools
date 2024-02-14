# Funciones para análisis de datos de encuestas

#' @title Tabla con intervalos de confianza
#'
#' @description
#' Devuelve tabla de frecuencias con intervalos de confianza para un nivel
#' `level`de significancia entre las categorías de respuesta de la
#' variable `.var`.
#'
#' @name svy_tabla_var_segmento
#'
#' @param .data data frame con diseño complejo
#' @param .var Variable en la que interesa comparar categorías de respuesta.
#' @param .segmento Segmentos de interés para ver diferencias en categorías
#'        de variable `.var`. Por defecto NULL
#' @param na.rm boolean. Considera o no missings, por defecto FALSE.
#' @param level double. Nivel de significancia para intervalos de confianza
#'
#' @importFrom rlang %||% .data
#' @importFrom srvyr survey_mean
#'
#' @return tibble
#'
svy_tabla_var_segmento <- function(.df,
                                   .var,
                                   .segmento = NULL,
                                   miss = NULL,
                                   vartype = c('ci', 'se'),
                                   level = 0.95){

  if(is.null(.segmento)){
    .df[['variables']][['total']] <- forcats::as_factor('Total')
    .segmento <- 'total'
  } else {
    .df[['variables']][[.segmento]] <- forcats::as_factor(.df[['variables']][[.segmento]])
  }

  if(inherits(.df[['variables']][[.var]], "haven_labelled")){
    .df[['variables']][[.var]] <- forcats::as_factor(.df[['variables']][[.var]])
  }

  # Declarar explícitamente NA que pudiesen estar en la categoría de respuestas
  # de la pregunta.
  # Solo si hay casos NA en el factor
  if(is.na(.df[['variables']][[.var]]) |> any()) {
    .df[['variables']][[.var]] <- forcats::fct_na_value_to_level(.df[['variables']][[.var]])
  }

  # Agrupar la tabla para calculo de indicador por grupo.
  df_group <- .df |>
    dplyr::group_by(pick(any_of(c(.segmento, .var))),
                    # Mantiene niveles de factor (.segmento y .var) que sin casos en los datos.
                    .drop = FALSE)

  f_group_prop <- function(.df_g,
                           vartype = NULL){
    .df_g |>
      dplyr::summarise(casos_unwt = srvyr::unweighted(n()),
                       casos = srvyr::survey_total(),
                       prop = srvyr::survey_prop(vartype = vartype,
                                                 level = level,
                                                 proportion = TRUE,
      # This appears to be the method used by SUDAAN and SPSS COMPLEX SAMPLES
                                                 prop_method = 'xlogit'),
                       .groups = 'drop')
  }

  # Agrega el porcentaje válido si es que se señalan categorías perdidas.
  if (!is.null(miss)) {
    # Caso de proporciones con necesidad de cálculo de prop_val.

    # Categorías de respuesta en miss a NA.
    val_is_miss <- is.na(.df[['variables']][[.var]]) |
      .df[['variables']][[.var]] %in% c(miss, NA)
    val_is_miss <- c(miss, NA)

    tab <- df_group |>
      f_group_prop(vartype = NULL)

    tab_miss <- df_group |>
      dplyr::slice(!val_is_miss)
      dplyr::filter(!val_is_miss, .preserve = FALSE) |>
      dplyr::summarise(prop_val = srvyr::survey_prop(vartype = vartype,
                                                     level = level,
                                                     proportion = TRUE,
                                                     prop_method = 'xlogit'),
                       .groups = 'drop')

    tab <- dplyr::left_join(tab,
                            tab_miss,
                            by = c(.segmento, .var))

  } else {
    # Caso de proporciones sin necesidad de cálculo de prop_val.
    tab <- df_group |>
      f_group_prop(vartype = vartype)
  }

  # Variable y etiqueta de pregunta
  tab$pregunta_var <- .var
  tab$pregunta_lab <- attr(.df[['variables']][[.var]],
                           'label',
                           exact = TRUE) %||% '-'
  names(tab)[names(tab) == .var] <- "pregunta_cat"

  # Variable y etiqueta de segmentos
  tab$segmento_var <- .segmento
  tab$segmento_lab <- attr(.df[['variables']][[.segmento]],
                           'label',
                           exact = TRUE) %||% '-'
  names(tab)[names(tab) == .segmento] <- "segmento_cat"

  # Determinar si hay diferencias significativas
  tab <- tab |>
    # Aplicar la función para cada grupo.
    tidyr::nest(.by = ('segmento_cat')) |>
    dplyr::mutate(data = purrr::map(data, svy_diff_sig)) |>
    tidyr::unnest(data)

  # Orden de variables de salida
  col_orden <- c('segmento_var', 'segmento_lab', 'segmento_cat',
                 'pregunta_var', 'pregunta_lab', 'pregunta_cat')

  col_adicionales <- setdiff(names(tab), col_orden)

  tab <- tab[c(col_orden, col_adicionales)]

  # Orden de casos
  tab[order(tab$segmento_cat, tab$pregunta_cat), ]
}


#' @title Tabla con intervalos de confianza
#'
#' @description
#' Devuelve tabla de frecuencias con intervalos de confianza para un nivel
#' `level`de significancia entre las categorías de respuesta de la
#' variable `.vars`.
#'
#' A diferencia de svy_tabla_var_segmentos, esta funcion puede procesar varias variables
#' y segmentos a la vez.
#'
#' @name svy_tabla_vars_segmentos
#'
#' @param .data `tbl_svy` data.frame con diseño de encuesta.
#' @param .vars c(). Variables de interés respecto.
#' @param .segmentos c(). Lista de variables por las que se quiere segmentar `.vars`.
#' @param ... atributos que se pasan a funcion `svy_tabla_var_segmento`.
#'
#' @importFrom rlang !! enquo sym
#' @importFrom tidyr unnest expand_grid
#' @importFrom tidyselect eval_select
#' @importFrom purrr map2
#'
#' @return tibble
#'
#' @export
svy_tabla_vars_segmentos <- function(.df,
                                     .vars,
                                     .segmentos = NULL,
                                     miss = NULL,
                                     vartype = c('ci', 'se'),
                                     level = 0.95) {

  if(!inherits(s, 'tbl_svy')) {
    stop('Se necesita un data.frame con diseno complejo')
  }

  vars <- tidyselect::eval_select(expr = enquo(.vars),
                                  data = .df[['variables']])
  vars <- names(vars)

  # Revisar que se entreguen segmentos
  segmentos_quo <- enquo(.segmentos)

  if(rlang::quo_is_null(segmentos_quo)){
    segmentos <- list(NULL)
  } else {
    segmentos <- tidyselect::eval_select(expr = segmentos_quo,
                                         data = .df[['variables']])
    segmentos <- names(segmentos)
  }

  tab <- purrr::map(
    vars,
    \(x_v) {
      purrr::map(
        segmentos,
        \(y_s) {
          svy_tabla_var_segmento(.df = .df,
                                 .var = x_v,
                                 .segmento = y_s,
                                 miss = miss,
                                 vartype = vartype,
                                 level = level)
        }
      )
    }
  )

  tab |>
    purrr::list_flatten() |>
    purrr::list_rbind() |>
    tibble::as_tibble()
}


#' @title Comparación entre intervalos de confianza
#'
#' @description
#' Determina diferencias significativas según intervalos de confianza calculados desde
#' `srvyr`.
#'
#' @name svy_diff_sig
#'
#' @param .df data.frame con variables `\\*_upp` y `\\*_low`
#'
#' @importFrom stringr str_detect str_which
#'
#' @return data.frame
#'
#' @export
svy_diff_sig <- function(.df){

  var_name <- colnames(.df)

  if(sum(stringr::str_detect(var_name, "_upp$|_low$")) != 2) {
    stop("Se necesita el intervalo de confianza con variables '_upp' y '_low'")
  }

  var_low <- var_name[stringr::str_which(var_name, '_low$')]
  var_upp <- var_name[stringr::str_which(var_name, '_upp$')]

  # 1 si no hay columna "casos".
  val_count <- .df[['casos']] %||% 1

  # Eliminar del cálculo de diferencias niveles en los que no hayan respuestas.
  val_low   <- ifelse(val_count == 0, NA, .df[[var_low]])
  val_upp   <- ifelse(val_count == 0, NA, .df[[var_upp]])

  suppressWarnings(
    # Aparece warnings por grupos en donde no hay valores válidos.
    .df$diff_sig <-
      min(val_upp, na.rm = TRUE) < val_low |
      max(val_low, na.rm = TRUE) > val_upp
  )

  .df
}
