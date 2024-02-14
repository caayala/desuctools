#' Tablas de resultados ----------------------------------------------------
#'
#' Generación de una data.frame con el número de casos y proporción de las distintas
#' variables de segmentos que se agregen en `...`.
#'
#' @description Produce una tabla de distribución por categorías para una variable.
#'
#' @param .df tibble. datos tidy.
#' @param .var chr. Variable de la que se sabrá su distribución. Solo una variable.
#' @param .wt chr. Variable de ponderación. Solo una variable.
#' @param miss chr vector. Nombre de niveles de categorías de `.var` que deben
#' ser considerados missing.
#'
#' @importFrom forcats as_factor fct_na_value_to_level
#' @importFrom rlang  %||%

tabla_categoria <- function(.df,
                            .var,
                            .wt = NULL,
                            miss = NULL) {

  v_pregunta <- .df[[.var]]

  # Si no hay pesos, se construye un vector de 1.
  if(is.null(.wt)){
    v_wgt    <- rep(1L, length(v_pregunta))
  } else {
    v_wgt    <- .df[[.wt]]
  }

  df_agg <- data.frame(casos        = v_wgt,
                       pregunta_cat = forcats::as_factor(v_pregunta))

  # Declarar explícitamente NA que pudiesen estar en la categoría de respuestas
  # de la pregunta.
  # Solo si hay casos NA en el factor
  if(is.na(df_agg$pregunta_cat) |> any()) {
    df_agg$pregunta_cat <- forcats::fct_na_value_to_level(df_agg$pregunta_cat)
  }

  tab <- aggregate(casos ~ pregunta_cat,
                   df_agg,
                   FUN = \(x) sum(x, na.rm = FALSE),
                   na.action = 'na.pass',
                   drop = FALSE)

  # Agregar 0 si es que hay combinación de pregunta y segmento sin casos.
  tab$casos <- replace(tab$casos, is.na(tab$casos), 0)

  # Variable y etiqueta de pregunta
  tab$pregunta_var <- .var
  tab$pregunta_lab <- attr(.df[[.var]], 'label', exact = TRUE) %||% '-'

  # Agrega el porcentaje de respuesta.
  tab <- tabla_prop(tab,
                    .segmento = 'pregunta_var')

  # Agrega el porcentaje válido si es que se señalan categorias perdidas.
  if (!is.null(miss)) {
    tab <- tabla_prop_val(tab,
                          miss = c(NA, miss))
  }

  # Orden de variables de salida
  col_orden <- c('pregunta_var', 'pregunta_lab', 'pregunta_cat')

  col_adicionales <- setdiff(names(tab), col_orden)

  tab <- tab[c(col_orden, col_adicionales)]

  # Orden de casos
  tab |>
    arrange(pregunta_cat)
}

#' @title Tabla de categorías
#'
#' @description
#' @description
#' Porcentaje de respuesta de categorías de varias variables.
#' Principalmente para mostrar la distribución de casos de variables de segmentación posteriores.
#'
#' @name tabla_categorias
#'
#' @param .df data.frame. Base de datos.
#' @param ... Preguntas de las que se quiere saber su proporcion. Se puede utilizar
#' `tidyselect` para facilitar la selección de varias columnas.
#' @param .wt name. Nombre de columna con ponderador o expansor de los datos. Por defecto es NULL,
#' sin expansor.
#'
#' @return tibble
#'
#' @importFrom rlang quo_is_null enquo
#' @importFrom tidyselect eval_select
#' @importFrom purrr map list_flatten list_rbind
#'
#' @export
tabla_categorias <- function(.df,
                             ...,
                             .wt = NULL,
                             miss = NULL) {

  # Transformar variable en NSE a un chr para poder ser input de función
  # tabla_cateroria2
  wt_quo <- rlang::enquo(.wt)

  if(!rlang::quo_is_null(wt_quo)){
    .wt <- wt_quo |> as_label()
  }

  vars <- tidyselect::eval_select(expr = expr(c(...)),
                                  data = .df)
  vars <- names(vars)

  tab <- map(
    vars,
    \(x) tabla_categoria(.df = .df,
                         .var = x,
                         .wt = .wt,
                         miss = miss)
  )

  tab |>
    purrr::list_flatten() |>
    purrr::list_rbind()
}


#' Cálculo de proporciones
#'
#' @param .df tibble.
#' @param .segmento chr.
#'
#' @return tibble
#'
#' @import dplyr
#'
#' @examples
#' df <- tibble(var = c('a', 'b'),
#'             casos = c(30, 70))
#'
#' tabla_prop(df, .segmento = NULL)
#'
#' tabla_prop(df, .segmento = 'var')
#'
tabla_prop <- function(.df,
                       .segmento) {
  # Cálculo de porcentaje de respuestas en tabla con numero de casos.
  # Proporción de "casos" en cada grupo ".segmento" y luego reemplazar NaN por 0.

  .df$prop <- ave(.df[["casos"]],
                  .df[[.segmento]],
                  FUN = \(x) x / sum(x, na.rm = TRUE))
  .df$prop <- ifelse(is.nan(.df$prop), 0, .df$prop)

  return(.df)
}


#' Calculo de proporción para categorías válidas.
#'
#' @param .df tibble
#' @param by chr vector. variables respecto de la que se calculará el porcentaje.
#' @param miss chr vector. Categorías que se deben considerar como missing o inválidas.
#'
#' @return tibble
#'
#' @examples
#' df <- tibble(pregunta_cat = c('a', 'b', 'x'),
#'              casos = c(30, 30, 40))
#'
#' tabla_prop_val(df, miss = 'x')
#'
#' tabla_prop_val(df, by = 'pregunta_cat', miss = 'x')
#'
tabla_prop_val <- function(.df,
                           by = NULL,
                           miss) {
  # Cálculo de porcetaje de respuestas válidas en tabla con numero de casos.

  # Pasar de quosure con texto a string y luego simbolo.
  .df$casos_val <- replace(.df$casos,
                           .df$pregunta_cat %in% miss,
                           NA)

  df <- .df |>
    mutate(prop_val = .data[['casos_val']] / sum(.data[['casos_val']], na.rm = TRUE),
           .by = all_of(by))

  # Eliminar variable auxiliar.
  df$casos_val <- NULL

  return(df)
}


#' Agrega dato total por segmento.
#'
#' @description Es una ayuda para tabla_vars_segmentos.
#'
#' @param .df tibble
#'
#' @importFrom dplyr bind_rows
#' @importFrom forcats as_factor
#'
#' @return tibble
#'
tabla_total_prop <- function(.df) {

  tab_tot <- aggregate(casos ~ pregunta_cat,
                       data = .df,
                       FUN = \(x) sum(x, na.rm = FALSE),
                       na.action = na.pass,
                       drop = FALSE)

  tab_tot$segmento_var <- unique(.df$segmento_var)
  tab_tot$segmento_lab <- unique(.df$segmento_lab)

  tab_tot$segmento_cat <- 'Total'

  tab_tot$pregunta_var <- unique(.df$pregunta_var)
  tab_tot$pregunta_lab <- unique(.df$pregunta_lab)

  tab <- dplyr::bind_rows(.df,
                          tab_tot)

  tab$segmento_cat <- forcats::as_factor(tab$segmento_cat)

  return(tab)
}


#' Tabla de respuestas de una variable para cada categoría de un segmento.
#'
#' Resuelve el caso particular para tabla_vars_segmentos.
#'
#' @param .df tibble.
#' @param .var chr.
#' @param .segmento chr.
#' @param .wt chr.
#' @param miss chr vector.
#' @param total logic
#'
#' @importFrom forcats as_factor fct_na_value_to_level
#' @importFrom dplyr arrange
#'
#' @return tibble
#'
tabla_var_segmento <- function(.df,
                               .var,
                               .segmento = NULL,
                               .wt = NULL,
                               miss = NULL,
                               total = FALSE) {

  v_pregunta <- .df[[.var]]

  # Si no hay pesos, se construye un vector de 1.
  if(is.null(.wt)){
    v_wgt    <- rep(1L, length(v_pregunta))
  } else {
    v_wgt    <- .df[[.wt]]
  }

  if(is.null(.segmento)){
    # Si no hay segmentos sobre los que dividir la variable de interés.
    v_segmento <- rep(1L, length(v_pregunta))
    segmento_var <- 'Total'
    segmento_lab <- 'Total'
  } else {
    v_segmento   <- .df[[.segmento]]
    segmento_var <- .segmento
    segmento_lab <- attr(.df[[.segmento]], 'label', exact = TRUE) %||% '-'
  }

  df_agg <- data.frame(casos        = v_wgt,
                       segmento_cat = forcats::as_factor(v_segmento))

  if(inherits(v_pregunta, c('haven_labelled', 'factor', 'character'))){
    # Caso en que pregunta es labelled, factor o string: se calcula proporción.
    df_agg$pregunta_cat <- forcats::as_factor(v_pregunta)

    # Declarar explícitamente NA que pudiesen estar en la categoría de respuestas
    # de la pregunta.
    # Solo si hay casos NA en el factor
    if(any(is.na(df_agg$pregunta_cat))) {
      df_agg$pregunta_cat <- forcats::fct_na_value_to_level(df_agg$pregunta_cat)
    }

    tab <- aggregate(casos ~ segmento_cat + pregunta_cat,
                     df_agg,
                     FUN = \(x) sum(x, na.rm = FALSE),
                     na.action = na.pass,
                     drop = FALSE)

    # Agregar 0 si es que hay combinación de pregunta y segmento sin casos.
    tab$casos <- replace(tab$casos, is.na(tab$casos), 0)

    # Agrega el porcentaje total a los segmentos.
    if (total) {
      tab <- tabla_total_prop(tab)
    }

    # Agrega el porcentaje de respuesta.
    tab <- tabla_prop(tab,
                      .segmento = 'segmento_cat')

    # Agrega el porcentaje válido si es que se señalan categorias perdidas.
    if (!is.null(miss)) {
      tab <- tabla_prop_val(tab,
                            miss = c(NA, miss),
                            by = 'segmento_cat')
    }

  } else {
    # Caso en que pregunta sea numérica: se calcula promedio.
    df_agg$pregunta_cat <- v_pregunta

    tab <- aggregate(x = df_agg[['pregunta_cat']],
                     by = list('segmento_cat' = df_agg[['segmento_cat']]),
                     FUN = function(x) c(casos = sum(             df_agg$casos[match(x, df_agg$pregunta_cat)]),
                                         mean  = weighted.mean(x, df_agg$casos[match(x, df_agg$pregunta_cat)])),
                     drop = FALSE)

    tab <- cbind(tab[1],
                 tab$x)

    tab$pregunta_cat <- 'mean'
  }

  # Variable y etiqueta de pregunta
  tab$pregunta_var <- .var
  tab$pregunta_lab <- attr(.df[[.var]], 'label', exact = TRUE) %||% '-'

  # Variable y etiqueta de segmentos
  tab$segmento_var <- segmento_var
  tab$segmento_lab <- segmento_lab


  # Orden de variables de salida
  col_orden <- c('segmento_var', 'segmento_lab', 'segmento_cat',
                 'pregunta_var', 'pregunta_lab', 'pregunta_cat')

  col_adicionales <- setdiff(names(tab), col_orden)

  tab <- tab[c(col_orden, col_adicionales)]

  # Orden de casos
  tab[order(tab$segmento_cat, tab$pregunta_cat), ]
}



#' @title Tabla de porcentajes de variables según segmentos.
#'
#' @description
#' Obtiene porcentajes de respuestas de múltiples variables según múltiples segmentos.
#'
#' @param .df tibble
#' @param .vars tidyselect, lista de nombres de variables de las que se quiere saber su proporción de respuestas
#' @param .segmentos tidyselect, lista de nombres de variables de segmentación de las preguntas de `.vars`
#' @param .wt name, nombre de la variable de ponderación
#' @param miss integers, Vector de valores que deben coniderarse como missings.
#' @param total logical, Si total = TRUE, se agrega el total para cada segmento.
#'
#' @return tibble
#'
#' @importFrom purrr map list_flatten list_rbind
#' @importFrom tidyselect eval_select
#' @importFrom rlang enquo quo_is_null
#'
#' @export
tabla_vars_segmentos <- function(.df,
                                 .vars,
                                 .segmentos = NULL,
                                 .wt = NULL,
                                 miss = NULL,
                                 total = FALSE) {

  vars <- tidyselect::eval_select(expr = enquo(.vars),
                                  data = .df)
  vars <- names(vars)

  # Revisar si se entrega un ponderador
  wt_quo <- rlang::enquo(.wt)

  if(rlang::quo_is_null(wt_quo)){
    wt_chr <- NULL
  } else {
    wt_chr <- rlang::as_label(wt_quo)
  }

  # Revisar que se entreguen segmentos
  segmentos_quo <- rlang::enquo(.segmentos)

  if(rlang::quo_is_null(segmentos_quo)){
    segmentos <- list(NULL)
  } else {
    segmentos <- tidyselect::eval_select(expr = segmentos_quo,
                                         data = .df)
    segmentos <- names(segmentos)
  }

  tab <- purrr::map(
    vars,
    \(x_v) {
      purrr::map(
        segmentos,
        \(y_s) {
          tabla_var_segmento(.df = .df,
                             .var = x_v,
                             .segmento = y_s,
                             .wt = wt_chr,
                             total = total,
                             miss = miss)
        }
      )
    }
  )

  tab |>
    purrr::list_flatten() |>
    purrr::list_rbind() |>
    tibble::as_tibble()
}
