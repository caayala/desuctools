#' Tablas de resultados ----------------------------------------------------
#'
#' Generación de una data.frame con el número de casos y proporción de las distintas
#' variables de segmentos que se agregen en `...`.
#'
#' @title Tabla de categorías
#'
#' Porcentaje de respuesta de categorías de varias variables.
#' Principalmente para mostrar la distribución de casos de variables de segmetnación posteriores.
#'
#' @name tabla_categorias
#'
#' @param .data data frame. Base de datos.
#' @param ... Preguntas de las que se quiere saber su proporcion. Se puede utilizar
#' `tidyselect` para facilitar la selección de varias columnas.
#' @param .wt Ponderador o expansor de los datos. Por defecto es NULL.
#'
#' @return tibble
#'
#' @import dplyr
#' @importFrom rlang %||% .data enquo
#' @importFrom forcats as_factor fct_na_value_to_level
#' @importFrom sjmisc to_label
#' @importFrom tidyselect vars_select
#' @importFrom purrr map_chr
#' @importFrom tidyr pivot_longer
#'
#' @export
tabla_categorias <- function(.data,
                             ...,
                             .wt = NULL) {
  # Tabla con número de casos y proporción de respuestas por distintas categorías.
  wt_quo <- enquo(.wt)

  preguntas <- tidyselect::vars_select(names(.data), ...)

  # Vector de etiqueta de variables.
  seg_labels <- map_chr(preguntas, ~attr(.data[[.]], 'label') %||% '')
  names(seg_labels) <- preguntas

  tabla <- .data %>%
    mutate(across(c(any_of(preguntas)),
                  \(x) forcats::as_factor(x))) %>%
    summarise(n = sum(!!wt_quo %||% n()),
              .by = any_of(unname(preguntas))) %>%
    tidyr::pivot_longer(cols = -n,
                        names_to = 'pregunta_var',
                        values_to = 'pregunta_cat') %>%
    mutate(pregunta_var = forcats::as_factor(.data[["pregunta_var"]]),
           pregunta_cat = forcats::as_factor(.data[["pregunta_cat"]]),
           pregunta_cat = forcats::fct_na_value_to_level(.data[["pregunta_cat"]],
                                                         level = 'NA'))

  tabla <- tabla |>
    count(across(c('pregunta_var', 'pregunta_cat')),
          wt = .data[['n']],
          name = 'casos') |>
    mutate(prop = casos/sum(casos),
           .by = 'pregunta_var')

  tabla |>
    mutate(pregunta_lab = forcats::as_factor(seg_labels[.data[["pregunta_var"]]])) %>%
    select(starts_with('pregunta'), everything())
}

tabla_categoria2 <- function(.df,
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
                       pregunta_cat = to_factor(v_pregunta))

  # Declarar explícitamente NA que pudiesen estar en la categoría de respuestas
  # de la pregunta.
  # Solo si hay casos NA en el factor
  if(is.na(df_agg$pregunta_cat) |> any()) {
    df_agg$pregunta_cat <- forcats::fct_na_value_to_level(df_agg$pregunta_cat)
  }

  tab <- aggregate(casos ~ pregunta_cat,
                   df_agg,
                   FUN = \(x) sum(x, na.rm = FALSE),
                   na.action = na.pass,
                   drop = FALSE)

  # Agregar 0 si es que hay combinación de pregunta y segmento sin casos.
  tab$casos <- replace(tab$casos, is.na(tab$casos), 0)

  # Variable y etiqueta de pregunta
  tab$pregunta_var <- .var
  tab$pregunta_lab <- attr(.df[[.var]], 'label', exact = TRUE) %||% '-'

  # Agrega el porcentaje de respuesta.
  tab <- tabla_prop2(tab,
                     .segmento = 'pregunta_var')

  # Agrega el porcentaje válido si es que se señalan categorias perdidas.
  if (!is.null(miss)) {
    tab <- tabla_prop_val2(tab,
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


tabla_categorias2 <- function(.df,
                              ...,
                              .wt = NULL,
                              miss = NULL) {

  # Transformar variable en NSE a un chr para poder ser input de función
  # tabla_cateroria2
  wt_quo <- enquo(.wt)

  if(!rlang::quo_is_null(wt_quo)){
    .wt <- wt_quo |> as_label()
  }

  vars <- tidyselect::eval_select(expr = expr(c(...)),
                                  data = .df)
  vars <- names(vars)

  tab <- map(
    vars,
    \(x) tabla_categoria2(.df = .df,
                          .var = x,
                          .wt = .wt,
                          miss = miss)
  )

  tab |>
    list_flatten() |>
    list_rbind()
}



tabla_orden <- function(.data, .var, .segmento = NULL) {
  # Orden de variables y categorias para la presentación de tablas.

  var_quo <- rlang::enquo(.var)
  segmento_quo <- rlang::enquo(.segmento)

  var_seg_exprs <- rlang::exprs(!!segmento_quo, starts_with(!!rlang::as_label(var_quo)))

  .data %>%
    select_at(vars(!!!var_seg_exprs, everything())) %>%
    arrange_at(vars(!!!var_seg_exprs))
}

tabla_prop <- function(.df,
                       .segmento) {
  # Cálculo de porcetaje de respuestas en tabla con numero de casos.

  segmento_quo <- rlang::enquo(.segmento)

  .df %>%
    group_by(across(!!segmento_quo)) %>%
    mutate(prop = .data[['casos']] / sum(.data[['casos']])) %>%
    ungroup()
}


tabla_prop2 <- function(.df,
                        .segmento) {
  # Cálculo de porcetaje de respuestas en tabla con numero de casos.

  .df |>
    mutate(prop = .data[['casos']] / sum(.data[['casos']], na.rm = TRUE),
           .by = all_of(.segmento))
}

tabla_prop_val <- function(.data, .var, .segmento, miss) {
  # Cálculo de porcetaje de respuestas válidas en tabla con numero de casos.

  # Pasar de quosure con texto a string y luego simbolo.
  var_quo <- rlang::sym(rlang::as_name(.var))

  .data %>%
    mutate(casos_val = replace(.data[['casos']], (!!var_quo %in% miss), NA_real_),
           prop_val = .data[['casos_val']] / sum(.data[['casos_val']], na.rm = TRUE),
           .by = {{ .segmento }}) %>%
    select(!'casos_val')
}


tabla_prop_val2 <- function(.df,
                            by = NULL,
                            miss) {
  # Cálculo de porcetaje de respuestas válidas en tabla con numero de casos.

  # Pasar de quosure con texto a string y luego simbolo.
  .df$casos_val <- replace(.df$casos,
                           .df$pregunta_cat %in% miss,
                           NA)

  df <- .df |>
    mutate(prop_val = casos_val / sum(casos_val, na.rm = TRUE),
           .by = all_of(by))

  # Eliminar variable auxiliar.
  df$casos_val <- NULL

  return(df)
}



tabla_total <- function(.data,
                        .var,
                        .segmento,
                        miss = NULL) {
  # Cálculo de porcetaje para el total de segmento

  tab_total <- .data %>%
    group_by(across({{ .var }})) %>%
    summarise(across('casos', sum)) %>%
    mutate({{ .segmento }} := "Total") %>%
    ungroup()

  tab_total <- tabla_prop(tab_total, .segmento = NULL)

  # Agrega el porcentaje válido si es que se señalan categorias perdidas.
  tab <- bind_rows(.data %>%
                     mutate({{ .segmento }} := as.character({{ .segmento }})),
                   tab_total)

  tab <- tab %>%
    mutate({{ .segmento }} := forcats::as_factor({{ .segmento }}))

  if (!is.null(miss)) {
    tab <- tabla_prop_val(tab,
                          .var = enquo(.var),
                          .segmento = {{ .segmento }},
                          miss = miss)
  }
  return(tab)
}

tabla_total2 <- function(.df) {

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

  tab <- bind_rows(.df,
                   tab_tot)

  tab$segmento_cat <- forcats::as_factor(tab$segmento_cat)

  return(tab)
}


tabla_var_segmento <- function(.data,
                               .var,
                               .segmento = NULL,
                               .wt = NULL,
                               total = FALSE,
                               miss = NULL) {
  # Tabla con número de casos y proporción de variable
  # Se agrega una variable de de segmentación llamada 'segmento' con valor 'Total'.

  var_quo <- enquo(.var)
  segmento_quo <- enquo(.segmento)
  wt_quo <- enquo(.wt)

  tab <- .data %>%
    mutate(across(c(!!segmento_quo, !!var_quo, !!wt_quo),
                  \(x) sjmisc::to_label(x, add.non.labelled = TRUE)),
           .keep = 'none') |>
    # group_by(across(c(!!segmento_quo, !!var_quo))) %>%
    summarise(casos = sum(!!wt_quo %||% n()),
              .by = c(!!segmento_quo, !!var_quo))
  # ungroup()

  # Agrega el porcentaje de respuesta.
  tab <- tabla_prop(tab,
                    .segmento = !!segmento_quo)

  # Agrega el porcentaje válido si es que se señalan categorias perdidas.
  if (!is.null(miss)) {
    tab <- tabla_prop_val(tab,
                          .var = var_quo,
                          .segmento = !!segmento_quo,
                          miss = miss)
  }

  # Agrega el porcentaje total a los segmentos.
  if (total) {
    tab <- tabla_total(tab,
                       .var = !!var_quo,
                       .segmento = !!segmento_quo,
                       miss = miss)
  }

  tabla_orden(tab,
              .var = !!var_quo,
              .segmento = !!segmento_quo)
}

tabla_var_segmento2 <- function(.df,
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
                       segmento_cat = forcats::as_factor(v_segmento),
                       pregunta_cat = forcats::as_factor(v_pregunta))

  # Declarar explícitamente NA que pudiesen estar en la categoría de respuestas
  # de la pregunta.
  # Solo si hay casos NA en el factor
  if(is.na(df_agg$pregunta_cat) |> any()) {
    df_agg$pregunta_cat <- forcats::fct_na_value_to_level(df_agg$pregunta_cat)
  }

  tab <- aggregate(casos ~ segmento_cat + pregunta_cat,
                   df_agg,
                   FUN = \(x) sum(x, na.rm = FALSE),
                   na.action = na.pass,
                   drop = FALSE)

  # Agregar 0 si es que hay combinación de pregunta y segmento sin casos.
  tab$casos <- replace(tab$casos, is.na(tab$casos), 0)

  # Variable y etiqueta de pregunta
  tab$pregunta_var <- .var
  tab$pregunta_lab <- attr(.df[[.var]], 'label', exact = TRUE) %||% '-'

  # Variable y etiqueta de segmentos
  tab$segmento_var <- segmento_var
  tab$segmento_lab <- segmento_lab

  # Agrega el porcentaje total a los segmentos.
  if (total) {
    tab <- tabla_total2(tab)
  }

  # Agrega el porcentaje de respuesta.
  tab <- tabla_prop2(tab,
                     .segmento = 'segmento_cat')

  # Agrega el porcentaje válido si es que se señalan categorias perdidas.
  if (!is.null(miss)) {
    tab <- tabla_prop_val2(tab,
                           miss = c(NA, miss),
                           by = 'segmento_cat')
  }

  # Orden de variables de salida
  col_orden <- c('segmento_var', 'segmento_lab', 'segmento_cat',
                 'pregunta_var', 'pregunta_lab', 'pregunta_cat')

  col_adicionales <- setdiff(names(tab), col_orden)

  tab <- tab[c(col_orden, col_adicionales)]

  # Orden de casos
  tab |>
    arrange(segmento_cat, pregunta_cat)
}



tabla_var_segmentos <- function(.data,
                                .var,
                                .segmentos,
                                .wt = NULL,
                                total = FALSE,
                                miss = NULL) {
  # Resultados de una pregunta `.var` para varios segmentos `.segmentos`

  tabla_var_seg <- function(.data, .seg) {

    seg_quo <- enquo(.seg)

    df <- tabla_var_segmento(.data,
                             .var = {{ .var }},
                             .segmento = {{ .seg }},
                             total = total,
                             .wt = {{ .wt }},
                             miss = miss)

    # print(seg_quo)
    # print(rlang::quo_is_null(seg_quo))

    df <- df |>
      mutate(segmento_var = !!rlang::as_label(seg_quo))

    if(rlang::quo_is_null(seg_quo)){
      df$segmento_cat <- 'NULL'
    } else {
      df <- df |>
        rename(segmento_cat = !!rlang::as_label(seg_quo))
    }

    df |>
      mutate(across(any_of(c('segmento_var', 'segmento_cat')), as.character))
  }

  tab <- map(.segmentos, ~tabla_var_seg(.data, .seg = !!.))

  tab <- reduce(tab, bind_rows) |>
    mutate(across(any_of('segmento_cat'), forcats::as_factor))

  # Copia label y labels a variable "var" recién creada.
  # No utilizo esto para no pegar las etiquetas de
  # tab <- sjlabelled::copy_labels(df_new = tab,
  #                                df_origin = .data)

  # Dejar solo el 'label' de la variable recién creada
  var_filtro <- rlang::as_name(enquo(.var))

  tab[[var_filtro]] <- structure(tab[[var_filtro]],
                                 label = attr(.data[[var_filtro]], 'label', exact = TRUE))

  tab %>%
    relocate(any_of(c('segmento_var',
                      'segmento_cat')),
             .before = 1)
}

#' @title Tabla de porcentajes de variables según segmentos.
#'
#'  Obtiene porcentajes de respuestas de múltiples variables según multiples segmentos.
#'
#' @param .data tibble
#'
#' @param .vars vars(), lista de nombres de variables de las que se quiere saber su proporción de respuestas
#' @param .segmentos vars(), lista de nombres de variables de segmentación de las preguntas de `.vars`
#' @param .wt name, nombre de la variable de ponderación
#' @param total logical, Si total = TRUE, se agrega el total para cada segmento.
#' @param miss integers, Vector de valores que deben coniderarse como missings.
#'
#' @return tibble
#'
#' @import dplyr
#' @importFrom purrr map map2 reduce
#' @importFrom forcats as_factor
#' @importFrom tidyselect vars_select
#' @importFrom rlang .data
#'
#' @export
tabla_vars_segmentos <- function(.data,
                                 .vars,
                                 .segmentos,
                                 .wt = NULL,
                                 total = FALSE,
                                 miss = NULL) {

  variables <- tidyselect::vars_select(names(.data), !!!.vars)

  tab <- purrr::map(variables, ~tabla_var_segmentos(.data,
                                                    .var = !!.,
                                                    .segmentos = .segmentos,
                                                    .wt = {{ .wt }},
                                                    total = total,
                                                    miss = miss))

  tabla_variables <- function(.data, .var) {

    # Captura la etiqueta de la variable. Si no tiene, lo deja en blanco
    var_label <- attr(.data[[.var]], 'label') %||% "-"

    .data %>%
      mutate(pregunta_var = .var,
             pregunta_lab = var_label) %>%
      rename(pregunta_cat = all_of(.var)) %>%
      mutate(across('pregunta_var', as.character))
  }

  purrr::map2(tab, variables, ~tabla_variables(.x, .y)) %>%
    purrr::reduce(bind_rows) %>%
    select(starts_with("segmento"),
           'pregunta_var',
           'pregunta_lab',
           'pregunta_cat',
           everything()) %>%
    mutate(across(c('segmento_var',
                    'pregunta_var',
                    'pregunta_lab'),
                  forcats::as_factor)
    )
}


tabla_vars_segmentos2 <- function(.df,
                                  .vars,
                                  .segmentos = NULL,
                                  .wt = NULL,
                                  total = FALSE,
                                  miss = NULL) {

  vars <- tidyselect::eval_select(expr = enquo(.vars),
                                  data = .df)
  vars <- names(vars)

  segmentos_quo <- enquo(.segmentos)

  if(quo_is_null(segmentos_quo)){
    segmentos <- list(NULL)
  } else {
    segmentos <- tidyselect::eval_select(expr = segmentos_quo,
                                         data = .df)
    segmentos <- names(segmentos)
  }

  segmentos <- c(segmentos, NULL)

  # print(segmentos)

  tab <- map(
    vars,
    \(x) map(
      segmentos,
      \(y) tabla_var_segmento2(.df = .df,
                               .var = x,
                               .segmento = y,
                               .wt = .wt,
                               total = total,
                               miss = miss)
    )
  )

  tab |>
    list_flatten() |>
    list_rbind()
}
