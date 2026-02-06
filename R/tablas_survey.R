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
#' @param .df data frame con diseño complejo
#' @param .var Variable en la que interesa comparar categorías de respuesta.
#' @param .segmento Segmentos de interés para ver diferencias en categorías
#'        de variable `.var`. Por defecto NULL
#' @param miss chr vector. Categorías a excluir en el denominador de `prop_val` y `mean`.
#' @param vartype chr vector. Tipo de error para estimaciones (`ci`, `se`, etc.).
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

  if (is.null(.segmento)) {
    .df[["variables"]][["total"]] <- forcats::as_factor("Total")
    .segmento <- "total"
    segmento_var <- "Total"
    segmento_lab <- "Total"
  } else {
    .df[["variables"]][[.segmento]] <- forcats::as_factor(.df[["variables"]][[.segmento]])
    segmento_var <- .segmento
    segmento_lab <- attr(.df[["variables"]][[.segmento]], "label", exact = TRUE) %||% "-"
  }

  v_pregunta <- .df[["variables"]][[.var]]

  if (desuc_is_categorical_var(v_pregunta)) {
    .df[["variables"]][[.var]] <- forcats::as_factor(v_pregunta)

    if (any(is.na(.df[["variables"]][[.var]]))) {
      .df[["variables"]][[.var]] <- forcats::fct_na_value_to_level(.df[["variables"]][[.var]])
    }

    df_group <- .df |>
      dplyr::group_by(pick(any_of(c(.segmento, .var))), .drop = FALSE)

    tab <- df_group |>
      dplyr::summarise(
        casos_unwt = srvyr::unweighted(n()),
        casos = srvyr::survey_total(),
        prop = srvyr::survey_prop(
          vartype = vartype,
          level = level,
          proportion = TRUE,
          prop_method = "xlogit"
        ),
        .groups = "drop"
      )

    if (!is.null(miss)) {
      .df[["variables"]][[".desuc_is_valid"]] <-
        !desuc_is_missing_category(.df[["variables"]][[.var]], miss = miss)

      tab_denom <- .df |>
        dplyr::group_by(pick(any_of(.segmento)), .drop = FALSE) |>
        dplyr::summarise(
          casos_val = srvyr::survey_total(.data[[".desuc_is_valid"]]),
          .groups = "drop"
        )

      tab <- dplyr::left_join(tab, tab_denom, by = .segmento)
      tab$prop_val <- tab$casos / tab$casos_val
      tab$prop_val <- ifelse(
        desuc_is_missing_category(tab[[.var]], miss = miss),
        NA_real_,
        tab$prop_val
      )
    }
  } else {
    .df[["variables"]][[".desuc_is_valid"]] <-
      !is.na(v_pregunta) & !(v_pregunta %in% miss)
    .df[["variables"]][[".desuc_val"]] <- ifelse(
      .df[["variables"]][[".desuc_is_valid"]],
      v_pregunta,
      NA_real_
    )

    tab <- .df |>
      dplyr::group_by(pick(any_of(.segmento)), .drop = FALSE) |>
      dplyr::summarise(
        casos_unwt = srvyr::unweighted(n()),
        casos = srvyr::survey_total(),
        casos_val = srvyr::survey_total(.data[[".desuc_is_valid"]]),
        mean = srvyr::survey_mean(
          .data[[".desuc_val"]],
          vartype = vartype,
          level = level,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    tab$pregunta_cat <- "mean"
  }

  tab$pregunta_var <- .var
  tab$pregunta_lab <- attr(.df[["variables"]][[.var]], "label", exact = TRUE) %||% "-"

  if (desuc_is_categorical_var(v_pregunta)) {
    names(tab)[names(tab) == .var] <- "pregunta_cat"
  }

  tab$segmento_var <- segmento_var
  tab$segmento_lab <- segmento_lab
  names(tab)[names(tab) == .segmento] <- "segmento_cat"

  if (sum(stringr::str_detect(names(tab), "_upp$|_low$")) == 2) {
    tab <- tab |>
      tidyr::nest(.by = "segmento_cat") |>
      dplyr::mutate(data = purrr::map(data, svy_diff_sig)) |>
      tidyr::unnest(data)
  }

  # Orden de variables de salida
  col_orden <- c("segmento_var", "segmento_lab", "segmento_cat",
                 "pregunta_var", "pregunta_lab", "pregunta_cat")

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
#' @param .df `tbl_svy` data.frame con diseño de encuesta.
#' @param .vars c(). Variables de interés respecto.
#' @param .segmentos c(). Lista de variables por las que se quiere segmentar `.vars`.
#' @param miss chr vector. Categorías a excluir en el denominador de `prop_val` y `mean`.
#' @param vartype chr vector. Tipo de error para estimaciones (`ci`, `se`, etc.).
#' @param level double. Nivel de confianza para intervalos.
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

  if(!inherits(.df, 'tbl_svy')) {
    stop("Se necesita un data.frame con diseno complejo")
  }

  tabla_vars_segmentos(
    .df = .df,
    .vars = {{ .vars }},
    .segmentos = {{ .segmentos }},
    miss = miss,
    vartype = vartype,
    level = level
  )
}

#' @export
#' @rdname tabla_vars_segmentos
#' @param vartype chr vector. Tipo de error para estimaciones de encuesta.
#' @param level double. Nivel de confianza para intervalos.
tabla_vars_segmentos.tbl_svy <- function(
  .df,
  .vars,
  .segmentos = NULL,
  miss = NULL,
  vartype = c("ci", "se"),
  level = 0.95,
  ...
) {
  vars <- desuc_eval_select_names(
    data = .df[["variables"]],
    sel_quo = rlang::enquo(.vars),
    allow_null = FALSE
  )

  segmentos <- desuc_eval_select_names(
    data = .df[["variables"]],
    sel_quo = rlang::enquo(.segmentos),
    allow_null = TRUE
  )

  tab <- purrr::map(
    vars,
    \(x_v) {
      purrr::map(
        segmentos,
        \(y_s) {
          svy_tabla_var_segmento(
            .df = .df,
            .var = x_v,
            .segmento = y_s,
            miss = miss,
            vartype = vartype,
            level = level
          )
        }
      )
    }
  )

  tab |>
    purrr::list_flatten() |>
    purrr::list_rbind() |>
    tibble::as_tibble()
}

#' @export
#' @rdname tabla_vars_segmentos
tabla_vars_segmentos.survey.design2 <- function(
  .df,
  .vars,
  .segmentos = NULL,
  miss = NULL,
  vartype = c("ci", "se"),
  level = 0.95,
  ...
) {
  .df_tbl <- srvyr::as_survey_design(.df)

  tabla_vars_segmentos(
    .df = .df_tbl,
    .vars = {{ .vars }},
    .segmentos = {{ .segmentos }},
    miss = miss,
    vartype = vartype,
    level = level
  )
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
