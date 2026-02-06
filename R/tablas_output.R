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

tabla_categoria <- function(.df, .var, .wt = NULL, miss = NULL) {
  v_pregunta <- .df[[.var]]

  # Si no hay pesos, se construye un vector de 1.
  if (is.null(.wt)) {
    v_wgt <- rep(1L, length(v_pregunta))
  } else {
    v_wgt <- .df[[.wt]]
  }

  df_agg <- data.frame(
    casos = v_wgt,
    pregunta_cat = forcats::as_factor(v_pregunta)
  )

  # Declarar explícitamente NA que pudiesen estar en la categoría de respuestas
  # de la pregunta.
  # Solo si hay casos NA en el factor
  if (is.na(df_agg$pregunta_cat) |> any()) {
    df_agg$pregunta_cat <- forcats::fct_na_value_to_level(df_agg$pregunta_cat)
  }

  tab <- aggregate(
    casos ~ pregunta_cat,
    df_agg,
    FUN = \(x) sum(x, na.rm = FALSE),
    na.action = "na.pass",
    drop = FALSE
  )

  # Agregar 0 si es que hay combinación de pregunta y segmento sin casos.
  tab$casos <- replace(tab$casos, is.na(tab$casos), 0)

  # Variable y etiqueta de pregunta
  tab$pregunta_var <- .var
  tab$pregunta_lab <- attr(.df[[.var]], "label", exact = TRUE) %||% "-"

  # Agrega el porcentaje de respuesta.
  tab <- tabla_prop(tab, .segmento = "pregunta_var")

  # Agrega el porcentaje válido si es que se señalan categorias perdidas.
  if (!is.null(miss)) {
    tab <- tabla_prop_val(tab, miss = miss)
  }

  # Orden de variables de salida
  col_orden <- c("pregunta_var", "pregunta_lab", "pregunta_cat")

  col_adicionales <- setdiff(names(tab), col_orden)

  tab <- tab[c(col_orden, col_adicionales)]

  # Orden de casos
  tab |>
    arrange(pregunta_cat)
}

#' @title Tabla de categorías
#'
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
#' @param miss chr vector. Categorías de respuesta que deben excluirse del cálculo de `prop_val`.
#'
#' @return tibble
#'
#' @importFrom rlang quo_is_null enquo
#' @importFrom tidyselect eval_select
#' @importFrom purrr map list_flatten list_rbind
#'
#' @export
tabla_categorias <- function(.df, ..., .wt = NULL, miss = NULL) {
  # Transformar variable en NSE a un chr para poder ser input de función
  # tabla_cateroria2
  wt_quo <- rlang::enquo(.wt)

  if (!rlang::quo_is_null(wt_quo)) {
    .wt <- wt_quo |> as_label()
  }

  vars <- tidyselect::eval_select(expr = expr(c(...)), data = .df)
  vars <- names(vars)

  tab <- map(
    vars,
    \(x) tabla_categoria(.df = .df, .var = x, .wt = .wt, miss = miss)
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
#' df <- tibble::tibble(var = c("a", "b"),
#'             casos = c(30, 70))
#'
#' desuctools:::tabla_prop(df, .segmento = NULL)
#'
#' desuctools:::tabla_prop(df, .segmento = "var")
#'
tabla_prop <- function(.df, .segmento) {
  # Cálculo de porcentaje de respuestas en tabla con numero de casos.
  # Proporción de "casos" en cada grupo ".segmento" y luego reemplazar NaN por 0.

  .df$prop <- ave(
    .df[["casos"]],
    .df[[.segmento]],
    FUN = \(x) x / sum(x, na.rm = TRUE)
  )
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
#' df <- tibble::tibble(
#' pregunta_cat = c("a", "b", "x"),
#' casos = c(30, 30, 40)
#' )
#'
#' desuctools:::tabla_prop_val(df, miss = "x")
#'
#' desuctools:::tabla_prop_val(df, by = "pregunta_cat", miss = "x")
#'
tabla_prop_val <- function(.df, by = NULL, miss) {
  # Cálculo de porcetaje de respuestas válidas en tabla con numero de casos.

  .df$casos_val <- replace(
    .df$casos,
    desuc_is_missing_category(.df$pregunta_cat, miss = miss),
    NA
  )

  df <- .df |>
    dplyr::mutate(
      prop_val = .data[["casos_val"]] / sum(.data[["casos_val"]], na.rm = TRUE),
      .by = all_of(by)
    )

  # Eliminar variable auxiliar.
  df$casos_val <- NULL

  return(df)
}


# Helpers internos para selección y tipo de variable ----------------------

desuc_normalize_tidyselect <- function(sel_quo) {
  expr_sel <- rlang::quo_get_expr(sel_quo)

  if (rlang::is_call(expr_sel, "vars")) {
    expr_sel <- rlang::expr(c(!!!rlang::call_args(expr_sel)))
    sel_quo <- rlang::new_quosure(expr_sel, env = rlang::quo_get_env(sel_quo))
  }

  sel_quo
}

desuc_eval_select_names <- function(data, sel_quo, allow_null = FALSE) {
  if (allow_null && rlang::quo_is_null(sel_quo)) {
    return(list(NULL))
  }

  sel_quo <- desuc_normalize_tidyselect(sel_quo)
  sel <- tidyselect::eval_select(expr = sel_quo, data = data)

  names(sel)
}

desuc_is_categorical_var <- function(x) {
  if (is.factor(x) || is.character(x)) {
    return(TRUE)
  }

  if (inherits(x, "haven_labelled")) {
    labels <- attr(x, "labels", exact = TRUE)
    return(!is.null(labels) && length(labels) > 0)
  }

  FALSE
}

desuc_is_missing_category <- function(x, miss = NULL) {
  x_chr <- as.character(x)
  miss_chr <- as.character(miss[!is.na(miss)])

  is.na(x) | is.na(x_chr) | x_chr %in% c("(Missing)", miss_chr)
}

desuc_weighted_mean <- function(x, w) {
  if (length(x) == 0) {
    return(NA_real_)
  }

  val <- stats::weighted.mean(x = x, w = w, na.rm = TRUE)

  ifelse(is.nan(val), NA_real_, val)
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
  tab_tot <- aggregate(
    casos ~ pregunta_cat,
    data = .df,
    FUN = \(x) sum(x, na.rm = FALSE),
    na.action = na.pass,
    drop = FALSE
  )

  tab_tot$segmento_var <- unique(.df$segmento_var)
  tab_tot$segmento_lab <- unique(.df$segmento_lab)

  tab_tot$segmento_cat <- "Total"

  tab_tot$pregunta_var <- unique(.df$pregunta_var)
  tab_tot$pregunta_lab <- unique(.df$pregunta_lab)

  tab <- dplyr::bind_rows(.df, tab_tot)

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
tabla_var_segmento <- function(
  .df,
  .var,
  .segmento = NULL,
  .wt = NULL,
  miss = NULL,
  total = FALSE
) {
  v_pregunta <- .df[[.var]]

  # Si no hay pesos, se construye un vector de 1.
  if (is.null(.wt)) {
    v_wgt <- rep(1L, length(v_pregunta))
  } else {
    v_wgt <- .df[[.wt]]
  }

  if (is.null(.segmento)) {
    # Si no hay segmentos sobre los que dividir la variable de interés.
    v_segmento <- rep("Total", length(v_pregunta))
    segmento_var <- "Total"
    segmento_lab <- "Total"
  } else {
    v_segmento <- .df[[.segmento]]
    segmento_var <- .segmento
    segmento_lab <- attr(.df[[.segmento]], "label", exact = TRUE) %||% "-"
  }

  df_agg <- data.frame(
    casos = v_wgt,
    segmento_cat = forcats::as_factor(v_segmento)
  )

  if (desuc_is_categorical_var(v_pregunta)) {
    # Caso en que pregunta es labelled, factor o string: se calcula proporción.
    df_agg$pregunta_cat <- forcats::as_factor(v_pregunta)

    # Declarar explícitamente NA que pudiesen estar en la categoría de respuestas
    # de la pregunta.
    # Solo si hay casos NA en el factor
    if (anyNA(df_agg$pregunta_cat)) {
      df_agg$pregunta_cat <- forcats::fct_na_value_to_level(df_agg$pregunta_cat)
    }

    tab <- aggregate(
      casos ~ segmento_cat + pregunta_cat,
      df_agg,
      FUN = \(x) sum(x, na.rm = FALSE),
      na.action = na.pass,
      drop = FALSE
    )

    # Agregar 0 si es que hay combinación de pregunta y segmento sin casos.
    tab$casos <- replace(tab$casos, is.na(tab$casos), 0)

    # Agrega el porcentaje total a los segmentos.
    if (total) {
      tab <- tabla_total_prop(tab)
    }

    # Agrega el porcentaje de respuesta.
    tab <- tabla_prop(tab, .segmento = "segmento_cat")

    # Agrega el porcentaje válido si es que se señalan categorias perdidas.
    if (!is.null(miss)) {
      tab <- tabla_prop_val(tab, miss = miss, by = "segmento_cat")
    }
  } else {
    # Caso en que pregunta sea numérica: se calcula promedio.
    df_agg$pregunta_cat <- v_pregunta

    tab <- aggregate(
      x = seq_len(nrow(df_agg)), # índices de fila
      by = list(segmento_cat = df_agg[["segmento_cat"]]),
      FUN = function(idx) {
        x <- df_agg[["pregunta_cat"]][idx]
        x_val <- !is.na(x) & !(x %in% miss)
        w <- df_agg[["casos"]][idx]

        c(
          casos = sum(w, na.rm = TRUE),
          casos_val = sum(w[x_val], na.rm = TRUE),
          mean = desuc_weighted_mean(x = x[x_val], w = w[x_val])
        )
      },
      drop = FALSE
    )

    tab <- cbind(tab[1], tab$x)

    tab$pregunta_cat <- "mean"

    if (total && !is.null(.segmento)) {
      x <- df_agg[["pregunta_cat"]]
      x_val <- !is.na(x) & !(x %in% miss)
      w <- df_agg[["casos"]]

      tab_tot <- data.frame(
        segmento_cat = "Total",
        casos = sum(w, na.rm = TRUE),
        casos_val = sum(w[x_val], na.rm = TRUE),
        mean = desuc_weighted_mean(x = x[x_val], w = w[x_val]),
        pregunta_cat = "mean"
      )

      tab <- dplyr::bind_rows(tab, tab_tot)
      tab$segmento_cat <- forcats::as_factor(tab$segmento_cat)
    }
  }

  # Variable y etiqueta de pregunta
  tab$pregunta_var <- .var
  tab$pregunta_lab <- attr(.df[[.var]], "label", exact = TRUE) %||% "-"

  # Variable y etiqueta de segmentos
  tab$segmento_var <- segmento_var
  tab$segmento_lab <- segmento_lab

  # Orden de variables de salida
  col_orden <- c(
    "segmento_var",
    "segmento_lab",
    "segmento_cat",
    "pregunta_var",
    "pregunta_lab",
    "pregunta_cat"
  )

  col_adicionales <- setdiff(names(tab), col_orden)

  tab <- tab[c(col_orden, col_adicionales)]

  # Orden de casos
  tab[order(tab$segmento_cat, tab$pregunta_cat), ]
}


#' @title Tabla de porcentajes de variables según segmentos.
#'
#' @description
#' Obtiene porcentajes de respuestas de múltiples variables según múltiples segmentos.
#' Función genérica que funciona con data.frames regulares y objetos tbl_svy.
#'
#' @param .df Un data.frame, tibble o tbl_svy
#' @param .vars tidyselect, lista de nombres de variables de las que se quiere saber su proporción de respuestas
#' @param .segmentos tidyselect, lista de nombres de variables de segmentación de las preguntas de `.vars`
#' @param ... Parámetros adicionales que serán pasados a los métodos específicos
#'
#' @return tibble
#'
#' @export
tabla_vars_segmentos <- function(.df, .vars, .segmentos = NULL, ...) {
  UseMethod("tabla_vars_segmentos")
}

#' @export
#' @rdname tabla_vars_segmentos
#' @param .wt name, nombre de la variable de ponderación
#' @param miss integers, Vector de valores que deben coniderarse como missings.
#' @param total logical, Si total = TRUE, se agrega el total para cada segmento.
tabla_vars_segmentos.data.frame <- function(
  .df,
  .vars,
  .segmentos = NULL,
  .wt = NULL,
  miss = NULL,
  total = FALSE,
  ...
) {
  vars <- desuc_eval_select_names(
    data = .df,
    sel_quo = rlang::enquo(.vars),
    allow_null = FALSE
  )

  # Revisar si se entrega un ponderador
  wt_quo <- rlang::enquo(.wt)

  if (rlang::quo_is_null(wt_quo)) {
    wt_chr <- NULL
  } else {
    wt_chr <- rlang::as_label(wt_quo)
  }

  segmentos <- desuc_eval_select_names(
    data = .df,
    sel_quo = rlang::enquo(.segmentos),
    allow_null = TRUE
  )

  tab <- purrr::map(
    vars,
    \(x_v) {
      purrr::map(
        segmentos,
        \(y_s) {
          tabla_var_segmento(
            .df = .df,
            .var = x_v,
            .segmento = y_s,
            .wt = wt_chr,
            total = total,
            miss = miss
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
