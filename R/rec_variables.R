# Recodificación

# Recodificación numérica según especificación tipo "1:2 = 1; 3 = 2; else = 9".
# Soporta rangos (a:b), valores separados por coma y "else" (que no captura NA).
rec_num <- function(x, rec) {
        reglas <- trimws(strsplit(rec, ";", fixed = TRUE)[[1]])

        out <- rep(NA_real_, length(x))
        recodificado <- rep(FALSE, length(x))
        else_val <- NULL

        for (regla in reglas) {
                partes <- trimws(strsplit(regla, "=", fixed = TRUE)[[1]])

                if (length(partes) != 2 || any(partes == "") || is.na(partes[2])) {
                        stop("Regla de recodificaci\u00f3n inv\u00e1lida: '", regla, "'")
                }

                lhs <- partes[1]
                rhs <- as.numeric(partes[2])

                if (lhs == "else") {
                        else_val <- rhs
                        next
                }

                if (grepl(":", lhs, fixed = TRUE)) {
                        limites <- as.numeric(trimws(strsplit(lhs, ":", fixed = TRUE)[[1]]))
                        idx <- !is.na(x) & x >= limites[1] & x <= limites[2]
                } else {
                        valores <- as.numeric(trimws(strsplit(lhs, ",", fixed = TRUE)[[1]]))
                        idx <- !is.na(x) & x %in% valores
                }

                out[idx & !recodificado] <- rhs
                recodificado <- recodificado | idx
        }

        if (!is.null(else_val)) {
                out[!recodificado & !is.na(x)] <- else_val
        }

        out
}

# Función para recodifircar variables y agregarle etiquetas a las
rec_cat <- function(variable, rec, labels = NULL) {
        rec_num(variable, rec = rec) %>%
                haven::labelled(
                        labels = labels,
                        label = attr(variable, "label")
                )
}

#' @title Recodificaión de variables 5 a 3
#'
#' @description
#' Por defecto recodifica variables _likert_ de 5 niveles a 3 niveles.
#'
#' @param variable numeric, Valores de variable a recodificar.
#' @param rec string, por defecto recodifica 1:2 en 1, 3 en 2 y 4:5 en 3.
#'        Todo lo demás lo deja como 9.
#'        Acepta rangos (`a:b`), valores separados por coma y `else`
#'        (los `NA` se mantienen como `NA`).
#' @param labels vector string, etiquetas para las variables recodificadas.
#'        Por defecto NULL.
#'
#' @importFrom haven labelled
#'
#' @return haven_labelled
#' @export
#'
#' @examples
#'
#' vect <- c(1, 4, 6, 99, NA)
#' rec_cat_5a3(vect, labels = c("alto" = 1))
#'
rec_cat_5a3 <- function(
        variable,
        rec = "1:2 = 1; 3 = 2; 4:5 = 3; else = 9",
        labels = NULL
) {
        rec_cat(variable = variable, rec = rec, labels = labels)
}

#' @title Recodificaión de variables 7 a 3
#'
#' @description
#' Por defecto recodifica variables de _notas_ de 7 niveles a 3 niveles.
#'
#' @param variable numeric, Valores de variable a recodificar.
#' @param rec string, por defecto recodifica 1:4 en 1, 5 en 2 y 6:7 en 3.
#'        Todo lo demás lo deja como 9.
#'        Acepta rangos (`a:b`), valores separados por coma y `else`
#'        (los `NA` se mantienen como `NA`).
#' @param labels vector string, etiquetas para las variables recodificadas.
#'        Por defecto NULL.
#'
#' @importFrom haven labelled
#'
#' @return haven_labelled
#' @export
#'
#' @examples
#'
#' vect <- c(1, 4, 6, 99, NA)
#' rec_cat_7a3(vect, labels = c("alto" = 1))
#'
rec_cat_7a3 <- function(
        variable,
        rec = "1:4 = 1; 5 = 2; 6:7 = 3; else = 9",
        labels = NULL
) {
        rec_cat(variable = variable, rec = rec, labels = labels)
}
