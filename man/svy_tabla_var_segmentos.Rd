% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tablas_survey.R
\name{svy_tabla_var_segmentos}
\alias{svy_tabla_var_segmentos}
\title{Tabla con intervalos de confianza}
\usage{
svy_tabla_var_segmentos(.data, .var, .segmentos = NULL, ...)
}
\arguments{
\item{.data}{`tbl_svy` data.frame con diseño de encuesta.}

\item{.var}{Variable de interés respecto.}

\item{.segmentos}{vars(). Lista de variables por las que se quiere segmentar `.var`.}

\item{...}{atributos que se pasan a funcion `svy_tabla_var_segmento`.}
}
\value{
data.frame
}
\description{
Devuelve tabla de frecuencias con intervalos de confianza para un nivel
`level`de significancia entre las categorías de respuesta de la
variable `.var`.
}
