% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rec_ortografia.R
\name{rec_ortografia}
\alias{rec_ortografia}
\title{Corrector ortográfico}
\source{
\url{https://nubededatos.blogspot.com/2015/01/corrector-ortografico-en-espanol-para-r.html}
}
\usage{
rec_ortografia(word, sorted_words = desuctools::corpus_rae$Orden[1:5000])
}
\arguments{
\item{word}{chr Palabra a corregir}

\item{sorted_words}{Lista de palabras de referencia ordenadas por frecuencia.
Por defecto usa lista de CRAE}
}
\value{
character
}
\description{
Código idéntico al de Rasmus Bååth
}
\examples{

rec_ortografia(c('pais', 'arbol'))

}
