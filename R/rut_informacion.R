#' Obtiene información para un RUT
#'
#' @description
#' Obtiene información personal a partir de un RUT de Chile.
#'
#' @param .rut string. rut sin puntos, con guión y dígito verificador.
#'
#' @returns
#' Una lista con datos asociados al RUT.
#'
#' @export
#'
#' @examples
#' rut_list = c(
#' "8714763-0",
#' "4606477-1",
#' "20283632-1"
#' ) # example list of RUT values
#'
#' l_ruts_info <- lapply(rut_list, rut_informacion)
#'
#' l_ruts_info |> str(2)

rut_informacion <- function(.rut) {
  req_info_persona <- httr2::request(
    'https://servicios-prd.mercadopublico.cl'
  ) |>
    httr2::req_url_path('/v1/registro-civil/informacionPersona') |>
    httr2::req_headers(
      'Accept' = 'application/json',
      'Content-Type' = 'application/json'
    ) |>
    httr2::req_body_json(list(rut = .rut)) |>
    httr2::req_error(is_error = \(resp) FALSE)

  cat("Informacion sobre: ", .rut, '\n')

  resp_info_persona <- req_info_persona |>
    httr2::req_perform()

  resp_info_persona |>
    httr2::resp_body_json(simplifyVector = TRUE)
}
