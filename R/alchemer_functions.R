# Funciones para el trabajo con Alchemer
# Servicio de encuestas web
# https://app.alchemer.com/

#' @title read Alchemer SPSS export
#'
#' @description
#' Lee el archivo .sav a partir del _distribution link_ de un reporte de
#' exportación de una encuesta programada en Alchemer.
#' https://help.alchemer.com/help/spss
#'
#' Compatibility
#' - Comments are not available in SPSS exports.
#' - The Conjoint question is not available in SPSS exports.
#' - "Other, Write-In" rows in grid questions as not available in SPSS exports.
#' - The Time Started field is not includes as part of the SPSS export.
#'
#'
#' @param url `chr` Distribution link del reporte de base de datos en SPSS.
#'
#' @return tibble
#'
#' @importFrom haven read_sav
#'
#' @export
#'
alch_read_spss <- function(url) {
  temp_zip <- tempfile(fileext = ".zip") # Descargo archivo zip

  download.file(url, destfile = temp_zip)

  temp_dir <- tempdir() # Descomprimo archivo descargado

  unzip(temp_zip, exdir = temp_dir)

  haven::read_sav(file.path(temp_dir, "spss.sav"))
}


#' @title Obtener respuestas de una encuesta de Alchemer
#'
#' @description
#' Esta función recupera las respuestas de una encuesta específica de Alchemer.
#' Permite descargar todas las respuestas o una página específica de resultados.
#'
#' @param api_token `chr` Clave de API de Alchemer. Se recomienda usar variables de entorno: Sys.getenv("XYZ")
#' @param api_token_secret `chr` Secreto de API de Alchemer. Se recomienda usar variables de entorno: Sys.getenv("XYZ")
#' @param survey_id `int` ID de la encuesta de la que deseas obtener las respuestas.
#' @param results_per_page `int` Cantidad de respuestas por página. El máximo y valor por defecto es 500.
#' @param page `int` o `chr`. El número de la página a descargar. Usar "all" (valor por defecto) para descargar todas las páginas.
#'
#' @return Una `list` que contiene la respuesta parseada de la API. Si `page = "all"`,
#' el elemento `data` de la lista contendrá las respuestas de todas las páginas.
#'
#' @importFrom httr2 request req_url_path_append req_url_query req_perform resp_body_json
#' @importFrom purrr map
#' @export
alch_get_survey_responses <- function(
  api_token = Sys.getenv("ALCH_API"),
  api_token_secret = Sys.getenv("ALCH_API_SECRET_KEY"),
  survey_id,
  results_per_page = 500,
  page = "all"
) {
  # Validaciones
  # 1. Credenciales de API
  if (api_token == "" || api_token_secret == "") {
    stop(
      "Error: Las credenciales 'api_token' y 'api_token_secret' no pueden estar vacías. Revisa tus variables de entorno."
    )
  }

  # 2. Parámetro 'survey_id'
  if (!is.numeric(survey_id) || length(survey_id) != 1) {
    stop("Error: 'survey_id' debe ser un único valor numérico.")
  }

  # 3. Parámetro 'results_per_page'
  if (
    !is.numeric(results_per_page) ||
      results_per_page <= 0 ||
      results_per_page > 500
  ) {
    stop("Error: 'results_per_page' debe ser un número entero entre 1 y 500.")
  }

  # 4. Parámetro 'page'
  if (
    !identical(page, "all") &&
      (!is.numeric(page) || length(page) != 1 || page <= 0)
  ) {
    stop(
      "Error: 'page' debe ser la cadena \"all\" o un número entero positivo."
    )
  }

  # Base URL para Alchemer API v5
  base_url <- "https://api.alchemer.com/v5"

  # Función interna para obtener una página específica
  get_page <- function(
    page_num,
    .api_token = api_token,
    .api_token_secret = api_token_secret
  ) {
    req <- httr2::request(base_url) |>
      httr2::req_url_path_append("survey", survey_id, "surveyresponse") |>
      httr2::req_url_query(
        resultsperpage = results_per_page,
        page = page_num,
        api_token = .api_token,
        api_token_secret = .api_token_secret
      )

    resp <- httr2::req_perform(req) |>
      httr2::resp_body_json()

    return(resp)
  }

  # Si se solicita una página específica, se devuelve la respuesta completa
  if (is.numeric(page)) {
    resp_data <- get_page(page)
    return(resp_data)
  }

  # Si se solicitan todas las páginas
  if (page == "all") {
    # Request inicial para obtener la primera página y metadatos
    resp_page1 <- get_page(1)

    # Si no hay datos, devolver la respuesta de la primera página tal cual
    if (resp_page1$total_count == 0) {
      return(resp_page1)
    }

    # Extraemos los datos la pagina 1
    all_data <- resp_page1$data

    # Recuperar la cantidad de paginas existentes segun la configuracion
    total_pages <- resp_page1$total_pages

    # Si hay más páginas, iterar y obtener los datos restantes
    if (total_pages > 1) {
      # Usar purrr::map para iterar desde la página 2 hasta el final
      remaining_pages_resp <- purrr::map(2:total_pages, get_page)

      # Extraer solo la data de cada respuesta
      remaining_pages_data <- unlist(
        purrr::map(remaining_pages_resp, "data"),
        recursive = FALSE
      )

      # Combinar los datos de todas las páginas
      all_data <- c(all_data, remaining_pages_data)
    }

    # Reemplazar el elemento 'data' en la respuesta original con todos los datos
    resp_page1$data <- all_data
    # Actualizar metadatos para reflejar que se han combinado las páginas
    resp_page1$page <- "all"
    resp_page1$results_per_page <- resp_page1$total_count

    return(resp_page1)
  }
}

# Ejemplo para obtener solo el primer caso de la primera página:
test <- alch_get_survey_responses(
  survey_id = 8487186,
  results_per_page = 4,
  page = 1
)

test$result_ok
test$page


tibble::tibble(data = test$data) |>
  unnest_wider(data) |>
  select(survey_data) |>
  mutate(
    survey_data =
  )
print(n = Inf)

tibble::tibble(responses = test$data) %>%
  tidyr::unnest_wider(responses) %>% # crea columna survey_data
  #select(survey_data) %>%
  mutate(respondent = row_number()) %>%
  mutate(
    variable = purrr::map_chr(
      survey_data,
      ~ if (is.null(.x$id)) NA_character_ else as.character(.x$id)
    ),
    etiqueta = purrr::map_chr(
      survey_data,
      ~ if (is.null(.x$question)) NA_character_ else as.character(.x$question)
    ),
    respuesta = purrr::map_chr(
      survey_data,
      ~ if (is.null(.x$answer)) NA_character_ else as.character(.x$answer)
    )
  ) %>%
  select(variable, etiqueta, respuesta)
