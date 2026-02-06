# Funci\u00f3nes para el trabajo con Alchemer
# Servicio de encuestas web
# https://app.alchemer.com/

#' @title read Alchemer SPSS export
#'
#' @description
#' Lee el archivo .sav a partir del _distribution link_ de un reporte de
#' exportacion de una encuesta programada en Alchemer.
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

  download.file(url, destfile = temp_zip, mode = "wb")

  temp_dir <- tempdir() # Descomprimo archivo descargado

  utils::unzip(temp_zip, exdir = temp_dir)

  haven::read_sav(file.path(temp_dir, "spss.sav"))
}


#' @title Obtener respuestas de una encuesta de Alchemer
#' @description
#' Recupera las respuestas de una encuesta en Alchemer. Permite descargar una
#' p\u00e1gina concreta de resultados o todas las p\u00e1ginas (combinadas) cuando
#' \code{page = "all"}. La funci\u00f3n valida par\u00e1metros y devuelve el objeto
#' parseado por la API (lista con metadatos y elemento \code{data} con las
#' respuestas).
#'
#' @param api_token `chr` Clave publica de API de Alchemer. Por defecto toma
#'   \code{Sys.getenv("ALCHEMER_API_KEY")}.
#' @param api_token_secret `chr` Clave secreta de API. Por defecto toma
#'   \code{Sys.getenv("ALCHEMER_API_SECRET")}.
#' @param survey_id `int` ID de la encuesta de la que se desean obtener las respuestas.
#' @param results_per_page `int` Numero de respuestas por p\u00e1gina (1..500). Valor por
#'   defecto: 500.
#' @param page `int` o \code{"all"}. Numero de p\u00e1gina a descargar, o
#'   \code{"all"} para recuperar y combinar todas las p\u00e1ginas.
#'
#' @return `list`. Objeto devuelto por la API (parseado a lista). Si
#'   \code{page = "all"} el elemento \code{data} contendra las respuestas de
#'   todas las p\u00e1ginas combinadas.
#'
#' @details
#' - Valida que las credenciales y par\u00e1metros sean correctos antes de llamar a la API.
#' - Cuando \code{page = "all"} hace multiples llamadas (si procede) y concatena
#'   todos los elementos \code{data} en la respuesta retornada.
#'
#' @examples
#' \dontrun{
#' # definir credenciales (mejor usar variables de entorno en tu sistema)
#' Sys.setenv(ALCHEMER_API_KEY = "tu_api_token")
#' Sys.setenv(ALCHEMER_API_SECRET = "tu_api_secret")
#'
#' # obtener la primera p\u00e1gina
#' resp_page1 <- alch_get_survey_responses(survey_id = 123456, page = 1)
#' str(resp_page1)
#'
#' # obtener todas las p\u00e1ginas combinadas
#' resp_all <- alch_get_survey_responses(survey_id = 123456, page = "all")
#' length(resp_all$data)    # numero total de respuestas descargadas
#' }
#'
#' @importFrom httr2 request req_url_path_append req_url_query req_perform resp_body_json
#' @importFrom purrr map
#' @export
alch_get_survey_responses <- function(
  api_token = Sys.getenv("ALCHEMER_API_KEY"),
  api_token_secret = Sys.getenv("ALCHEMER_API_SECRET"),
  survey_id,
  results_per_page = 500,
  page = "all"
) {
  # Validaciones
  # 1. Credenciales de API
  if (api_token == "" || api_token_secret == "") {
    stop(
      "Error: Las credenciales 'api_token' y 'api_token_secret' no pueden estar vac\u00edas. Revisa tus variables de entorno."
    )
  }

  # 2. Parametro 'survey_id'
  if (!is.numeric(survey_id) || length(survey_id) != 1) {
    stop("Error: 'survey_id' debe ser un unico valor num\u00e9rico.")
  }

  # 3. Parametro 'results_per_page'
  if (
    !is.numeric(results_per_page) ||
      results_per_page <= 0 ||
      results_per_page > 500
  ) {
    stop("Error: 'results_per_page' debe ser un numero entero entre 1 y 500.")
  }

  # 4. Parametro 'page'
  if (
    !identical(page, "all") &&
      (!is.numeric(page) || length(page) != 1 || page <= 0)
  ) {
    stop(
      "Error: 'page' debe ser la cadena \"all\" o un numero entero positivo."
    )
  }

  # Base URL para Alchemer API v5
  base_url <- "https://api.alchemer.com/v5"

  # Funci\u00f3n interna para obtener una p\u00e1gina especifica
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

  # Si se solicita una p\u00e1gina especifica, se devuelve la respuesta completa
  if (is.numeric(page)) {
    resp_data <- get_page(page)
    return(resp_data)
  }

  # Si se solicitan todas las p\u00e1ginas
  if (page == "all") {
    # Request inicial para obtener la primera p\u00e1gina y metadatos
    resp_page1 <- get_page(1)

    # Si no hay datos, devolver la respuesta de la primera p\u00e1gina tal cual
    if (resp_page1[["total_count"]] == 0) {
      return(resp_page1)
    }

    # Extraemos los datos de la p\u00e1gina 1
    all_data <- resp_page1[["data"]]

    # Recuperar la cantidad de p\u00e1ginas existentes segun la configuracion
    total_pages <- resp_page1[["total_pages"]]

    # Si hay mas p\u00e1ginas, iterar y obtener los datos restantes
    if (total_pages > 1) {
      # Usar purrr::map para iterar desde la p\u00e1gina 2 hasta el final
      remaining_pages_resp <- purrr::map(2:total_pages, get_page)

      # Extraer solo la data de cada respuesta
      remaining_pages_data <- unlist(
        purrr::map(remaining_pages_resp, "data"),
        recursive = FALSE
      )

      # Combinar los datos de todas las p\u00e1ginas
      all_data <- c(all_data, remaining_pages_data)
    }

    # Reemplazar el elemento 'data' en la respuesta original con todos los datos
    resp_page1[["data"]] <- all_data
    # Actualizar metadatos para reflejar que se han combinado las p\u00e1ginas
    resp_page1[["page"]] <- "all"
    resp_page1[["results_per_page"]] <- resp_page1[["total_count"]]

    return(resp_page1)
  }
}


#' @title Obtener contactos de una campa\u00f1a de encuesta en Alchemer
#' @description
#' Recupera los contactos asociados a una campa\u00f1a de encuesta en Alchemer.
#' Permite descargar una p\u00e1gina concreta de contactos o todas las p\u00e1ginas
#' (combinadas) cuando \code{page = "all"}. La funci\u00f3n valida par\u00e1metros y
#' devuelve el objeto parseado por la API (lista con metadatos y elemento
#' \code{data} con los contactos).
#'
#' @param api_token `chr` Clave publica de API de Alchemer. Por defecto toma
#'   \code{Sys.getenv("ALCHEMER_API_KEY")}.
#' @param api_token_secret `chr` Clave secreta de API. Por defecto toma
#'   \code{Sys.getenv("ALCHEMER_API_SECRET")}.
#' @param survey_id `int` ID de la encuesta a la que pertenece la campa\u00f1a.
#' @param campaign_id `int` ID de la campa\u00f1a de la que se desean obtener los contactos.
#' @param results_per_page `int` Numero de contactos por p\u00e1gina (1..500). Valor por
#'   defecto: 500.
#' @param page `int` o \code{"all"}. Numero de p\u00e1gina a descargar, o
#'   \code{"all"} para recuperar y combinar todas las p\u00e1ginas.
#'
#' @return `list`. Objeto devuelto por la API (parseado a lista). Si
#'   \code{page = "all"} el elemento \code{data} contendra los contactos de
#'   todas las p\u00e1ginas combinadas.
#'
#' @details
#' - Valida que las credenciales y par\u00e1metros sean correctos antes de llamar a la API.
#' - Cuando \code{page = "all"} hace multiples llamadas (si procede) y concatena
#'   todos los elementos \code{data} en la respuesta retornada.
#' - Los contactos pueden incluir campos como \code{id}, \code{email_address},
#'   \code{first_name}, \code{last_name}, etc., segun la configuracion de la campa\u00f1a.
#'
#' @examples
#' \dontrun{
#' # definir credenciales (mejor usar variables de entorno en tu sistema)
#' Sys.setenv(ALCHEMER_API_KEY = "tu_api_token")
#' Sys.setenv(ALCHEMER_API_SECRET = "tu_api_secret")
#'
#' # obtener la primera p\u00e1gina de contactos
#' contactos_page1 <- alch_get_survey_contacts(
#'   survey_id = 8529571,
#'   campaign_id = 24631558,
#'   page = 1
#' )
#' str(contactos_page1)
#'
#' # obtener todos los contactos combinados
#' contactos_all <- alch_get_survey_contacts(
#'   survey_id = 8529571,
#'   campaign_id = 24631558,
#'   page = "all"
#' )
#' length(contactos_all$data)    # numero total de contactos descargados
#' }
#'
#' @importFrom httr2 request req_url_path_append req_url_query req_perform resp_body_json
#' @importFrom purrr map
#' @export

alch_get_survey_contacts <- function(
  api_token = Sys.getenv("ALCHEMER_API_KEY"),
  api_token_secret = Sys.getenv("ALCHEMER_API_SECRET"),
  survey_id,
  campaign_id,
  results_per_page = 500,
  page = "all"
) {
  # Validaciones
  # 1. Credenciales de API
  if (api_token == "" || api_token_secret == "") {
    stop(
      "Error: Las credenciales 'api_token' y 'api_token_secret' no pueden estar vac\u00edas. Revisa tus variables de entorno."
    )
  }

  # 2. Parametro 'survey_id'
  if (!is.numeric(survey_id) || length(survey_id) != 1) {
    stop("Error: 'survey_id' debe ser un unico valor num\u00e9rico.")
  }

  # 3. Parametro 'campaign_id'
  if (!is.numeric(campaign_id) || length(campaign_id) != 1) {
    stop("Error: 'campaign_id' debe ser un unico valor num\u00e9rico.")
  }

  # 4. Parametro 'results_per_page'
  if (
    !is.numeric(results_per_page) ||
      results_per_page <= 0 ||
      results_per_page > 500
  ) {
    stop("Error: 'results_per_page' debe ser un numero entero entre 1 y 500.")
  }

  # 5. Parametro 'page'
  if (
    !identical(page, "all") &&
      (!is.numeric(page) || length(page) != 1 || page <= 0)
  ) {
    stop(
      "Error: 'page' debe ser la cadena \"all\" o un numero entero positivo."
    )
  }

  # Base URL para Alchemer API v5
  base_url <- "https://api.alchemer.com/v5"

  # Funci\u00f3n interna para obtener una p\u00e1gina especifica
  get_page <- function(
    page_num,
    .api_token = api_token,
    .api_token_secret = api_token_secret
  ) {
    req <- httr2::request(base_url) |>
      httr2::req_url_path_append(
        "survey",
        survey_id,
        "surveycampaign",
        campaign_id,
        "surveycontact"
      ) |>
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

  # Si se solicita una p\u00e1gina especifica
  if (is.numeric(page)) {
    resp_data <- get_page(page)
    return(resp_data)
  }

  # Si se solicitan todas las p\u00e1ginas
  if (page == "all") {
    resp_page1 <- get_page(1)

    if (resp_page1[["total_count"]] == 0) {
      return(resp_page1)
    }

    all_data <- resp_page1[["data"]]
    total_pages <- resp_page1[["total_pages"]]

    if (total_pages > 1) {
      remaining_pages_resp <- purrr::map(2:total_pages, get_page)
      remaining_pages_data <- unlist(
        purrr::map(remaining_pages_resp, "data"),
        recursive = FALSE
      )
      all_data <- c(all_data, remaining_pages_data)
    }

    resp_page1[["data"]] <- all_data
    resp_page1[["page"]] <- "all"
    resp_page1[["results_per_page"]] <- resp_page1[["total_count"]]

    return(resp_page1)
  }
}


#' @title Construye tibble de respuestas desde la lista de Alchemer
#' @description
#' Construye una tabla (tibble) a partir de la salida de la API de Alchemer
#' (objeto devuelto por \code{alch_get_survey_responses}). Extrae para cada
#' pregunta los campos \code{id}, \code{question} y \code{answer}, pivotea las
#' respuestas a formato ancho (una fila por respuesta) creando columnas
#' \code{var<id>} y asigna la etiqueta de variable (atributo \code{"label"})
#' con el texto de la pregunta (etiquetas HTML son limpiadas).
#'
#' @param ls_alchemer List. Objeto parseado de la API de Alchemer; debe contener
#'   el elemento \code{data} con las respuestas (lista de respuestas).
#'
#' @return Tibble. Una fila por respuesta de encuesta con las columnas de
#'   metadatos originales y columnas de respuesta llamadas \code{var<id>}.
#'   Cada columna de respuesta recibe el atributo \code{"label"} con la
#'   pregunta correspondiente (texto limpio).
#'
#' @details
#' - Si una pregunta no tiene \code{answer}, el valor resultante sera \code{NA}.
#' - Respuestas multiples por misma pregunta se colapsan en una cadena
#'   separada por \code{"; "}.
#' - La funci\u00f3n asume que \code{ls_alchemer[["data"]]} es una lista con la
#'   estructura estandar de la API (cada elemento contiene sublistas con
#'   elementos \code{id}, \code{question}, \code{answer} entre otros).
#'
#' @examples
#' \dontrun{
#' resp <- alch_get_survey_responses(survey_id = 8487186)
#' df <- alch_create_df(resp)
#' head(df)
#' # ver label de la columna var3
#' attr(df$var3, "label")
#' }
#'
#' @importFrom rlang .data
#' @importFrom purrr map
#' @importFrom tibble enframe
#' @importFrom tidyr unnest_wider pivot_wider
#' @importFrom dplyr bind_rows bind_cols select distinct mutate across starts_with
#' @export
alch_create_df <- function(ls_alchemer) {
  # Funci\u00f3n interna para extraer y construir la base de datos de
  # respuestas de Alchemer
  .procesar_encuestas <- function(lista_encuestas) {
    # Itera sobre cada respuesta de encuesta y combina los resultados en un tibble
    # El argumento .id = "resp_id" crea una columna con el indice de cada respuesta
    ls_resp <- purrr::map(lista_encuestas, function(x) {
      # Convierte la lista de preguntas de una respuesta en un tibble
      tibble::enframe(x, name = "pregunta_original_id", value = "data") |>
        # Modifica la columna de lista para quedarse solo con los elementos deseados
        dplyr::mutate(
          data = map(data, ~ .x[c("id", "question", "answer")])
        ) |>
        # Expande la columna de lista "data" en multiples columnas
        tidyr::unnest_wider(col = data, names_sep = "_")
    })

    # Funci\u00f3n para eliminar etiquetas HTML y normalizar espacios en una cadena
    .simple_strip_html_vec <- function(x) {
      x_chr <- as.character(x)
      entities <- c(
        "&nbsp;" = " ",
        "&amp;" = "&",
        "&lt;" = "<",
        "&gt;" = ">",
        "&quot;" = "\"",
        "&#39;" = "'"
      )
      vapply(
        x_chr,
        FUN.VALUE = character(1),
        FUN = function(txt) {
          if (is.na(txt)) {
            return(NA_character_)
          }
          # quitar tags
          s <- gsub("<[^>]+>", "", txt, perl = TRUE)
          # reemplazar entidades comunes
          for (k in names(entities)) {
            s <- gsub(k, entities[k], s, fixed = TRUE)
          }
          # normalizar espacios y recortar
          s <- gsub("\\s+", " ", s, perl = TRUE)
          trimws(s)
        },
        USE.NAMES = FALSE
      )
    }

    # Creo un diccionario con las preguntas y sus etiquetas limpias
    labels <- dplyr::bind_rows(ls_resp, .id = "resp_id") |>
      dplyr::distinct(.data[["data_id"]], .data[["data_question"]]) |>
      dplyr::mutate(
        data_id = paste0("var", .data[["data_id"]]),
        data_question = .simple_strip_html_vec(.data[["data_question"]])
      )

    # Creo un vector nombrado para renombrar las columnas
    labels_vec <- stats::setNames(
      as.character(labels[["data_question"]]),
      as.character(labels[["data_id"]])
    )

    rm(labels)

    # Combino todas las respuestas en un solo tibble
    df_wide <- purrr::map(ls_resp, function(x) {
      x |>
        dplyr::select(data_id, data_answer) |>
        tidyr::pivot_wider(
          id_cols = NULL,
          names_from = data_id,
          values_from = data_answer,
          names_prefix = "var"
        )
    }) |>
      dplyr::bind_rows()

    # Agregar etiquetas de variable
    for (nm in names(labels_vec)) {
      attr(df_wide[[nm]], "label") <- labels_vec[[nm]]
    }

    return(df_wide)
  }

  # Convertimos la lista general en un tibble
  df0 <- ls_alchemer[["data"]] |>
    tibble::enframe() |>
    tidyr::unnest_wider(value)

  # Procesamos survey_data ya que ahi estan todas las respuestas de
  # las encuestas
  df1 <- .procesar_encuestas(df0[["survey_data"]])

  # Combinamos la info general con las respuestas procesadas
  df_final <- df0 |>
    dplyr::select(!survey_data) |>
    dplyr::bind_cols(df1)

  # Retornamos la base final
  return(df_final)
}
