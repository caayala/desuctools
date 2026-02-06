#' @title Clasificaci\u00f3n NSE AIM 2023
#'
#' @description
#'
#' Clasifica el nivel socioecon\u00f3mico (GSE) segun la
#' actualizaci\u00f3n AIM 2023, usando:
#' - edu_jh: educaci\u00f3n jefe(a) de hogar (c\u00f3digos edu_jh: 1-5)
#' - ocu_jh: ocupaci\u00f3n jefe(a) de hogar (c\u00f3digos ocu_jh: 1-6)
#' - residentes: numero de personas residentes en el hogar (>=1)
#' - tramo_ingreso: Tramos de ingreso aut\u00f3nomo per c\u00e1pita del hogar (1-7, 99 = NS/NR)
#'
#' Devuelve un vector de caracteres con el GSE: E, D, C3, C2, C1b, C1a, AB.
#'
#' Preguntas:
#'
#' Educaci\u00f3n
#'
#' `edu_jh`: \u00bfCu\u00e1l es el nivel educaci\u00f3nal alcanzado (\u00faltimo a\u00f1o aprobado) por el principal sostenedor del hogar?
#' 1 a 1 Sin estudios formales
#' 2 a 1 B\u00e1sica incompleta / primaria o preparatoria incompleta
#' 3 a 2 B\u00e1sica completa / primaria o preparatoria completa
#' 4 a 3 Media cientifico humanista o media tecnico profesional incompleta / humanidades incompleta
#' 5 a 3 Media cientifico humanista o media tecnico profesional completa / humanidades completa
#' 6 a 4 Instituto tecnico (CFT) o instituto profesional incompleto (carreras 1 a 3 a\u00f1os)
#' 7 a 4 Instituto tecnico (CFT) o instituto profesional completo (carreras 1 a 3 a\u00f1os) / hasta suboficial de FF.AA./Carabineros
#' 8 a 4 Universitaria incompleta (carreras de 4 o mas a\u00f1os)
#' 9 a 5 Universitaria completa (carreras de 4 o mas a\u00f1os) / oficial de FF.AA./Carabineros
#' 10 a 5 Postgrado (Postitulo, Master, Magister, Doctor)
#'
#' `ocu_jh` \u00bfCu\u00e1l de las siguientes ocupaci\u00f3nes corresponde al trabajo del principal sostenedor del hogar?
#' (SI EL PRINCIPAL SOSTENEDOR DEL HOGAR ESTA CESANTE O ES JUBILADO, PREGUNTAR POR LA ULTIMA OCUPACION REMUNERADA QUE TUVO)
#' (SI EL PRINCIPAL SOSTENEDOR TIENE MAS DE 1 TRABAJO, DEBE REGISTRARSE EL DE MAYOR INGRESO)
#' 1 a 1 Trabajadores no calificados en ventas y servicios, peones agropecuarios, forestales, construccion, etc.
#' 2 a 2 Obreros, operarios y artesa\u00f1os de artes mec\u00e1nicas y de otros oficios
#' 3 a 3 Trabajadores de los servicios y vendedores de comercio y mercados
#' 4 a 3 Agricultores y trabajadores calificados agropecuarios y pesqueros
#' 5 a 3 Operadores de instalaciones y m\u00e1quinas y montadores / conductores de veh\u00edculos
#' 6 a 3 Otros grupos no identificados (incluye rentistas, incapacitados, etc.)
#' 7 a 4 Empleados de oficina p\u00fablicos y privados
#' 8 a 5 Tecnicos y profesionales de nivel medio (incluye hasta suboficiales FF.AA./Carabineros)
#'    Alto ejecutivo (gerente general o gerente de area o sector) de empresa privadas o p\u00fablicas. Director o due\u00f1o de
#'    grandes empresas. Alto directivo del poder ejecutivo, de los cuerpos legislativos y de la administracion publica
#'    (incluye oficiales de FF.AA./carabineros)
#' 9 a 6 Profesionales, cient\u00edficos e intelectuales
#'
#' @source https://aimchile.cl/wp-content/uploads/2025/06/Actualizacion-y-Manual-GSE-AIM-2023.pdf
#'
#' @param .edu_jh Integer. Vector de c\u00f3digos de educaci\u00f3n (P1 AIM: 1-5).
#' @param .ocu_jh Integer. Vector de c\u00f3digos de ocupaci\u00f3n (P2 AIM: 1-6).
#' @param .tramo_ingreso Integer. Vector de tramo de ingreso (1-7, 99 = NS/NR).
#' @param factor logical. Si es `TRUE`, retorna factor ordenado; si es `FALSE`, retorna labelled.
#'
#' @return labelled. Vector con la clasificaci\u00f3n GSE.
#'
#' @examples
#' # personas <- data.frame(
#' #   edu_jh        = c(3L, 5L, 2L, 4L),
#' #   ocu_jh        = c(1L, 6L, 4L, 3L),
#' #   residentes    = c(4L, 5L, 2L, 9L),
#' #   tramo_ingreso = c(4L, 7L, 3L, 2L)
#' # )
#' # personas$gse <- rec_nse_aim2023(
#' #   edu_jh        = personas$edu_jh,
#' #   ocu_jh        = personas$ocu_jh,
#' #   residentes    = personas$residentes,
#' #   tramo_ingreso = personas$tramo_ingreso
#' # )
#' @export
rec_nse_aim2023 <- function(.edu_jh, .ocu_jh, .tramo_ingreso, factor = FALSE) {
  ## --------------------------------------------------------------
  ## 0. Chequeos b\u00e1sicos
  ## --------------------------------------------------------------
  n <- length(.edu_jh)
  if (
    !all(
      length(.ocu_jh) == n,
      length(.tramo_ingreso) == n
    )
  ) {
    stop("Todas las variables deben tener la misma longitud.", call. = FALSE)
  }

  if (any(!is.na(.edu_jh) & !.edu_jh %in% c(1:10, 99))) {
    stop(
      "Hay c\u00f3digos de '.edu_jh' fuera del rango 1-10, 99 (AIM).",
      call. = FALSE
    )
  }
  if (any(!is.na(.ocu_jh) & !.ocu_jh %in% c(1:9, 99))) {
    stop(
      "Hay c\u00f3digos de '.ocu_jh' fuera del rango 1-9, 99 (AIM).",
      call. = FALSE
    )
  }
  if (any(!is.na(.tramo_ingreso) & !.tramo_ingreso %in% c(1:7, 99))) {
    stop(
      "Hay c\u00f3digos de '.tramo_ingreso' fuera del rango 1-7, 99.",
      call. = FALSE
    )
  }

  ## Recodificar educaci\u00f3n
  map_edu <- c(1, 1, 2, 3, 3, 4, 4, 4, 5, 5)
  edu_jh_rec <- map_edu[as.integer(.edu_jh)]

  # print(edu_jh_rec)

  ## Recodificar ocupaci\u00f3n
  ## Recodificar educaci\u00f3n
  map_ocu <- c(1, 2, 3, 3, 3, 3, 4, 5, 6, 6)
  ocu_jh_rec <- map_ocu[as.integer(.ocu_jh)]

  # print(ocu_jh_rec)

  ## Normalizar ingreso
  tramo_clean <- ifelse(.tramo_ingreso == 99L, NA_integer_, .tramo_ingreso)

  ## --------------------------------------------------------------
  ## 1. Tabla YPCE: cruce residentes (1-8) x tramo_ingreso (1-7)
  ##    Aqu\u00ed se asume YPCE = tramo_ingreso (por tramo equivalente).
  ## --------------------------------------------------------------

  tabla_ypce <- expand.grid(
    residentes = 1:8,
    tramo_ingreso = 1:7
  )
  tabla_ypce$ypce <- tabla_ypce$tramo_ingreso

  ## --------------------------------------------------------------
  ## 2. Tabla GSE AIM 2023 (c\u00f3digos num\u00e9ricos P1/P2) en formato ancho
  ## --------------------------------------------------------------
  # fmt: skip
  tabla_gse_wide <- data.frame(
    edu_psh = c(
      1L, 2L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 3L, 2L, 4L, 1L, 3L, 2L, 4L, 4L, 3L,
      5L, 1L, 4L, 2L, 5L, 5L, 3L, 4L, 5L, 4L, 5L, 5L
    ),
    ocu_psh = c(
      1L, 1L, 2L, 2L, 1L, 3L, 3L, 2L, 4L, 3L, 4L, 1L, 5L, 4L, 5L, 2L, 3L, 5L,
      1L, 6L, 4L, 6L, 2L, 3L, 6L, 5L, 4L, 6L, 5L, 6L
    ),
    YPCE1 = c(
      "E","E","E","E","E","E","E","E","E","E","E","E","E","E","E","D","D","D",
      "D","D","D","D","D","D","D","D","D","D","D","C3"
    ),
    YPCE2 = c(
      "E","E","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D",
      "D","D","D","D","D","C3","C3","C3","C3","C3","C3","C3"
    ),
    YPCE3 = c(
      "D","D","D","D","D","D","D","D","D","C3","C3","C3","C3","C3","C3","C3","C3","C3",
      "C3","C3","C3","C3","C3","C3","C3","C3","C2","C2","C2","C2"
    ),
    YPCE4 = c(
      "C3","C3","C3","C3","C3","C3","C3","C3","C3","C3","C3","C3","C3","C3","C3","C3","C2","C2",
      "C2","C2","C2","C2","C2","C2","C2","C2","C2","C2","C2","C1b"
    ),
    YPCE5 = c(
      "C3","C3","C3","C2","C2","C2","C2","C2","C2","C2","C2","C2","C2","C2","C2","C2","C2","C2",
      "C2","C2","C2","C1b","C1b","C1b","C1b","C1b","C1b","C1b","C1b","C1a"
    ),
    YPCE6 = c(
      "C2","C2","C2","C2","C2","C2","C2","C2","C2","C2","C2","C2","C2","C2","C1b","C1b","C1b","C1b",
      "C1b","C1b","C1b","C1b","C1b","C1b","C1b","C1b","C1b","C1a","C1a","C1a"
    ),
    YPCE7 = c(
      "C2","C2","C2","C2","C2","C2","C2","C2","C2","C1b","C1b","C1b","C1b","C1b","C1b","C1b","C1b","C1b",
      "C1b","C1b","C1a","C1b","C1b","C1b","C1a","C1a","C1a","C1a","C1a","AB"
    ),
    stringsAsFactors = FALSE
  )

  ## --------------------------------------------------------------
  ## 3. Pasar tabla GSE a formato largo (edu_psh, ocu_psh, ypce, gse)
  ## --------------------------------------------------------------
  tabla_gse <- stats::reshape(
    tabla_gse_wide,
    varying = paste0("YPCE", 1:7),
    v.names = "gse",
    timevar = "ypce",
    times = 1:7,
    direction = "long"
  )
  # reshape crea fila/columna id; ordenamos y limpiamos
  tabla_gse <- tabla_gse[order(tabla_gse$id, tabla_gse$ypce), ]
  rownames(tabla_gse) <- NULL
  tabla_gse$id <- NULL

  ## --------------------------------------------------------------
  ## 4. Construir data.frame de entrada + merges
  ## --------------------------------------------------------------
  df <- data.frame(
    id = seq_len(n),
    edu_psh = as.integer(edu_jh_rec),
    ocu_psh = as.integer(ocu_jh_rec),
    ypce = as.integer(tramo_clean),
    stringsAsFactors = FALSE
  )

  # 1) Merge con tabla_gse para obtener gse
  df <- merge(
    df,
    tabla_gse,
    by = c("edu_psh", "ocu_psh", "ypce"),
    all.x = TRUE,
    sort = FALSE
  )

  ## -------------------------------
  ##  CONVERSI\u00d3N SEG\u00daN factor =
  ## -------------------------------
  niveles <- c("E", "D", "C3", "C2", "C1b", "C1a", "AB")

  gse <- factor(df$gse, levels = niveles, ordered = TRUE)

  if (isTRUE(factor)) {
    # Devolver como factor ordenado
    return(gse)
  }

  # Devolver como labelled (caracter con etiquetas)
  return(
    labelled::labelled(
      as.integer(gse),
      labels = stats::setNames(seq_along(niveles), niveles)
    )
  )
}
