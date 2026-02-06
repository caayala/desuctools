# ---- Core helpers ----
.moefpc <- function(N = NULL, n) {
  if (missing(n)) {
    stop("Debes entregar n.")
  }

  # Si no entregas N o es NULL -> todo sin FPC
  if (missing(N) || is.null(N)) {
    return(rep(1, length(n)))
  }

  N <- as.numeric(N)
  n <- as.numeric(n)

  # Reciclar longitudes al máximo
  L <- max(length(N), length(n))
  N <- rep(N, length.out = L)
  n <- rep(n, length.out = L)

  out <- rep(NA_real_, L)

  # Casos "sin ajuste": NA o Inf
  na_inf <- is.na(N) | is.infinite(N)
  out[na_inf] <- 1

  # N <= 1 -> 1 (degenerado, tratamos como sin FPC)
  smallN <- !na_inf & (N <= 1)
  out[smallN] <- 1

  # Válidos para calcular
  ok <- !na_inf & (N > 1)

  # Pares inválidos N < n -> NA y warning
  bad_pair <- ok & (N < n)
  if (any(bad_pair)) {
    warning("Hay posiciones con N < n; se devuelve NA all\u00ed.")
  }

  ok2 <- ok & !bad_pair
  out[ok2] <- sqrt((N[ok2] - n[ok2]) / (N[ok2] - 1))

  return(out)
}

# .moefpc(N = c(NA, NA, 1000, 500), n = c(400, 400, 400, 400))

.zcrit <- function(conf = 0.95) stats::qnorm(1 - (1 - conf) / 2)

# ---- MOE for a mean ----
# Options:
#  - provide x (vector) OR (sd and n)
#  - N optional for FPC; deff optional (default 1)
#  - use_t = TRUE uses t_{df=n-1} when sd is sample-based
moe_mean <- function(
  x = NULL,
  sd = NULL,
  n = NULL,
  N = NULL,
  conf = 0.95,
  deff = 1,
  use_t = TRUE
) {
  if (!is.null(x)) {
    x <- as.numeric(x)
    n <- sum(!is.na(x))
    sd <- stats::sd(x, na.rm = TRUE)
  }
  if (is.null(sd) || is.null(n)) {
    stop("Provide x OR sd and n.")
  }
  if (n < 2 && use_t) {
    warning("n < 2: switching to normal critical value.")
  }
  alpha <- 1 - conf
  crit <- if (use_t && n > 1) {
    stats::qt(1 - alpha / 2, df = n - 1)
  } else {
    .zcrit(conf)
  }
  fpc <- .moefpc(N, n)
  se <- (sd / sqrt(n)) * sqrt(deff)
  crit * se * fpc
}

# ---- MOE for a proportion (Wald) ----
# Options:
#  - provide (x and n) OR (p and n). If p missing, defaults to 0.5 (peor caso)
#  - N optional for FPC; deff optional (default 1)
moe_prop <- function(
  p = NULL,
  x = NULL,
  n = NULL,
  N = NULL,
  conf = 0.95,
  deff = 1
) {
  if (!is.null(x) && !is.null(n)) {
    p <- x / n
  }
  if (is.null(p)) {
    if (is.null(n)) {
      stop("Provide n and (x or p).")
    }
    p <- 0.5 # worst-case when p unknown
  }
  if (is.null(n)) {
    stop("Provide n and (x or p).")
  }
  z <- .zcrit(conf)
  fpc <- .moefpc(N, n)
  se <- sqrt(p * (1 - p) / n) * sqrt(deff)
  z * se * fpc
}

# ---- Simple dispatcher (optional) ----
moe <- function(type = c("mean", "prop"), ...) {
  type <- match.arg(type)
  switch(type, mean = moe_mean(...), prop = moe_prop(...))
}
