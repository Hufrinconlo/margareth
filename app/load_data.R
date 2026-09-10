# ---------------------------------------------------------------------------
# app/load_data.R
#
# The app NEVER scrapes. It reads the artifact the weekly ETL publishes to
# the 'data' branch. Sourced by app.R at startup.
# ---------------------------------------------------------------------------

library(dplyr)

DATA_BASE <- "https://raw.githubusercontent.com/Hufrinconlo/margareth/data/"

# Download once per session into a temp file, then read.
#
# The artifact is legitimately absent between a reset of the 'data' branch and
# the next green ETL, and download.file() signals that by throwing. Raise a
# message the dashboard can show instead, so a missing file degrades to a
# notice rather than taking every panel down with a raw R error.
fetch_artifact <- function(file, reader) {
  dest <- tempfile(fileext = paste0("_", file))
  ok <- tryCatch({
    utils::download.file(paste0(DATA_BASE, file), dest, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)

  if (!ok || !file.exists(dest) || file.size(dest) == 0) {
    stop("No hay datos publicados todavía (no se pudo descargar ", file,
         "). El ETL semanal publica el artefacto en la rama 'data'; ",
         "si acaba de reiniciarse, espere a la próxima ejecución.",
         call. = FALSE)
  }
  reader(dest)
}

load_margaret <- function() {
  raw <- fetch_artifact("margaret.rds", readRDS)

  # getting_data() returns: [[1]] group summary, [[2]] researcher-level
  # production, [3..n] one data frame per product type.
  list(
    groups      = raw[[1]],
    researchers = raw[[2]],
    products    = raw[-c(1, 2)]
  )
}

load_health <- function() {
  fetch_artifact("run_health.csv",
                 function(p) utils::read.csv(p, stringsAsFactors = FALSE))
}

# Which product sheets actually have rows? Most groups fill only a handful
# of the ~48 categories, so this drives what's worth showing.
product_summary <- function(products) {
  tibble::tibble(
    tipo = names(products),
    filas = vapply(products, function(d) {
      if (is.data.frame(d)) nrow(d) else 0L
    }, integer(1))
  ) |>
    filter(filas > 0) |>
    arrange(desc(filas))
}
