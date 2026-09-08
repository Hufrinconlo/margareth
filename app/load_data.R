# ---------------------------------------------------------------------------
# app/load_data.R
#
# The app NEVER scrapes. It reads the artifact the weekly ETL publishes to
# the 'data' branch. Sourced by app.R at startup.
# ---------------------------------------------------------------------------

library(dplyr)

DATA_BASE <- "https://raw.githubusercontent.com/Hufrinconlo/margareth/data/"

# Download once per session into a temp file, then read.
fetch_artifact <- function(file, reader) {
  dest <- file.path(tempdir(), file)
  if (!file.exists(dest)) {
    utils::download.file(paste0(DATA_BASE, file), dest,
                         mode = "wb", quiet = TRUE)
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
