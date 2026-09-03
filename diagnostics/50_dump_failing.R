# ---------------------------------------------------------------------------
# The 47 failures aren't private and aren't parse bugs in the usual sense:
# "Formacion Academica" is simply absent. Two size classes (~25KB and ~3.5KB)
# suggest two different situations. Read them.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(rvest); library(httr); library(stringr)
})

base <- "https://scienti.minciencias.gov.co/cvlac/visualizador/generarCurriculoCv.do?cod_rh="

# one from each size class seen in the audit
targets <- c("0002184217", "0001373805")

for (cod in targets) {
  u <- paste0(base, cod)
  cat("\n", strrep("=", 70), "\ncod_rh:", cod, "\n", sep = "")

  r  <- GET(u, user_agent("margaret-etl"), timeout(60))
  pg <- read_html(r)
  tx <- html_text(pg)
  tx <- str_squish(tx)

  cat("bytes:", length(content(r, "raw")), "\n")
  cat("tables:", length(html_table(pg)), "\n\n")

  cat("--- visible text (first 1200 chars) ---\n")
  cat(str_trunc(tx, 1200), "\n\n")

  cat("--- section headings present ---\n")
  for (s in c("Formaci\u00f3n Acad\u00e9mica", "Formaci\u00f3n Complementaria",
              "Experiencia profesional", "\u00c1reas de actuaci\u00f3n",
              "Hoja de vida", "Nombre")) {
    cat(sprintf("  %-28s %s\n", s, grepl(s, tx, fixed = TRUE)))
  }

  cat("\n--- table shapes ---\n")
  for (i in seq_along(html_table(pg))) {
    t <- html_table(pg)[[i]]
    cat(sprintf("  [%d] %d x %d\n", i, nrow(t), ncol(t)))
  }
}
