# ---------------------------------------------------------------------------
# Are the 40 "CvLAC oculto" genuinely private, or a second silent bug?
#
# safely() turns every failure into the same label. This re-runs the parse
# across all researchers, keeps the error messages, and probes the failing
# pages to tell "restricted by the owner" apart from "we can't parse it".
#
# Run from the repo root: Rscript diagnostics/40_audit_hidden.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(rvest); library(httr); library(dplyr); library(purrr)
  library(stringr); library(tibble); library(margaret)
})

# patch_all(), not just the add_rownames half: the parser patch is what turns a
# blank academic section into "Sin información". Auditing without it counts
# those as failures and reports a hidden rate the ETL never actually produces.
source("etl/patch_margaret.R"); patch_all()

groups <- readr::read_csv("groups.csv", show_col_types = FALSE)

# --- collect every CvLAC url across all groups -----------------------------
all_urls <- map(groups$url, function(u) {
  h <- read_html(GET(u)) |> html_nodes("a") |> html_attr("href")
  h[grepl("cvlac", h, ignore.case = TRUE)]
}) |> unlist() |> unique()

cat("distinct CvLAC urls:", length(all_urls), "\n\n")

# --- run the real parser, keeping errors -----------------------------------
f   <- getFromNamespace("get_posgrade_clasficitation_cvlac", "margaret")
res <- map(all_urls, safely(f))

ok   <- map_lgl(res, ~ is.null(.x$error))
errs <- map_chr(res[!ok], ~ conditionMessage(.x$error))

cat("parsed OK :", sum(ok), "\n")
cat("failed    :", sum(!ok), "\n\n")

cat("--- error messages by frequency ---\n")
print(sort(table(str_trunc(errs, 90)), decreasing = TRUE))

# --- probe the failing pages -----------------------------------------------
# A genuinely restricted CvLAC still returns 200 but carries a notice and
# very little content. A parse bug returns a full page we simply mishandle.
fail_urls <- all_urls[!ok]

cat("\n--- probing up to 8 failing pages ---\n")
probe <- map_dfr(head(fail_urls, 8), function(u) {
  r  <- GET(u, user_agent("margaret-etl"), timeout(60))
  bd <- length(content(r, "raw"))
  tx <- tryCatch(html_text(read_html(r)), error = function(e) "")
  nt <- tryCatch(length(html_table(read_html(r))), error = function(e) NA_integer_)
  tibble(
    cod_rh    = str_extract(u, "(?<=cod_rh=)\\d+"),
    status    = status_code(r),
    bytes     = bd,
    tables    = nt,
    has_form  = grepl("Formaci\u00f3n Acad\u00e9mica", tx, fixed = TRUE),
    restricted = grepl("no ha autorizado|no autoriz|privacidad|no se encuentra|no disponible",
                       tx, ignore.case = TRUE)
  )
})
print(as.data.frame(probe))

cat("\n--- reading ---\n")
cat("bytes ~150k + has_form TRUE  -> full page, PARSE BUG (fixable)\n")
cat("small bytes + restricted TRUE -> genuinely private, 30% is real\n")

# --- compare against a known-good page for contrast ------------------------
good <- all_urls[ok][1]
rg   <- GET(good, user_agent("margaret-etl"), timeout(60))
cat("\nreference (parses fine):", str_extract(good, "(?<=cod_rh=)\\d+"),
    "| bytes:", length(content(rg, "raw")), "\n")
