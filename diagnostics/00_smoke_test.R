# ---------------------------------------------------------------------------
# margaret smoke test
#
# Purpose: find out whether margaret 0.1.4's hardcoded table positions still
# match current GrupLAC pages, BEFORE building any pipeline around it.
#
# Run this locally (not in CI) against 2-3 of your real group URLs.
#
# margaret's scraper assumes, for every GrupLAC page:
#   - html_table() returns AT LEAST 85 tables
#   - table [[1]]  = group header info, has a column named X1, >= 9 rows
#   - table [[5]]  = researcher roster, has columns X1..X4, > 2 rows
#   - tables [[14]]..[[85]] = one product category each
# If any of those are false, the package will error or silently return garbage.
# ---------------------------------------------------------------------------

library(rvest)
library(httr)
library(dplyr)

# --- EDIT ME --------------------------------------------------------------
urls <- c(
  "https://scienti.minciencias.gov.co/gruplac/jsp/visualiza/visualizagr.jsp?nro=00000000008562"
  # add a second URL here once the first one is working
)
# --------------------------------------------------------------------------

check_page <- function(url) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("URL: ", url, "\n", sep = "")

  resp <- tryCatch(
    GET(url, user_agent("Mozilla/5.0 (compatible; margaret-smoketest)"),
        timeout(60)),
    error = function(e) e
  )
  if (inherits(resp, "error")) {
    cat("FETCH FAILED: ", conditionMessage(resp), "\n", sep = "")
    return(invisible(NULL))
  }

  cat("HTTP status : ", status_code(resp), "\n", sep = "")
  cat("Content-Type: ", headers(resp)[["content-type"]], "\n", sep = "")
  cat("Body bytes  : ", length(content(resp, "raw")), "\n", sep = "")
  if (status_code(resp) != 200) {
    cat("Non-200 response, stopping here for this URL.\n")
    return(invisible(NULL))
  }

  page   <- read_html(resp)
  tables <- html_table(page)
  n      <- length(tables)

  cat("\nTables found: ", n, "\n", sep = "")
  cat("margaret needs >= 85 -> ",
      if (n >= 85) "OK" else "*** FAIL: product loop 14:85 will break ***",
      "\n", sep = "")

  # --- table 1: group header ---
  cat("\n--- table [[1]] (expects column X1, >= 9 rows) ---\n")
  t1 <- tables[[1]]
  cat("dim  : ", paste(dim(t1), collapse = " x "), "\n", sep = "")
  cat("names: ", paste(names(t1), collapse = " | "), "\n", sep = "")
  cat("has X1: ", "X1" %in% names(t1), " | >=9 rows: ", nrow(t1) >= 9, "\n", sep = "")
  print(head(as.data.frame(t1), 10))

  # --- table 5: researcher roster ---
  if (n >= 5) {
    cat("\n--- table [[5]] (expects X1..X4, > 2 rows) ---\n")
    t5 <- tables[[5]]
    cat("dim  : ", paste(dim(t5), collapse = " x "), "\n", sep = "")
    cat("names: ", paste(names(t5), collapse = " | "), "\n", sep = "")
    cat("has X1..X4: ", all(c("X1","X2","X3","X4") %in% names(t5)),
        " | >2 rows: ", nrow(t5) > 2, "\n", sep = "")
    print(head(as.data.frame(t5), 5))
  }

  # --- product tables ---
  cat("\n--- product tables 14:min(85,n): first cell of each ---\n")
  hi <- min(85, n)
  if (hi >= 14) {
    for (j in 14:hi) {
      tj <- tables[[j]]
      first <- if (nrow(tj) && ncol(tj)) as.character(tj[[1]][1]) else "<empty>"
      cat(sprintf("  [%2d] %dx%d  %s\n", j, nrow(tj), ncol(tj),
                  substr(first, 1, 60)))
    }
  } else {
    cat("  fewer than 14 tables; product extraction is impossible.\n")
  }

  # --- researcher links (margaret drops the first two <a> hrefs) ---
  hrefs <- page |> html_nodes("a") |> html_attr("href")
  cat("\n<a> hrefs: ", length(hrefs),
      " (margaret slices off the first 2, then cbinds the rest to table [[5]]",
      " -- these two lengths MUST match)\n", sep = "")
  cat("rows after slice: ", max(0, length(hrefs) - 2), "\n", sep = "")
  if (n >= 5) cat("table [[5]] rows after slice(-1,-2): ", max(0, nrow(tables[[5]]) - 2), "\n", sep = "")

  invisible(list(url = url, n_tables = n, tables = tables))
}

results <- lapply(urls, check_page)

# ---------------------------------------------------------------------------
# Only if the above looks sane, try the real thing on ONE group.
# getting_data() hardcodes write_xlsx(..., "margaret.xlsx") into getwd(),
# so we chdir into a temp dir to keep the repo clean.
# ---------------------------------------------------------------------------

run_real <- FALSE   # flip to TRUE once the structure checks pass

if (run_real) {
  groups <- data.frame(
    grupo = c("NOMBRE DEL GRUPO"),
    url   = urls[1],
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tempdir(), "margaret_out")
  dir.create(out_dir, showWarnings = FALSE)
  old <- setwd(out_dir); on.exit(setwd(old), add = TRUE)

  t0 <- Sys.time()
  res <- tryCatch(margaret::getting_data(groups), error = function(e) e)
  cat("\nelapsed: ", round(difftime(Sys.time(), t0, units = "secs")), "s\n", sep = "")

  if (inherits(res, "error")) {
    cat("getting_data() FAILED: ", conditionMessage(res), "\n", sep = "")
  } else {
    cat("sheets returned: ", length(res), "\n", sep = "")
    cat("names: ", paste(names(res), collapse = ", "), "\n", sep = "")
    cat("articulos rows: ", nrow(res[["articulos"]]), "\n", sep = "")
    cat("xlsx written to: ", file.path(out_dir, "margaret.xlsx"), "\n", sep = "")
  }
  setwd(old)
}

# ---------------------------------------------------------------------------
# Environment fingerprint -- record this, you'll need to reproduce it in CI.
# ---------------------------------------------------------------------------
cat("\n--- environment ---\n")
cat("R: ", R.version.string, "\n", sep = "")
for (p in c("margaret", "rvest", "httr", "igraph", "widyr", "tidytext",
            "scholar", "dplyr", "writexl")) {
  v <- tryCatch(as.character(packageVersion(p)), error = function(e) "NOT INSTALLED")
  cat(sprintf("  %-10s %s\n", p, v))
}
