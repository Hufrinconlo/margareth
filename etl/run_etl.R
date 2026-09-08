# ---------------------------------------------------------------------------
# margaret ETL runner
#
# margaret makes HTTP calls in two places, both via httr::GET:
#   1. data_getting()            -> 1 request per group. NOT protected; crashes.
#   2. data_cleaning_researcher()-> 1 request per member. Wrapped in safely(),
#                                   so failures silently become "CvLAC oculto".
#
# We replace httr::GET in its own namespace with a retrying version. That
# covers both call sites without forking the package, and gives us telemetry
# so silent degradation becomes visible.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(httr); library(dplyr); library(readr); library(margaret)
})

source("etl/patch_margaret.R"); patch_all()

OUT_DIR   <- Sys.getenv("OUT_DIR", "output")
MAX_TRIES <- as.integer(Sys.getenv("MAX_TRIES", "4"))
DELAY_SEC <- as.numeric(Sys.getenv("DELAY_SEC", "0.5"))
TIMEOUT   <- as.integer(Sys.getenv("TIMEOUT_SEC", "60"))

# --- telemetry -------------------------------------------------------------
tel <- new.env(parent = emptyenv())
tel$ok <- 0L; tel$retried <- 0L; tel$failed <- 0L; tel$failed_urls <- character()

orig_GET <- httr::GET

retry_GET <- function(url = NULL, ...) {
  Sys.sleep(DELAY_SEC)                      # be a polite scraper
  for (attempt in seq_len(MAX_TRIES)) {
    resp <- tryCatch(
      orig_GET(url,
               httr::user_agent("margaret-etl (research dashboard)"),
               httr::timeout(TIMEOUT), ...),
      error = function(e) e
    )
    ok <- !inherits(resp, "error") && httr::status_code(resp) == 200
    if (ok) {
      if (attempt > 1L) tel$retried <- tel$retried + 1L
      tel$ok <- tel$ok + 1L
      return(resp)
    }
    if (attempt < MAX_TRIES) Sys.sleep(2^attempt)   # 2s, 4s, 8s
  }
  tel$failed <- tel$failed + 1L
  tel$failed_urls <- c(tel$failed_urls, as.character(url))
  # Re-raise so data_getting() fails loudly; safely() still catches it in
  # data_cleaning_researcher(), where degradation is acceptable.
  stop("GET failed after ", MAX_TRIES, " attempts: ", url)
}

assignInNamespace("GET", retry_GET, ns = "httr")

# --- input -----------------------------------------------------------------
groups <- read_csv("groups.csv", show_col_types = FALSE)
stopifnot(all(c("grupo", "url") %in% names(groups)))
message("groups: ", nrow(groups))

# --- run -------------------------------------------------------------------
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
old <- setwd(OUT_DIR); on.exit(setwd(old), add = TRUE)   # getting_data() writes to getwd()

t0  <- Sys.time()
res <- margaret::getting_data(as.data.frame(groups))

# Strip direct identifiers before publishing. The data branch is public,
# and a consolidated dataset is a different thing from 8 separate pages.
res[[1]] <- res[[1]] |> dplyr::select(-dplyr::any_of(c("email", "url.y")))

# getting_data() already wrote margaret.xlsx with the email column in it,
# so overwrite it with the cleaned version.
writexl::write_xlsx(res, file.path(OUT_DIR, "margaret.xlsx"))

elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
setwd(old)

# --- telemetry report ------------------------------------------------------
message("\n--- http ---")
message("ok: ", tel$ok, " | succeeded-after-retry: ", tel$retried, " | failed: ", tel$failed)
if (length(tel$failed_urls)) message("failed:\n  ", paste(tel$failed_urls, collapse = "\n  "))
message("elapsed: ", elapsed, " min")

# --- the check that matters ------------------------------------------------
# A timeout and a genuinely private CvLAC look identical downstream. Track the
# rate; a jump means network trouble, not a sudden wave of private profiles.
researchers <- res[[2]]
hidden  <- sum(researchers$posgrade == "CvLAC oculto", na.rm = TRUE)
blank   <- sum(researchers$posgrade == "Sin información", na.rm = TRUE)
total   <- nrow(researchers)
message("\nCvLAC oculto : ", hidden, "/", total)
message("Sin información: ", blank, "/", total)
rate <- round(100 * hidden / total, 1)
message("\nCvLAC oculto: ", hidden, "/", total, " (", rate, "%)")

readr::write_csv(
  tibble(run_at = Sys.time(), groups = nrow(groups), researchers = total,
         hidden = hidden, hidden_pct = rate, http_failed = tel$failed,
         elapsed_min = elapsed),
  file.path(OUT_DIR, "run_health.csv")
)

THRESHOLD <- as.numeric(Sys.getenv("HIDDEN_THRESHOLD", "35"))
if (rate > THRESHOLD) {
  stop("CvLAC oculto rate ", rate, "% exceeds ", THRESHOLD,
       "% -- likely network failure, not real data. Refusing to publish.")
}

saveRDS(res, file.path(OUT_DIR, "margaret.rds"))
message("\nwrote ", file.path(OUT_DIR, "margaret.rds"), " and margaret.xlsx")
