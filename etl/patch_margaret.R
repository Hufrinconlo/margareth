# ---------------------------------------------------------------------------
# patch_margaret.R  --  source() this before calling margaret::getting_data()
#
# Two patches, both for 2022-era assumptions that no longer hold:
#
#   1. add_rownames()  -- defunct in modern dplyr; threw on EVERY researcher.
#   2. empty CvLACs    -- profiles with no "Formacion Academica" section blow
#                         up on rename(1, 2) before reaching the function's
#                         own fallback, so they were mislabelled "CvLAC
#                         oculto" (hidden) when they are really just blank.
#
# Run patch_all() to apply both.
# ---------------------------------------------------------------------------

# --- 1. add_rownames -------------------------------------------------------

add_rownames_shim <- function(df, var = "rowname") {
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  out <- tibble::rownames_to_column(out, var = var)
  tibble::as_tibble(out)
}

patch_add_rownames <- function() {
  done <- character()

  try({
    assignInNamespace("add_rownames", add_rownames_shim, ns = "dplyr")
    done <- c(done, "dplyr")
  }, silent = TRUE)

  try({
    imp <- parent.env(asNamespace("margaret"))
    if (exists("add_rownames", envir = imp, inherits = FALSE)) {
      if (bindingIsLocked("add_rownames", imp)) unlockBinding("add_rownames", imp)
      assign("add_rownames", add_rownames_shim, envir = imp)
      done <- c(done, "margaret:imports")
    }
  }, silent = TRUE)

  message("patched add_rownames in: ", paste(done, collapse = ", "))
  invisible(done)
}

# --- 2. CvLAC parser -------------------------------------------------------
# Body copied from margaret::get_posgrade_clasficitation_cvlac with one
# change: check for the academic section BEFORE running the pipeline that
# assumes it exists. Category extraction is unchanged and still runs, since
# a blank profile can still carry a Minciencias classification.

cvlac_parser_patched <- function(cvlac_url) {

  X1 <- X5 <- X7 <- X9 <- value <- rowname <- posgrade <-
    Month <- ranking <- X2 <- clasification <- NULL

  cvlac_df <- read_html(httr::GET(cvlac_url)) |> html_table()

  # --- category (runs regardless of whether education is recorded) ---
  cvlac_category <- cvlac_df[[1]] |>
    filter(X1 == paste("Categor", i, "a", sep = "")) |>
    select(X2)

  if (is_empty(cvlac_category$X2)) {
    cvlac_category <- tibble(clasification = "Sin clasificar")
  } else {
    cvlac_category <- cvlac_category |>
      mutate(clasification = str_extract(string = X2, pattern = ".*\\)")) |>
      select(clasification)
  }

  # --- THE GUARD: no academic section at all -------------------------------
  has_formacion <- any(
    str_detect(cvlac_df[[1]]$X1,
               paste("Formaci", o, "n Acad", e, "mica", sep = "")),
    na.rm = TRUE
  )

  if (!has_formacion) {
    return(bind_cols(tibble(posgrade = "Sin informaci\u00f3n"), cvlac_category))
  }

  # --- original pipeline, unchanged ----------------------------------------
  cvlac_posgrade <- cvlac_df[[1]] |>
    filter(str_detect(string = X1,
                      pattern = paste("Formaci", o, "n Acad", e, "mica", sep = ""))) |>
    select(X5, X7, X9) |>
    slice(1) |>
    separate_rows(X5, sep = "\r\n") |>
    slice(1, 4) |>
    mutate(X5 = str_trim(X5)) |>
    nest(data = X5) |> rename("X5" = data) |>
    separate_rows(X7, sep = "\r\n") |>
    slice(1, 4) |>
    mutate(X7 = str_trim(X7)) |>
    nest(data = X7) |> rename("X7" = data) |>
    separate_rows(X9, sep = "\r\n") |>
    slice(1, 4) |>
    mutate(X9 = str_trim(X9)) |>
    nest(data = X9) |> rename("X9" = data) |>
    unnest(cols = c(X5, X7, X9)) |>
    add_rownames() |>
    gather(var, value, -rowname) |>
    spread(rowname, value) |>
    select(-var) |>
    rename("posgrade" = 1, "duration" = 2) |>
    separate(duration, sep = " - ", into = c("start", "end")) |>
    filter(end != "de") |>
    filter(posgrade %in% c("Doctorado",
                           paste("Maestr", i, "a/Magister", sep = ""),
                           paste("Especializaci", o, "n", sep = ""),
                           "Pregrado/Universitario")) |>
    separate(end, into = c("Month", "year"), sep = " ") |>
    mutate(Month = if_else(Month == "de", "Enero", Month)) |>
    mutate(Month = str_remove(Month, "de"),
           end = str_c(Month, year, sep = " "),
           end = parse_date(end, "%B %Y", locale = locale("es"))) |>
    filter(end <= today()) |>
    mutate(ranking = if_else(posgrade == "Doctorado", 3,
                      if_else(posgrade == paste("Maestr", i, "a/Magister", sep = ""), 2,
                       if_else(posgrade == paste("Especializaci", o, "n", sep = ""), 1, 0)))) |>
    slice_max(ranking) |>
    select(posgrade) |>
    slice(1)

  if (is_empty(cvlac_posgrade$posgrade)) {
    cvlac_posgrade <- tibble(posgrade = paste("T", e, "cnico", sep = ""))
  }

  bind_cols(cvlac_posgrade, cvlac_category)
}

patch_cvlac_parser <- function() {
  # Evaluate inside margaret's namespace so dplyr/tidyr/rvest verbs and the
  # accent helpers (o, e, i) all resolve exactly as they do for the original.
  environment(cvlac_parser_patched) <- asNamespace("margaret")
  assignInNamespace("get_posgrade_clasficitation_cvlac",
                    cvlac_parser_patched, ns = "margaret")
  message("patched get_posgrade_clasficitation_cvlac")
}

patch_all <- function() {
  patch_add_rownames()
  patch_cvlac_parser()
}

# --- helper ----------------------------------------------------------------

verify_patch <- function(cvlac_urls, n = 5) {
  f <- getFromNamespace("get_posgrade_clasficitation_cvlac", "margaret")
  res <- purrr::map(head(cvlac_urls, n), purrr::safely(f))
  for (k in seq_along(res)) {
    if (is.null(res[[k]]$error)) {
      cat(sprintf("[%d] OK\n", k)); print(res[[k]]$result)
    } else {
      cat(sprintf("[%d] FAILING: %s\n", k, conditionMessage(res[[k]]$error)))
    }
  }
  invisible(res)
}
