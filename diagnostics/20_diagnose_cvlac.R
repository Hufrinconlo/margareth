# Confirm the patch fixes CvLAC parsing before rerunning the full ETL.

suppressPackageStartupMessages({
  library(rvest); library(httr); library(purrr); library(tibble); library(margaret)
})

source("patch_margaret.R")

groups <- readr::read_csv("groups.csv", show_col_types = FALSE)

page  <- read_html(GET(groups$url[1]))
hrefs <- page |> html_nodes("a") |> html_attr("href")
urls  <- hrefs[grepl("cvlac", hrefs, ignore.case = TRUE)]
cat("cvlac urls found:", length(urls), "\n\n")

cat("--- BEFORE patch ---\n")
verify_patch(urls, n = 2)

cat("\n--- applying patch ---\n")
patch_add_rownames()

cat("\n--- AFTER patch ---\n")
verify_patch(urls, n = 5)
