# Margaret dashboard

Research dashboard for UNAL La Paz (Cesar). Scrapes 8 GrupLAC groups
weekly via GitHub Actions, publishes an artifact to the `data` branch,
Shiny app reads it. Repo: Hufrinconlo/margareth (public).

## Critical: margaret is unmaintained (CRAN 0.1.4, 2022)
Two runtime patches in etl/patch_margaret.R, applied via patch_all().
WITHOUT THEM THE PIPELINE SILENTLY RETURNS GARBAGE:
1. dplyr::add_rownames() is defunct -> threw on every researcher, all 131
   mislabelled "CvLAC oculto".
2. CvLACs with no "Formación Académica" section hit rename(1,2) on an empty
   frame and error -> now labelled "Sin información" (~25% of researchers,
   this is real data, not a bug).

## Version pinning
rocker/r-ver:4.5.3 pins R AND the CRAN snapshot (p3m.dev .../2026-04-23).
Do NOT change to :latest. Version drift is what broke margaret, not
Minciencias — GrupLAC's HTML has been stable since 2022.

## Layout
etl/run_etl.R          scrape + httr::GET retry shim + health gate
etl/patch_margaret.R   the two patches
Dockerfile             ETL image (margaret, tidyverse)
Dockerfile.deploy      app + rsconnect image; run the app locally, and deploy
app/app.R              shinydashboard + plotly
app/load_data.R        reads raw.githubusercontent .../data/
diagnostics/           regression tests — run these first when it breaks
.github/workflows/etl.yml   weekly cron, container ghcr.io/hufrinconlo/margaret-etl

## Data shape
margaret.rds: [[1]] groups (8), [[2]] researchers (131), [3:n] ~38 product sheets.
Researchers have semicolon-delimited grupo paired positionally with
semicolon-delimited counts ("A; B" <-> "9; 0").
21 of the 48 product sheets carry `ano`, including articulos (which also has
SJR_Q) and capitulos — so the year slider does apply to more than articulos.
The rest (libros, proyectos, trabajos_dirigidos, ...) cannot be year-filtered
and the value boxes label themselves "todos los años" when they are not.
Top categories: articulos 364, eventos_cientificos 252, capitulos 207,
proyectos 143, trabajos_dirigidos 126. A softwares sheet exists but is empty,
so it renders "n/d", not 0.

`cursos` carries email addresses in its cell values, which dropping columns
cannot reach. run_etl.R redacts EMAIL_RE across every sheet before the gate
for exactly this reason — without it a good scrape aborts instead of
publishing.

## Gates
Two, both abort publishing:
- HIDDEN_THRESHOLD, if the hidden-CvLAC rate spikes. Currently 0% hidden.
  This one caught the add_rownames bug — keep it.
- PII, if an `email` column survives into either artifact. It reads the xlsx
  back off DISK rather than checking `res`: getting_data() writes its own copy
  into getwd(), so the in-memory strip can be correct while the published file
  is not. That is exactly what happened on 2026-09-08.

## Open items
- The `data` branch holds a MANUAL republish (2026-09-11) of the 2026-09-08
  scrape, cleaned to pass the gate — not a fresh scrape. The branch was deleted
  by hand on 2026-09-08 after it published researcher emails. Minciencias has
  been unreachable since 2026-09-10; the next green ETL replaces this.
- Researcher CvLAC urls in res[[2]] are published deliberately, not an
  oversight. Decided 2026-09-11: they are useful and already public.
- Deploy to shinyapps.io (app exists: hugorl/margaret, appId 17878520).
  Deploy from a container on the pinned snapshot, never a bare machine —
  rsconnect records whatever package versions the deploying host happens to
  have, which is the drift the base-image pin exists to prevent.
- Parked: OECD `Áreas de actuación` from CvLAC row 16 (cache HTML in the
  GET shim, parse offline); service portfolio taxonomy
