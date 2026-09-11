# data

Artifact branch for the Margaret dashboard. Normally written by the weekly
ETL (.github/workflows/etl.yml); the app reads margaret.rds from here.

## This commit was published by hand

Scraped 2026-09-08. Republished manually on 2026-09-11 because
scienti.minciencias.gov.co was unreachable and a demo was needed.

It is NOT a fresh scrape. The source artifact predated the PII fixes, so it
was cleaned before publishing: `email` and `url.y` dropped from the groups
sheet, and email addresses redacted from cell values in `cursos`. It passes
the PII gate in etl/run_etl.R. Researcher CvLAC urls are retained
deliberately.

The next successful ETL run replaces all of this.
