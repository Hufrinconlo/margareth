# Pins BOTH R and the CRAN snapshot date (2026-04-23). Do not change to
# :latest — that would let package upgrades break margaret, which is
# unmaintained and already needs runtime patches.

FROM rocker/r-ver:4.5.3

# Heavy deps in their own layer so they stay cached across rebuilds
RUN install2.r --error --skipinstalled \
    scholar tidytext widyr treemapify igraph writexl SnowballC remotes

# margaret last — this is the layer you'll rebuild if you need to patch it
RUN R -e "remotes::install_github('coreofscience/margaret', upgrade='never')" \
 || R -e "install.packages('margaret')"

WORKDIR /work
FROM rocker/r-ver:4.5.3

RUN apt-get update && apt-get install -y --no-install-recommends \
      libuv1t64 libglpk40 libgmp10 libxml2 libicu74 \
      libfontconfig1 libfreetype6 libharfbuzz0b libfribidi0 \
      libpng16-16 libtiff6 libjpeg8 \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled \
    tidyverse rvest httr scholar tidytext widyr treemapify \
    igraph writexl SnowballC remotes devtools usethis

RUN R -q -e "remotes::install_github('cran/margaret@0.1.4', upgrade='never')"

# Hard gate — fails the build instead of silently producing a broken image
RUN R -q -e 'library(margaret); cat("margaret", as.character(packageVersion("margaret")), "OK\n")'

WORKDIR /work
