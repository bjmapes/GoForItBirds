FROM rocker/r-ver:4.3.3
ENV RSPM=https://packagemanager.posit.co/cran/__linux__/jammy/latest
RUN apt-get update && apt-get install -y --no-install-recommends \
  libcurl4-openssl-dev libssl-dev libxml2-dev zlib1g-dev libgit2-dev make g++ git \
  && rm -rf /var/lib/apt/lists/*
RUN R -q -e 'install.packages("pak", repos=Sys.getenv("RSPM"))' \
    -e 'pak::pkg_install(c("nflfastR","nflreadr","dplyr","readr","jsonlite","nfl4th"))'