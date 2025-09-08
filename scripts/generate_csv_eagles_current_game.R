# frozen_string_literal: false
# Generate current game CSV for the site (no commit)
suppressPackageStartupMessages({
  library(nflfastR)
  library(nfl4th)
  library(dplyr)
  library(readr)
  library(tibble)
})


# ---- Config ----
team <- "PHI"
out_path <- "assets/data/current_game.csv"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

source("scripts/helper.R")

# ---- Resolve current game_id ----
game_id <- resolve_current_game_id(team = team)
message("Using game_id: ", game_id)

# ---- Scrape and enrich ----
pbp <- nflfastR::fast_scraper(game_id = game_id)
if (nrow(pbp) == 0) stop("No plays returned for game_id: ", game_id)
enriched <- enrich_with_nfl4th(pbp)

schema <- get_master_schema()
enriched <- enforce_schema_strict(enriched, schema$names, add_missing = TRUE, drop_extras = TRUE)
# ---- Write CSV (no gzip, no commit) ----
readr::write_csv(enriched, out_path)
message("Wrote: ", normalizePath(out_path, winslash = "/"))
