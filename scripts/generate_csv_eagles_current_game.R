# frozen_string_literal: false
# Generate current game CSV for the site (no commit)
suppressPackageStartupMessages({
  library(nfl4th)
  library(dplyr)
  library(readr)
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
pbp <- nfl4th::get_4th_plays(game_id)
if (nrow(pbp) == 0) stop("No plays returned for game_id: ", game_id)

# add WP options and then compute frontend helpers (aliases, flags, clocks, pct, recommendation)
enriched <- pbp %>%
  nfl4th::add_4th_probs() %>%
  add_frontend_columns_4th(team = team)

message("Columns: ", paste(names(enriched), collapse = ", "))
# ---- Write CSV (no gzip, no commit) ----
readr::write_csv(enriched, out_path)
message("Wrote: ", normalizePath(out_path, winslash = "/"))
