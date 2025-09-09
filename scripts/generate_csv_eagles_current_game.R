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
enriched <- enrich_with_nfl4th(pbp) %>% add_frontend_columns("PHI")

# pick latest historical game as master schema
year_dirs <- list.dirs("_data/pbp", recursive = FALSE, full.names = TRUE)
latest_year <- max(basename(year_dirs))
game_files <- list.files(file.path("_data/pbp", latest_year), pattern = "\\.csv$", full.names = TRUE)
master_path <- sort(game_files, decreasing = TRUE)[[1]]
schema <- get_master_schema(sample_file = master_path)

enriched <- enforce_schema_strict(
  enriched,
  master_names = schema$names,
  master_types = schema$types,
  add_missing = TRUE,
  drop_extras = TRUE
)
# ---- Write CSV (no gzip, no commit) ----
readr::write_csv(enriched, out_path)
message("Wrote: ", normalizePath(out_path, winslash = "/"))
