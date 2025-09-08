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

# ---- Resolve current game_id ----
today <- Sys.Date()
season <- as.integer(format(today, "%Y"))
sched <- nflfastR::fast_scraper_schedules(season)
phi_sched <- sched %>% filter(home_team == team | away_team == team)
if (nrow(phi_sched) == 0) stop("No PHI games found in schedules for season: ", season)

#
# Normalize date column name from schedules
date_col <- if ("game_date" %in% names(phi_sched)) {
  "game_date"
} else if ("gameday" %in% names(phi_sched)) {
  "gameday"
} else {
  stop("No game date column found in schedules (expected 'game_date' or 'gameday').")
}
phi_sched <- phi_sched %>%
  mutate(.game_date = as.Date(.data[[date_col]]))

# Prefer most recent game on/before today; if none, take next upcoming
past <- phi_sched %>% filter(.game_date <= today)
chosen <- if (nrow(past) > 0) {
  past %>% arrange(desc(.game_date)) %>% slice(1)
} else {
  phi_sched %>% arrange(.game_date) %>% slice(1)
}
game_id <- chosen$game_id[[1]]

message("Using game_id: ", game_id)

# ---- Scrape PBP for that game ----
pbp <- nflfastR::fast_scraper(game_id = game_id)
if (nrow(pbp) == 0) stop("No plays returned for game_id: ", game_id)

# ---- Enrich with nfl4th model (2024 model for any 2025 rows) ----
pbp <- pbp %>% mutate(season_orig = season)

base_df <- pbp %>%
  mutate(
    season = dplyr::if_else(season_orig == 2025L, 2024L, season_orig),
    actual_decision = dplyr::case_when(
      play_type == "punt" ~ "Punt",
      play_type == "field_goal" ~ "Field Goal",
      play_type %in% c("run", "pass") ~ "Go for it",
      TRUE ~ "Other"
    )
  )

# Derive home_opening_kickoff if missing (needed by nfl4th)
if (!"home_opening_kickoff" %in% names(base_df)) {
  first_kick <- base_df %>% dplyr::filter(qtr == 1, kickoff_attempt == 1) %>% dplyr::slice(1)
  if (nrow(first_kick) == 1) {
    hok <- as.integer(first_kick$posteam == first_kick$home_team)
  } else {
    hok <- NA_integer_
  }
  base_df <- base_df %>% mutate(home_opening_kickoff = hok)
}

enriched <- base_df %>%
  nfl4th::add_4th_probs() %>%
  mutate(
    season = season_orig,
    model_recommendation = dplyr::case_when(
      go_wp > fg_wp & go_wp > punt_wp ~ "Go for it",
      fg_wp > punt_wp ~ "Field Goal",
      TRUE ~ "Punt"
    )
  ) %>%
  select(-season_orig)

# ---- Write CSV (no gzip, no commit) ----
readr::write_csv(enriched, out_path)
message("Wrote: ", normalizePath(out_path, winslash = "/"))
