# scripts/generate_csv_eagles_current_game.R

suppressPackageStartupMessages({
  library(nflfastR)
  library(nfl4th)
  library(dplyr)
  library(tibble)
})
source("scripts/helper.R")

resolve_current_game_id <- function(team = "PHI", today = Sys.Date(),
                                    season = as.integer(format(today, "%Y"))) {
  sched <- nflfastR::fast_scraper_schedules(season)
  team_sched <- dplyr::filter(sched, home_team == team | away_team == team)
  if (nrow(team_sched) == 0) stop("No games found for ", team)

  date_col <- if ("game_date" %in% names(team_sched)) "game_date" else "gameday"
  team_sched <- dplyr::mutate(team_sched, .game_date = as.Date(.data[[date_col]]))

  past <- dplyr::filter(team_sched, .game_date <= today)
  chosen <- if (nrow(past) > 0) dplyr::slice(dplyr::arrange(past, dplyr::desc(.game_date)), 1)
            else dplyr::slice(dplyr::arrange(team_sched, .game_date), 1)
  chosen$game_id[[1]]
}

team <- "PHI"
game_id <- resolve_current_game_id(team = team)
message("Using game_id: ", game_id)

pbp <- nfl4th::get_4th_plays(game_id)
enriched <- pbp %>%
  nfl4th::add_4th_probs() %>%
  add_frontend_columns_4th(team = team)

out_dir <- "assets/data/current"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, paste0(game_id, ".csv"))
readr::write_csv(enriched, out_path)
message("Wrote current game CSV: ", normalizePath(out_path, winslash = "/"))