

# frozen_string_literal: false
suppressPackageStartupMessages({
  library(nflfastR)
  library(nfl4th)
  library(dplyr)
  library(tibble)
})

#' Resolve the current or next PHI game_id for a given season
#' Chooses the most recent game on/before today; if none, the next upcoming.
#' @param team three-letter team code, default "PHI"
#' @param today Date, default Sys.Date()
#' @param season integer year, default as.integer(format(today, "%Y"))
#' @return single character game_id
resolve_current_game_id <- function(team = "PHI", today = Sys.Date(), season = as.integer(format(today, "%Y"))) {
  sched <- nflfastR::fast_scraper_schedules(season)
  team_sched <- dplyr::filter(sched, home_team == team | away_team == team)
  if (nrow(team_sched) == 0) stop("No games found for ", team, " in season: ", season)

  # Normalize date column name
  date_col <- if ("game_date" %in% names(team_sched)) {
    "game_date"
  } else if ("gameday" %in% names(team_sched)) {
    "gameday"
  } else {
    stop("No game date column found in schedules (expected 'game_date' or 'gameday').")
  }
  team_sched <- dplyr::mutate(team_sched, .game_date = as.Date(.data[[date_col]]))

  past <- dplyr::filter(team_sched, .game_date <= today)
  chosen <- if (nrow(past) > 0) {
    dplyr::slice(dplyr::arrange(past, dplyr::desc(.game_date)), 1)
  } else {
    dplyr::slice(dplyr::arrange(team_sched, .game_date), 1)
  }
  chosen$game_id[[1]]
}

#' Enrich a play-by-play dataframe with nfl4th 4th-down probabilities
#' Applies 2024 model to 2025 rows, derives home_opening_kickoff if missing,
#' and restores season after enrichment. Drops helper column season_orig.
#' @param pbp_df play-by-play dataframe with at least columns season, qtr, kickoff_attempt, posteam, home_team, play_type
#' @return enriched dataframe
enrich_with_nfl4th <- function(pbp_df) {
  if (!"season" %in% names(pbp_df)) stop("Input df missing `season` column.")
  df <- dplyr::mutate(pbp_df, season_orig = season,
                      season = dplyr::if_else(season == 2025L, 2024L, season),
                      actual_decision = dplyr::case_when(
                        play_type == "punt" ~ "Punt",
                        play_type == "field_goal" ~ "Field Goal",
                        play_type %in% c("run", "pass") ~ "Go for it",
                        TRUE ~ "Other"
                      ))

  # Derive home_opening_kickoff if missing (needed by nfl4th)
  if (!"home_opening_kickoff" %in% names(df)) {
    first_kick <- df %>% dplyr::filter(qtr == 1, kickoff_attempt == 1) %>% dplyr::slice(1)
    if (nrow(first_kick) == 1) {
      hok <- as.integer(first_kick$posteam == first_kick$home_team)
    } else {
      hok <- NA_integer_
    }
    df <- dplyr::mutate(df, home_opening_kickoff = hok)
  }

  df %>%
    nfl4th::add_4th_probs() %>%
    dplyr::mutate(
      season = season_orig,
      model_recommendation = dplyr::case_when(
        go_wp > fg_wp & go_wp > punt_wp ~ "Go for it",
        fg_wp > punt_wp ~ "Field Goal",
        TRUE ~ "Punt"
      )
    ) %>%
    dplyr::select(-season_orig)
}