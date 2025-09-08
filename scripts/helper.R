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
        fg_wp > punt_wp                 ~ "Field Goal",
        TRUE                            ~ "Punt"
      )
    )
}  

#' Get canonical schema (names and type hints) from an existing per-game CSV(.gz)
#' If sample_file is NULL, auto-pick the first match under assets/data/pbp.
#' Returns a list with elements: names (character vector) and types (named character vector of typeof()).
get_master_schema <- function(sample_file = NULL) {
  if (is.null(sample_file)) {
    candidates <- list.files("assets/data/pbp", pattern = "\\.csv(\\.gz)?$", recursive = TRUE, full.names = TRUE)
    if (length(candidates) == 0) {
      stop("No sample CSVs found under assets/data/pbp.")
    }
    sample_file <- candidates[[1]]
  }
  read_sample <- function(path) {
    if (grepl("\\.gz$", path)) {
      con <- gzfile(path, open = "rb"); on.exit(close(con), add = TRUE)
      readr::read_csv(con, n_max = 1, show_col_types = FALSE)
    } else {
      readr::read_csv(path, n_max = 1, show_col_types = FALSE)
    }
  }
  sm <- read_sample(sample_file)
  list(names = names(sm), types = vapply(sm, typeof, character(1)))
}

#' Enforce exact column set and order (optionally add missing with typed NA, drop extras)
#' Returns the dataframe with columns reordered to match master_names.
enforce_schema_strict <- function(df, master_names, master_types = NULL, add_missing = TRUE, drop_extras = FALSE) {
  # Add missing columns with appropriately typed NA so CSV shows blanks
  missing <- setdiff(master_names, names(df))
  if (length(missing) > 0) {
    if (!add_missing) {
      stop("Schema mismatch: missing required columns: ", paste(missing, collapse = ", "))
    }
    for (nm in missing) {
      tp <- if (!is.null(master_types) && nm %in% names(master_types)) master_types[[nm]] else "character"
      df[[nm]] <- switch(tp,
        "integer"   = NA_integer_,
        "double"    = NA_real_,
        "numeric"   = NA_real_,
        "logical"   = NA,
        "character" = NA_character_,
        NA_character_
      )
    }
  }
  # Drop extra columns if requested
  extras <- setdiff(names(df), master_names)
  if (length(extras) > 0) {
    if (!drop_extras) {
      stop("Schema mismatch: extra columns present: ", paste(extras, collapse = ", "))
    }
    message("Schema notice: extra columns will be dropped: ", paste(extras, collapse = ", "))
    df <- df[, setdiff(names(df), extras), drop = FALSE]
  }
  # Reorder to canonical
  dplyr::select(df, dplyr::all_of(master_names))
}

#' Resolve human-facing decision and display fields for frontend
#' Adds:
#'  - actual_decision_calculated: fallback from desc/punt/fg/pass/rush with "(Penalty)" tag
#'  - phi_coach: coach for PHI (or provided team)
#'  - opp_coach: coach for opponent
#'  - game_clock: "MM:SS" from quarter_seconds_remaining
#'  - fg_prob_calculated: fg_prob if an actual FGA, else fg_make_prob
#'  - fg_prob_calculated_pct: rounded percentage of fg_prob_calculated
#'  - *_pct rounded percentage helpers for table display
#' @param df enriched pbp dataframe (after enrich_with_nfl4th)
#' @param team team code to treat as "Birds" (default "PHI")
#' @return dataframe with added columns
add_frontend_columns <- function(df, team = "PHI") {
  # safe helpers
  to_lower <- function(x) {
    x <- if (is.null(x)) NA_character_ else x
    tolower(ifelse(is.na(x), "", as.character(x)))
  }
  has_substr <- function(hay, needle) {
    hay <- to_lower(hay)
    needle <- tolower(needle)
    ifelse(is.na(hay), FALSE, grepl(needle, hay, fixed = TRUE))
  }
  n_to_clock <- function(n) {
    n <- suppressWarnings(as.integer(n))
    mins <- ifelse(is.na(n), NA_integer_, n %/% 60)
    secs <- ifelse(is.na(n), NA_integer_, n %% 60)
    ifelse(is.na(mins),
           NA_character_,
           paste0(sprintf("%d", mins), ":", sprintf("%02d", secs)))
  }
  pct_round <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    ifelse(is.na(x), NA_real_, round(x * 100))
  }

  # decision_resolved
  punt_flag <- as.integer(df$punt_attempt %||% 0)
  fg_flag   <- as.integer(df$field_goal_attempt %||% 0)
  pass_flag <- as.integer(df$pass %||% 0)
  rush_flag <- as.integer(df$rush %||% 0)
  desc_low  <- to_lower(df$desc)

  actual_decision_calculated <- df$actual_decision
  actual_decision_calculated[is.na(actual_decision_calculated)] <- ""

  idx_other <- which(actual_decision_calculated == "Other")
  if (length(idx_other)) {
    is_punt <- punt_flag[idx_other] > 0 |
      has_substr(desc_low[idx_other], "punt formation") |
      has_substr(desc_low[idx_other], " punt")
    is_fg <- fg_flag[idx_other] > 0 |
      has_substr(desc_low[idx_other], "field goal formation") |
      has_substr(desc_low[idx_other], " fg") |
      has_substr(desc_low[idx_other], "field goal")
    is_go <- pass_flag[idx_other] > 0 | rush_flag[idx_other] > 0

    actual_decision_calculated[idx_other] <- ifelse(is_punt, "Punt",
                                    ifelse(is_fg, "Field Goal",
                                    ifelse(is_go, "Go for it", "Other")))
    has_pen <- has_substr(desc_low[idx_other], "penalty")
    actual_decision_calculated[idx_other] <- ifelse(has_pen,
                                           paste0(actual_decision_calculated[idx_other], " (Penalty)"),
                                           actual_decision_calculated[idx_other])
  }

  # phi_coach
  phi_coach <- ifelse(df$home_team == team, df$home_coach, df$away_coach)
  opp_coach <- ifelse(df$home_team != team, df$home_coach, df$away_coach)

  # game_clock
  game_clock <- n_to_clock(df$quarter_seconds_remaining)

  # fg_prob_calculated and fg_prob_calculated_pct
  fga <- as.integer(df$field_goal_attempt %||% 0)
  fg_prob_calculated <- ifelse(fga > 0, df$fg_prob, df$fg_make_prob)
  fg_prob_calculated_pct <- pct_round(fg_prob_calculated)

  # percent columns
  go_wp_pct           <- pct_round(df$go_wp)
  first_down_prob_pct <- pct_round(df$first_down_prob)
  wp_fail_pct         <- pct_round(df$wp_fail)
  wp_succeed_pct      <- pct_round(df$wp_succeed)
  punt_wp_pct         <- pct_round(df$punt_wp)
  fg_wp_pct           <- pct_round(df$fg_wp)
  make_fg_wp_pct      <- pct_round(df$make_fg_wp)
  miss_fg_wp_pct      <- pct_round(df$miss_fg_wp)

  dplyr::mutate(
    df,
    actual_decision_calculated = actual_decision_calculated,
    phi_coach = phi_coach,
    opp_coach = opp_coach,
    game_clock = game_clock,
    fg_prob_calculated = fg_prob_calculated,
    fg_prob_calculated_pct = fg_prob_calculated_pct,
    go_wp_pct = go_wp_pct,
    first_down_prob_pct = first_down_prob_pct,
    wp_fail_pct = wp_fail_pct,
    wp_succeed_pct = wp_succeed_pct,
    punt_wp_pct = punt_wp_pct,
    fg_wp_pct = fg_wp_pct,
    make_fg_wp_pct = make_fg_wp_pct,
    miss_fg_wp_pct = miss_fg_wp_pct
  )
}

# Provide infix %||% like rlang's for simple defaulting
`%||%` <- function(x, y) if (is.null(x)) y else x