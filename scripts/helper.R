# scripts/helper.R

suppressPackageStartupMessages({
  library(dplyr)
})

# --- Shared helpers ---
`%||%` <- function(x, y) if (is.null(x)) y else x

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

# --- PBP-specific enhancements (from nflfastR pbp) ---
add_frontend_columns_pbp <- function(df, team = "PHI") {
  # Coaches
  phi_coach <- ifelse(df$home_team == team, df$home_coach, df$away_coach)
  opp_coach <- ifelse(df$home_team != team, df$home_coach, df$away_coach)

  # Clock
  game_clock <- n_to_clock(df$quarter_seconds_remaining)

  # FG prob
  fga <- as.integer(df$field_goal_attempt %||% 0)
  fg_prob_calculated <- ifelse(fga > 0, df$fg_prob, df$fg_make_prob)
  fg_prob_calculated_pct <- pct_round(fg_prob_calculated)

  # Decision string
  base_decision <- dplyr::coalesce(df$actual_decision, "Other")
  actual_decision_calculated <- base_decision
  has_pen_any <- (as.integer(df$penalty %||% 0) > 0) |
                 grepl("penalty", tolower(df$desc %||% ""), fixed = TRUE)
  actual_decision_calculated <- ifelse(has_pen_any,
                                       paste0(actual_decision_calculated, " (Penalty)"),
                                       actual_decision_calculated)

  # Model recommendation from go_wp / fg_wp / punt_wp with tie handling
  go_c   <- dplyr::coalesce(df$go_wp,   -Inf)
  fg_c   <- dplyr::coalesce(df$fg_wp,   -Inf)
  punt_c <- dplyr::coalesce(df$punt_wp, -Inf)
  top    <- pmax(go_c, fg_c, punt_c)
  top_ties <- as.integer(go_c == top) + as.integer(fg_c == top) + as.integer(punt_c == top)
  model_recommendation <- dplyr::case_when(
    top_ties >= 2          ~ "Toss Up",
    go_c   == top          ~ "Go for it",
    fg_c   == top          ~ "Field Goal",
    TRUE                   ~ "Punt"
  )

  dplyr::mutate(
    df,
    phi_coach = phi_coach,
    opp_coach = opp_coach,
    game_clock = game_clock,
    fg_prob_calculated = fg_prob_calculated,
    fg_prob_calculated_pct = fg_prob_calculated_pct,
    actual_decision_calculated = actual_decision_calculated,
    model_recommendation = model_recommendation,
    go_wp_pct = pct_round(df$go_wp),
    first_down_prob_pct = pct_round(df$first_down_prob),
    wp_fail_pct = pct_round(df$wp_fail),
    wp_succeed_pct = pct_round(df$wp_succeed),
    punt_wp_pct = pct_round(df$punt_wp),
    fg_wp_pct = pct_round(df$fg_wp),
    make_fg_wp_pct = pct_round(df$make_fg_wp),
    miss_fg_wp_pct = pct_round(df$miss_fg_wp)
  )
}

# --- 4th-down-only enhancements (from nfl4th::add_4th_probs) ---
add_frontend_columns_4th <- function(df, team = "PHI") {
  # Map yardline → yrdln for consistency with frontend
  if ("yardline" %in% names(df)) {
    df <- dplyr::mutate(df, yrdln = yardline)
  }

  # Clock
  game_clock <- n_to_clock(df$quarter_seconds_remaining)

  # FG prob
  fg_prob_calculated <- df$fg_make_prob
  fg_prob_calculated_pct <- pct_round(df$fg_make_prob)

  # Decision string from play_type/type_text
  base_decision <- dplyr::case_when(
    grepl("punt", tolower(df$type_text %||% "")) ~ "Punt",
    grepl("field goal", tolower(df$type_text %||% "")) ~ "Field Goal",
    grepl("pass|rush", tolower(df$type_text %||% "")) ~ "Go for it",
    TRUE ~ "Other"
  )
  actual_decision_calculated <- base_decision

  dplyr::mutate(
    df,
    game_clock = game_clock,
    fg_prob_calculated = fg_prob_calculated,
    fg_prob_calculated_pct = fg_prob_calculated_pct,
    actual_decision_calculated = actual_decision_calculated,
    go_wp_pct = pct_round(df$go_wp),
    first_down_prob_pct = pct_round(df$first_down_prob),
    wp_fail_pct = pct_round(df$wp_fail),
    wp_succeed_pct = pct_round(df$wp_succeed),
    punt_wp_pct = pct_round(df$punt_wp),
    fg_wp_pct = pct_round(df$fg_wp),
    make_fg_wp_pct = pct_round(df$make_fg_wp),
    miss_fg_wp_pct = pct_round(df$miss_fg_wp)
  )
}