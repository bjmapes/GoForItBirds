# generate_fg_model.R — deterministic FG make table via nfl4th
# Writes /assets/data/fg_table.json with rows: distance, roof, prob

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(jsonlite)
  library(nfl4th)
})

cat("WD:", getwd(), "\n")

# --- Build grid of (distance, roof) ---
fg_grid <- expand.grid(
  distance = 10:70,
  roof = c("outdoors", "dome"),   # nfl4th roof levels
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(
    yardline_100 = pmin(pmax(distance - 17L, 1L), 99L)
  )

# --- Minimal PBP frame for nfl4th::add_4th_probs() ---
# Use neutral, valid context so fg_make_prob is defined.
base_ctx <- tibble(
  season = 2024L,
  season_type = "REG",
  qtr = 1L,
  quarter_seconds_remaining = 900L,
  half_seconds_remaining = 900L,
  game_seconds_remaining = 3600L,
  down = 4L,
  ydstogo = 1L,
  posteam = "PHI",
  defteam = "KC",
  home_team = "PHI",
  away_team = "KC",
  posteam_timeouts_remaining = 3L,
  defteam_timeouts_remaining = 3L,
  posteam_score = 0L,
  defteam_score = 0L,
  score_differential = 0L,
  turnover = 0L,
  # neutral environment to avoid NA from missing factors
  temp = 70L,
  wind = 0L,
  surface = "fieldturf",
  home_opening_kickoff = TRUE,
  receive_2h_ko = FALSE
)

pbp_rows <- fg_grid %>%
  bind_cols(base_ctx[rep(1, nrow(fg_grid)), ]) %>%
  select(
    season, season_type, qtr, quarter_seconds_remaining,
    half_seconds_remaining, game_seconds_remaining,
    down, ydstogo, posteam, defteam, home_team, away_team,
    home_opening_kickoff, receive_2h_ko,
    posteam_timeouts_remaining, defteam_timeouts_remaining,
    posteam_score, defteam_score, score_differential, turnover,
    temp, wind, surface,
    yardline_100, roof
  )

# --- Query nfl4th for probabilities ---
with_probs <- nfl4th::add_4th_probs(pbp_rows)

if (!"fg_make_prob" %in% names(with_probs)) {
  stop("fg_make_prob not found in nfl4th output; update nfl4th and retry.")
}

fg_table <- with_probs %>%
  transmute(
    distance = fg_grid$distance,
    roof = fg_grid$roof,
    prob = round(pmin(pmax(as.numeric(.data$fg_make_prob), 0), 1), 4)
  )

cat("NA probs:", sum(is.na(fg_table$prob)), "of", nrow(fg_table), "\n")
cat("Preview (first/last 6):\n"); print(head(fg_table)); print(tail(fg_table))

out_dir <- file.path(getwd(), "assets", "data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_path <- file.path(out_dir, "fg_table.json")
write_json(fg_table, out_path, pretty = TRUE, auto_unbox = TRUE)
cat("Wrote:", out_path, "\n")