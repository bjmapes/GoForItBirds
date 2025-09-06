# scripts/generate_csv_eagles_all_plays_historic.R

# ---- Packages ----
library(nflfastR)
library(dplyr)
library(purrr)
library(readr)
library(jsonlite)
library(stringr)
library(tibble)

# ---- Config ----
seasons <- 1999:2024
pbp_out_dir <- "assets/data/pbp"                 # per-game CSV.gz
index_out_liquid <- "_data/games_index.json"     # for Jekyll/Liquid
index_out_js     <- "assets/data/games_index.json" # for client JS

dir.create(pbp_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(index_out_liquid), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(index_out_js), recursive = TRUE, showWarnings = FALSE)

# ---- Helpers ----
last_non_na <- function(x) {
  y <- x[!is.na(x)]
  if (length(y)) y[length(y)] else NA
}

# ---- Load PBP ----
message("Loading play-by-play ", min(seasons), "-", max(seasons), " ...")
pbp_all <- load_pbp(seasons)

# Only games involving PHI (home or away)
eagles_pbp <- pbp_all %>% filter(home_team == "PHI" | away_team == "PHI")

# Guard: require game_id
if (!"game_id" %in% names(eagles_pbp)) {
  stop("Column `game_id` missing in PBP. nflfastR schema changed?")
}

# ---- Write per-game CSV.gz & collect index rows ----
message("Writing per-game CSV.gz ...")

idx_list <- list()
gids <- unique(eagles_pbp$game_id)

for (i in seq_along(gids)) {
  gid <- gids[[i]]
  g <- eagles_pbp %>% filter(game_id == gid)

  if (nrow(g) == 0) next

  season <- dplyr::first(g$season)
  week   <- dplyr::first(g$week)
  date   <- as.character(dplyr::first(g$game_date))
  home   <- dplyr::first(g$home_team)
  away   <- dplyr::first(g$away_team)

  final_home <- last_non_na(g$total_home_score)
  final_away <- last_non_na(g$total_away_score)

  # Ensure season directory exists
  season_dir <- file.path(pbp_out_dir, as.character(season))
  dir.create(season_dir, recursive = TRUE, showWarnings = FALSE)

  # Output file path
  csv_gz_path <- file.path(season_dir, paste0(gid, ".csv.gz"))

  # Write gzipped CSV (all plays for both teams)
  message(sprintf("  • (%d/%d) %s", i, length(gids), gid))
  con <- gzfile(csv_gz_path, open = "wb")
  write_csv(g, con)
  close(con)

  # Collect index row
  idx_list[[length(idx_list) + 1]] <- tibble(
    season = season,
    game_id = gid,
    week = week,
    date = date,
    home = home,
    away = away,
    final = list(list(home = final_home, away = final_away)),
    pbp_url = paste0("/", csv_gz_path) # site-relative URL
  )
}

if (length(idx_list) == 0) {
  stop("No Eagles games found in the selected seasons.")
}

idx_rows <- bind_rows(idx_list) %>%
  arrange(season, week, game_id)

# ---- Build season-grouped index structure ----
idx_by_season <- idx_rows %>%
  group_by(season) %>%
  reframe(
    games = list(purrr::pmap(
      list(game_id, week, date, home, away, final, pbp_url),
      \(gid, wk, dt, hm, aw, fin, url) {
        list(
          game_id = gid,
          week = wk,
          date = dt,
          home = hm,
          away = aw,
          final = fin[[1]],
          pbp_url = url
        )
      }
    ))
  ) %>%
  ungroup() %>%
  arrange(season)

index_list <- purrr::map2(
  idx_by_season$season, idx_by_season$games,
  ~list(season = .x, games = .y)
)

# ---- Write JSON index to both locations ----
json_txt <- toJSON(index_list, auto_unbox = TRUE, pretty = TRUE)
writeLines(json_txt, index_out_liquid)
writeLines(json_txt, index_out_js)

message("Done.")
message("Per-game CSV.gz in: ", normalizePath(pbp_out_dir, winslash = "/"))
message("Index JSON: ", normalizePath(index_out_liquid, winslash = "/"))
message("Index JSON (public): ", normalizePath(index_out_js, winslash = "/"))