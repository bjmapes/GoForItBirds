# scripts/generate_csv_eagles_all_plays.R

# ---- Packages ----
library(nflfastR)
library(dplyr)
library(purrr)
library(readr)
library(jsonlite)
library(stringr)
library(tibble)
library(nfl4th)
source("scripts/helper.R")

# ---- Config ----
# Seasons/args passthrough: do NOT interpret. Whatever is provided on CLI
# is forwarded directly to load_pbp(...). If nothing provided, call with no args.
cli_args <- commandArgs(trailingOnly = TRUE)

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
# Pass through exactly what the CLI provided.
# Supports:
#   Rscript ...                -> load_pbp()
#   Rscript ... 1999           -> load_pbp(1999)
#   Rscript ... 1999:2004      -> load_pbp(1999:2004)
#   Rscript ... 1999 2000 2001 -> load_pbp(c(1999,2000,2001))
#   Rscript ... TRUE           -> load_pbp(TRUE)
#   Rscript ... any_garbage    -> raw passthrough; load_pbp(any_garbage) will error there.
if (length(cli_args) == 0) {
  message("Loading play-by-play with NO season arg (full passthrough)...")
  pbp_all <- load_pbp()
} else {
  # Build an expression string: single token as-is; multiple tokens become c(token1,token2,...)
  arg_expr <- if (length(cli_args) == 1) {
    cli_args[[1]]
  } else {
    paste0("c(", paste(cli_args, collapse = ","), ")")
  }

  message("Loading play-by-play with arg text: ", arg_expr)

  parsed <- tryCatch(
    eval(parse(text = arg_expr)),
    error = function(e) structure(list(.raw = arg_expr, .err = conditionMessage(e)),
                                  class = "goforit_parse_error")
  )

  if (inherits(parsed, "goforit_parse_error")) {
    # Raw passthrough: let load_pbp(...) handle/throw the error.
    pbp_all <- load_pbp(arg_expr)
  } else {
    pbp_all <- load_pbp(parsed)
  }
}

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

# ---- Enrich once ----
enriched <- enrich_with_nfl4th(eagles_pbp) %>%
  add_frontend_columns("PHI")

# --- Meta uses original season for paths/index ---
meta <- enriched %>%
  dplyr::group_by(game_id) %>%
  dplyr::summarise(
    season = as.integer(substr(dplyr::first(game_id), 1, 4)),
    week   = dplyr::first(week),
    date   = as.character(dplyr::first(game_date)),
    home   = dplyr::first(home_team),
    away   = dplyr::first(away_team),
    final_home = last_non_na(total_home_score),
    final_away = last_non_na(total_away_score),
    .groups = "drop"
  )

gids <- meta$game_id

for (i in seq_along(gids)) {
  gid <- gids[[i]]
  g <- enriched %>% dplyr::filter(game_id == gid)

  if (nrow(g) == 0) next

  # pull meta for this game
  m <- meta %>% dplyr::filter(game_id == gid) %>% dplyr::slice(1)
  season <- m$season
  week   <- m$week
  date   <- m$date
  home   <- m$home
  away   <- m$away
  final_home <- m$final_home
  final_away <- m$final_away

  # Ensure season directory exists
  season_dir <- file.path(pbp_out_dir, as.character(season))
  dir.create(season_dir, recursive = TRUE, showWarnings = FALSE)

  # Output file path
  cal_year <- format(as.Date(date), "%Y")
  parts <- strsplit(gid, "_", fixed = TRUE)[[1]]
  parts[1] <- cal_year
  file_base <- paste(parts, collapse = "_")
  csv_gz_path <- file.path(season_dir, paste0(file_base, ".csv.gz"))

  # Write gzipped CSV (all plays for both teams, enriched with 4th-down model fields)
  message(sprintf("  • (%d/%d) %s", i, length(gids), gid))
  con <- gzfile(csv_gz_path, open = "wb")
  readr::write_csv(g, con)
  close(con)

  # Collect index row
  idx_list[[length(idx_list) + 1]] <- tibble::tibble(
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
          final = fin,
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