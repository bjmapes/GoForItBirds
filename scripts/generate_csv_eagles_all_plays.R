# scripts/generate_csv_eagles_all_plays.R

library(nflfastR)
library(dplyr)
library(purrr)
library(readr)
library(jsonlite)
library(stringr)
library(tibble)
library(nfl4th)
source("scripts/helper.R")

cli_args <- commandArgs(trailingOnly = TRUE)

pbp_out_dir <- "assets/data/pbp"
index_out_liquid <- "_data/games_index.json"
index_out_js     <- "assets/data/games_index.json"

dir.create(pbp_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(index_out_liquid), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(index_out_js), recursive = TRUE, showWarnings = FALSE)

last_non_na <- function(x) {
  y <- x[!is.na(x)]
  if (length(y)) y[length(y)] else NA
}

if (length(cli_args) == 0) {
  message("Loading play-by-play with NO season arg (full passthrough)...")
  pbp_all <- load_pbp() 
} else {
  arg_expr <- if (length(cli_args) == 1) cli_args[[1]]
              else paste0("c(", paste(cli_args, collapse = ","), ")")
  message("Loading play-by-play with arg text: ", arg_expr)

  parsed <- tryCatch(
    eval(parse(text = arg_expr)),
    error = function(e) structure(list(.raw = arg_expr, .err = conditionMessage(e)),
                                  class = "goforit_parse_error")
  )
  pbp_all <- if (inherits(parsed, "goforit_parse_error")) load_pbp(arg_expr) else load_pbp(parsed)
}

eagles_pbp <- pbp_all %>% filter(home_team == "PHI" | away_team == "PHI")

if (!"game_id" %in% names(eagles_pbp)) {
  stop("Column `game_id` missing in PBP. nflfastR schema changed?")
}

message("Writing per-game CSV.gz ...")

idx_list <- list()
gids <- unique(eagles_pbp$game_id)

enriched <- eagles_pbp %>%
  dplyr::mutate(
    season_orig = season,
    season = dplyr::if_else(season_orig == 2025L, 2024L, season_orig),
    actual_decision = dplyr::case_when(
      play_type == "punt" ~ "Punt",
      play_type == "field_goal" ~ "Field Goal",
      play_type %in% c("run", "pass") ~ "Go for it",
      TRUE ~ "Other"
    )
  ) %>%
  nfl4th::add_4th_probs() %>%
  dplyr::mutate(season = season_orig) %>%
  add_frontend_columns_pbp(team = "PHI")

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

  m <- meta %>% dplyr::filter(game_id == gid) %>% dplyr::slice(1)
  season <- m$season; week <- m$week; date <- m$date
  home <- m$home; away <- m$away
  final_home <- m$final_home; final_away <- m$final_away

  season_dir <- file.path(pbp_out_dir, as.character(season))
  dir.create(season_dir, recursive = TRUE, showWarnings = FALSE)

  cal_year <- format(as.Date(date), "%Y")
  parts <- strsplit(gid, "_", fixed = TRUE)[[1]]
  parts[1] <- cal_year
  file_base <- paste(parts, collapse = "_")
  csv_gz_path <- file.path(season_dir, paste0(file_base, ".csv.gz"))

  message(sprintf("  • (%d/%d) %s", i, length(gids), gid))
  con <- gzfile(csv_gz_path, open = "wb")
  readr::write_csv(g, con); close(con)

  idx_list[[length(idx_list) + 1]] <- tibble::tibble(
    season = season,
    game_id = gid,
    week = week,
    date = date,
    home = home,
    away = away,
    final = list(list(home = final_home, away = final_away)),
    pbp_url = paste0("/", csv_gz_path)
  )
}

if (length(idx_list) == 0) stop("No Eagles games found in the selected seasons.")

idx_rows <- bind_rows(idx_list) %>% arrange(season, week, game_id)

idx_by_season <- idx_rows %>%
  group_by(season) %>%
  reframe(
    games = list(purrr::pmap(
      list(game_id, week, date, home, away, final, pbp_url),
      \(gid, wk, dt, hm, aw, fin, url) {
        list(game_id = gid, week = wk, date = dt, home = hm, away = aw,
             final = fin, pbp_url = url)
      }
    ))
  ) %>%
  ungroup() %>%
  arrange(season)

index_list <- purrr::map2(
  idx_by_season$season, idx_by_season$games,
  ~list(season = .x, games = .y)
)

json_txt <- toJSON(index_list, auto_unbox = TRUE, pretty = TRUE)
writeLines(json_txt, index_out_liquid)
writeLines(json_txt, index_out_js)

message("Done.")