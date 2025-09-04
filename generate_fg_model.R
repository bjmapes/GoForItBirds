# generate_fg_model.R — empirical FG table via LOESS (no fastrmodels)

suppressPackageStartupMessages({
  library(nflfastR)
  library(dplyr)
  library(jsonlite)
})

cat("Working dir:", getwd(), "\n")

# 1) Load play-by-play (adjust seasons if needed for speed)
seasons <- 1999:2024
cat("Loading PBP for seasons:", paste(range(seasons), collapse = "-"), "\n")
pbp <- load_pbp(seasons)

# 2) Filter field goal attempts with distances in [10, 80]
fg <- pbp %>%
  filter(play_type == "field_goal",
         !is.na(kick_distance),
         kick_distance >= 10, kick_distance <= 80,
         !is.na(field_goal_result)) %>%
  transmute(kick_distance = as.numeric(kick_distance),
            made = as.integer(field_goal_result == "made"))

cat("FG rows:", nrow(fg), " range(dist):",
    paste(range(fg$kick_distance, na.rm = TRUE), collapse = "-"),
    " made%:", round(mean(fg$made) * 100, 1), "%\n")

if (nrow(fg) < 1000) {
  stop("Not enough FG attempts in dataset. Check seasons or data load.")
}

# 3) Fit LOESS on (distance -> make probability)
#    Span controls smoothing (0.25–0.4 typical). Degree=1 for robustness.
span_val <- 0.3
lo <- loess(made ~ kick_distance, data = fg, span = span_val, degree = 1)

# 4) Predict for integer distances 10..80 and clamp to [0,1]
distances <- seq(10, 80, by = 1)
p <- predict(lo, newdata = data.frame(kick_distance = distances))
p[!is.finite(p)] <- NA
# Fill any NA by simple nearest-neighbor then clamp
for (i in seq_along(p)) {
  if (is.na(p[i])) {
    # nearest non-NA index
    left <- max(which(!is.na(p[seq_len(i)])), na.rm = TRUE)
    right <- i + which(!is.na(p[-seq_len(i)]))[1]
    cand <- c(ifelse(is.finite(left), p[left], NA), ifelse(is.finite(right), p[right], NA))
    p[i] <- mean(cand, na.rm = TRUE)
  }
}
p <- pmin(pmax(p, 0), 1)

fg_table <- data.frame(distance = distances, prob = p)
cat("Preview:\n"); print(head(fg_table)); print(tail(fg_table))

# 5) Write JSON to assets/data/fg_table.json
out_dir <- file.path(getwd(), "assets", "data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_path <- file.path(out_dir, "fg_table.json")
cat("Writing:", out_path, "\n")
write_json(fg_table, out_path, pretty = TRUE, auto_unbox = TRUE)

if (file.exists(out_path)) {
  cat("Wrote file:", out_path, " (", file.info(out_path)$size, " bytes)\n", sep = "")
} else {
  stop("FAILED to write ", out_path)
}