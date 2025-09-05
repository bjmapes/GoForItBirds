# Load required libraries
library(nflfastR)
library(dplyr)
library(nfl4th)

# Define output path
output_path <- "_data/eagles_4th_downs_all_time.csv"
output_path2 <- "assets/data/eagles_4th_downs_all_time.csv"


# Load play-by-play data for all seasons
# Load play-by-play data for all seasons
latest <- nflfastR::most_recent_season()
pbp_all <- load_pbp(1999:latest)

# Filter for Eagles 4th down plays
eagles_4th_all <- pbp_all %>%
  filter(posteam == "PHI", down == 4)

# Add actual decision label
eagles_4th_all <- eagles_4th_all %>%
  mutate(actual_decision = case_when(
    play_type == "punt" ~ "Punt",
    play_type == "field_goal" ~ "Field Goal",
    play_type %in% c("run", "pass") ~ "Go for it",
    TRUE ~ "Other"
  ))

# Add model probabilities
eagles_4th_all <- add_4th_probs(eagles_4th_all)

# Add model recommendation based on win probabilities
eagles_4th_all <- eagles_4th_all %>%
  mutate(model_recommendation = case_when(
    go_wp > fg_wp & go_wp > punt_wp ~ "Go for it",
    fg_wp > punt_wp ~ "Field Goal",
    TRUE ~ "Punt"
  ))

# Write to CSV
write.csv(eagles_4th_all, output_path, row.names = FALSE)

# Optional confirmation message
cat("Exported", nrow(eagles_4th_all), "plays to", output_path, "\n")


# Write to CSV
write.csv(eagles_4th_all, output_path2, row.names = FALSE)

# Optional confirmation message
cat("Exported", nrow(eagles_4th_all), "plays to", output_path2, "\n")
