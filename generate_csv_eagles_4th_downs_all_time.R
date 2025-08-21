# Load libraries
library(nflfastR)
library(dplyr)
library(readr)

# Define output path
output_path <- "eagles_4th_downs_all_time.csv"

# Download data for all seasons
seasons <- 1999:2023  # Adjust as needed
pbp <- purrr::map_df(seasons, load_pbp)

# Filter for Eagles 4th down plays
eagles_4th <- pbp %>%
  filter(posteam == "PHI", down == 4) %>%
  mutate(
    field_position = dplyr::case_when(
      yardline_100 == 50 ~ "50",
      side_of_field == posteam ~ paste0("Own ", 100 - yardline_100),
      TRUE ~ paste0("Opponent ", yardline_100)
    )
  )

# Write to CSV
write_csv(eagles_4th, output_path)

# Optional: Print confirmation
cat("Exported", nrow(eagles_4th), "plays to", output_path, "\n")