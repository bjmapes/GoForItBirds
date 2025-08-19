import pandas as pd
import re
import os
import shutil
from pathlib import Path

# Paths
CSV_PATH = Path("../data/eagles_4th_downs_all_years.csv").resolve()
SEASONS_DIR = Path("seasons")
GAMES_DIR = Path("games")

def slugify(game_id):
    return re.sub(r"[^a-z0-9]+", "-", game_id.lower())

def clear_directory(directory):
    for file in directory.glob("*.md"):
        file.unlink()

def generate_pages():
    df = pd.read_csv(CSV_PATH)
    # Basic validation: drop rows with NaN in critical columns
    df = df.dropna(subset=["game_id", "season", "week"])

    # Clear old .md files before regenerating
    clear_directory(SEASONS_DIR)
    clear_directory(GAMES_DIR)

    # Create season pages
    for season in sorted(df["season"].unique()):
        season_path = SEASONS_DIR / f"{season}.md"
        content = f"""---
layout: season
title: {season} Eagles 4th Downs
season: {season}
---"""
        season_path.write_text(content)

    # Create game pages
    grouped = df.groupby(["season", "week", "game_id"])
    for (season, week, game_id), _ in grouped:
        slug = slugify(game_id)
        game_path = GAMES_DIR / f"{slug}.md"
        title = f"Week {week} – Game {game_id}"
        content = f"""---
layout: game
title: {title}
season: {season}
game_id: {game_id}
---"""
        game_path.write_text(content)

if __name__ == "__main__":
    SEASONS_DIR.mkdir(exist_ok=True, parents=True)
    GAMES_DIR.mkdir(exist_ok=True, parents=True)
    generate_pages()
    print("✅ Pages generated in /seasons and /games")