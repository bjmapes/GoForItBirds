import json
import re
from pathlib import Path

INDEX_JSON = Path("_data/games_index.json")
SEASONS_DIR = Path("seasons")
GAMES_DIR = Path("games")

def slugify(game_id: str) -> str:
    return re.sub(r"[^a-z0-9]+", "-", game_id.lower())

def clear_directory(directory: Path):
    directory.mkdir(parents=True, exist_ok=True)
    for f in directory.glob("*.md"):
        f.unlink()

def parse_final(final_val):
    """
    Accepts:
      - {"home": x, "away": y}
      - [{"home": x, "away": y}]
      - scalar / None (returns (None, None))
    """
    if isinstance(final_val, dict):
        return final_val.get("home"), final_val.get("away")
    if isinstance(final_val, list) and final_val and isinstance(final_val[0], dict):
        f0 = final_val[0]
        return f0.get("home"), f0.get("away")
    return None, None

def generate_pages():
    data = json.loads(INDEX_JSON.read_text())

    # Clear old .md files
    clear_directory(SEASONS_DIR)
    clear_directory(GAMES_DIR)

    # data is a list like: [{"season": 2023, "games": [...]}, ...]
    for season_block in data:
        season = season_block.get("season")
        games = season_block.get("games", [])

        # Season page
        (SEASONS_DIR / f"{season}.md").write_text(
            f"---\nlayout: season\ntitle: {season}\nseason: {season}\n---"
        )

        # Game pages
        for g in games:
            gid = g.get("game_id")
            week = g.get("week")
            date = g.get("date")
            home = g.get("home")
            away = g.get("away")
            pbp_url = g.get("pbp_url")

            final_home, final_away = parse_final(g.get("final"))

            title = f"Week {week} — {away} at {home}"
            slug = slugify(gid)
            (GAMES_DIR / f"{slug}.md").write_text(
                "---\n"
                f"layout: game\n"
                f"title: {title}\n"
                f"season: {season}\n"
                f"game_id: {gid}\n"
                f"week: {week}\n"
                f"date: {date}\n"
                f"home_team: {home}\n"
                f"away_team: {away}\n"
                f"final_home: {'' if final_home is None else final_home}\n"
                f"final_away: {'' if final_away is None else final_away}\n"
                f"pbp_url: {pbp_url}\n"
                "---"
            )

if __name__ == "__main__":
    SEASONS_DIR.mkdir(parents=True, exist_ok=True)
    GAMES_DIR.mkdir(parents=True, exist_ok=True)
    generate_pages()
    print("✅ Pages generated in /seasons and /games")