#!/bin/bash
set -e

echo "🧹 Cleaning site..."
bundle exec $(bundle show jekyll)/exe/jekyll clean

echo "📄 Generating pages..."
./.venv/bin/python generate_pages.py

echo "🔨 Building site..."
bundle exec $(bundle show jekyll)/exe/jekyll build

echo "✅ Done. Site built in _site/"