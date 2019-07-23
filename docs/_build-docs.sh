#!/usr/bin/env bash

SCRIPT_PATH="$(cd "$(dirname "$0")"; pwd -P)"
MAIN_TITLE="Parsing With Haskell Parser Combinators"
DESCRIPTION="Need to parse something? Never heard of a parser combinator? \
Looking to learn some Haskell? \
Awesome! This is everything you'll need to get up and parsing with Haskell parser combinators. \
From here you can try tackling esoteric data serialization formats, compiler front ends, domain specific languages—you name it!"
REPO_URL="https://github.com/lettier/parsing-with-haskell-parser-combinators"
AUTHOR="David Lettier"
CSS="style.css"

$PANDOC \
  -f gfm \
  -t html5 \
  --highlight-style=breezedark \
  --template=$SCRIPT_PATH/_template.html5 \
  $SCRIPT_PATH/../README.md \
  --metadata pagetitle="$MAIN_TITLE" \
  --metadata author-meta="$AUTHOR" \
  --metadata description="$DESCRIPTION" \
  --metadata css=$CSS \
  -o "$SCRIPT_PATH/index.html"
