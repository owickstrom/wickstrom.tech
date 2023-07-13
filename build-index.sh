#!/usr/bin/env bash

# This script builds the index for the static HTML blog.

# Exit on error
set -e

# Source files to build index for
FILES=$@

# Build the index
cat <<EOF
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>Oskar Wickström</title>
  <link rel="stylesheet" href="reset.css">
  <link rel="stylesheet" href="index.css">
  <link rel="icon" type="image/png" href="/assets/icon.png">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes">
</head>
<body class="index-page">
  <header>
    <h1>Oskar Wickström</h1>
    <p>Software design, testing, functional programming, and other delightful things.</p>
  </header>
  <h2>Posts</h2>
  <ul class="index">
EOF
for f in $FILES; do
  if [[ "$f" != "index.html" ]]; then
    title=$(yq --front-matter=extract '.title' $f)
    date=$(yq --front-matter=extract '.date' $f)
    target=$(echo $f | sed 's/src\/posts\///' | sed 's/\.md$/.html/')
    echo "    <li><a href=\"$target\"><span class=\"title\">$title</span></a><time>$date</time></li>"
  fi
done
cat <<EOF
  </ul>
</body>
</html>
EOF
