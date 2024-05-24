#!/usr/bin/env bash

DIR=$(dirname $0)

# This script builds the index for the static HTML blog.

# Exit on error
set -e

# Source files to build index for
FILES=$@

HEAD=$(cat $(dirname $0)/src/head.html)

# Build the index
cat <<EOF
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>Oskar Wickström</title>
  <link rel="stylesheet" href="reset.css">
  <link rel="stylesheet" href="index.css">
  ${HEAD}
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
cat $DIR/src/post-nav-after.html
