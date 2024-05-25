#!/usr/bin/env bash

# This script builds the Atom feed for the static HTML blog.

# Exit on error
set -e

# Source files to build index for
FILES=$@

HEAD=$(cat $(dirname $0)/src/head.html)
UPDATED=$(date --iso-8601=seconds)

# Build the index
cat <<EOF
<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
 
 <id>https://wickstrom.tech/</id>
 <title>Oskar Wickström</title>
 <subtitle>Software design, testing, functional programming, and other delightful things.</subtitle>
 <link href="https://wickstrom.tech/feed.xml" rel="self"/>
 <link href="https://wickstrom.tech/"/>
EOF
echo " <updated>${UPDATED}</updated>"
echo " <author><name>Oskar Wickström</name></author>"
for f in $FILES; do
	if [[ "$f" != "index.html" ]]; then
		title=$(yq --front-matter=extract '.title' $f)
		date=$(yq --front-matter=extract '.date' $f)
		rfc_date=$(date -d "$date" --iso-8601=seconds)
		intro=$(pandoc -f markdown -t plain -i $f | awk -v RS= '/./ { print; exit }')
		target=$(echo $f | sed 's/src\/posts\///' | sed 's/\.md$/.html/')
		echo "<entry><id>https://wickstrom.tech/${target}</id><title>${title}</title><link href=\"${target}\"/><updated>${rfc_date}</updated><summary>${intro}</summary></entry>"
	fi
done
cat <<EOF
</feed>
EOF
