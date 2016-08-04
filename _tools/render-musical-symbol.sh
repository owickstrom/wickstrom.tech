#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

music_assets=$DIR/../assets/music
symbol=$1
name=$2

time_signature_hide=""

if [ -z $SHOW_TIME_SIGN ]; then
  time_signature_hide="\override Staff.TimeSignature #'stencil = ##f"
fi

mkdir -p $music_assets
read -r -d '' contents << EOF
\header{
  title = ""
  tagline = ""
}

\relative {
  $time_signature_hide
  \time 4/4
  \override Staff.Clef.color = #white
  \override Staff.Clef.layer = #-1
  \override Staff.TimeSignature.layer = #-1
  \stopStaff
  $symbol
}
EOF

lilypond_trim() {
  echo $1 | lilypond \
    --format=png \
    -dresolution=$2 \
    --output $3 \
    -
  gm convert $3.png -trim $3.png
  echo "Created and trimmed $3.png"
}

lilypond_trim "$contents" 100 $music_assets/$name.1x
lilypond_trim "$contents" 200 $music_assets/$name.2x
