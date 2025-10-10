#!/usr/bin/env bash

set -e

make clean all
parallel -j4 <<EOF
  live-server --port=8080 target
  watchexec -w src -e css,gif,html,jpg,md,otf,pdf,png,rng,svg,webp,woff2 make html
EOF
