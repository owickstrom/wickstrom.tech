#!/usr/bin/env bash

set -e

make -j$(nproc) clean all
parallel -j4 <<EOF
  python3 -m http.server --directory target
  fswatch -o . --exclude target | xargs -n1 -I{} make -j$(nproc) all
EOF